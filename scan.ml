open Lwt

type port_status =
  | Open
  | Closed
  | Timed_out
  | Error of Unix.error

type probe_result = { ipaddr : Unix.inet_addr; port : int; status : port_status }

let results, push_result = Lwt_stream.create ()

let addr_gen prefix port_min port_max =
  let open BatEnum in
  let enum = cartesian_product
      (from_while (Addr.network_gen prefix))
      (range port_min ~until:port_max) in
  fun () -> get enum

let addrs ~slots network port_min port_max =
  let generator = addr_gen network port_min port_max in
  Lwt_stream.from (fun () ->
      Semaphore.wait slots >>
      return (generator ()))

let try_connect ~timeout (ipaddr, port) =
  let fd = try
      Lwt_unix.(socket PF_INET SOCK_STREAM 0)
    with Unix.Unix_error (Unix.EMFILE, _, _) -> failwith "Too many open sockets" in
  try_lwt
    Lwt_io.printf "Trying %s:%d\n" (Unix.string_of_inet_addr ipaddr) port >>
    Lwt_unix.with_timeout timeout (fun () ->
        Lwt_unix.(connect fd (ADDR_INET (ipaddr, port)))) >>
    return Open
  with
  | Lwt_unix.Timeout -> return Timed_out
  | Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> return Closed
  | Unix.Unix_error (e, _, _) -> return (Error e)
  finally
    Lwt_unix.close fd

let probe ~slots ~timeout (ipaddr, port) =
  lwt status = try_connect ~timeout (ipaddr, port) in
  Semaphore.post slots >>
  return (push_result (Some { ipaddr = ipaddr; port = port; status = status }))

let string_of_status = function
  | Open -> "open"
  | Closed -> "closed"
  | Timed_out -> "timed out"
  | Error e -> Printf.sprintf "error: %s" (Unix.error_message e)

let string_of_result r =
  Printf.sprintf "%s:%d -> %s" (Unix.string_of_inet_addr r.ipaddr) r.port
    (string_of_status r.status)

let main network port_min port_max =
  let slots = Semaphore.create 1000 in
  join [
    (Lwt_stream.iter_p (probe ~slots ~timeout:1.)
       (addrs ~slots network port_min port_max) >>
     (push_result None; return_unit));
    Lwt_stream.iter_s (fun r -> Lwt_io.printf "Result: %s\n" (string_of_result r)) results;
  ]

let usage () =
  Printf.printf "Usage: %s {network} {port_min} {port_max}\n" Sys.argv.(0);
  exit 1

let () =
  if Array.length Sys.argv < 4 then
    usage ();

  let network = match Addr.network_of_string Sys.argv.(1) with
    | Some n -> n
    | None -> Printf.printf "Invalid network\n"; usage () in

  let port_min = int_of_string Sys.argv.(2) in
  let port_max = int_of_string Sys.argv.(3) in

  Lwt_main.run (main network port_min port_max)
