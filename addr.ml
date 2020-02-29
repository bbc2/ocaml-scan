module V4 = struct
  let next i = Ipaddr.V4.of_int32 (Int32.add (Ipaddr.V4.to_int32 i) 1l)

  let init = Ipaddr.V4.Prefix.network
end

module V6 = struct
  let next i =
    let (i3, i2, i1, i0) = Ipaddr.V6.to_int32 i in
    let i' =
      if i0 == Int32.max_int then
        if i1 == Int32.max_int then
          if i2 == Int32.max_int then
            (Int32.add i3 1l, i2, i1, i0)
          else
            (i3, Int32.add i2 1l, i1, i0)
        else
          (i3, i2, Int32.add i1 1l, i0)
      else
        (i3, i2, i1, Int32.add i0 1l)
    in
    Ipaddr.V6.of_int32 i'

  let init n = next (Ipaddr.V6.Prefix.network n)
end

let next = function
  | Ipaddr.V4 i -> Ipaddr.V4 (V4.next i)
  | Ipaddr.V6 i -> Ipaddr.V6 (V6.next i)

let init = function
  | Ipaddr.V4 i -> Ipaddr.V4 (V4.init i)
  | Ipaddr.V6 i -> Ipaddr.V6 (V6.init i)

let is_in = Ipaddr.Prefix.mem

let network_of_string s =
  match Ipaddr.Prefix.of_string s with
  | Ok n -> Some n
  | Error _ -> (
    match Ipaddr.of_string s with
    | Ok i -> Some (Ipaddr.Prefix.of_addr i)
    | Error _ -> None )

let to_unix = Ipaddr_unix.to_inet_addr

let network_gen network =
  let ipaddr = ref (init network) in
  fun () ->
    let value = to_unix !ipaddr in
    if is_in !ipaddr network then (
      ipaddr := next !ipaddr;
      Some value
    ) else
      None
