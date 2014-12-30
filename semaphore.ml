open Lwt

type t = { mutable count : int; cond : unit Lwt_condition.t }

let create init_count =
  { count = init_count; cond = Lwt_condition.create () }

let wait s =
  (if s.count == 0 then Lwt_condition.wait s.cond else return_unit) >>
  (s.count <- s.count - 1; return_unit)

let post s =
  s.count <- s.count + 1;
  Lwt_condition.signal s.cond ();
  return_unit
