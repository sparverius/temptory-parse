(* #include "share/HATS/temptory_staload_bucs320.hats" *)
(* #staload "libats/DATS/basics.dats" *)
(* #staload "libats/SATS/gint.sats" *)
(*
val n0 = length(cs)
*)
(*
val
[
n0:int
]
n0 = $UN.cast{Nat}(n0)
*)
val [n0:int] n0 = g1ofg0(10)
(*
vtypedef cs = array(char,n0)
val
(pf0,fpf|p0) =
$UN.ptr0_vtake{cs}(ptrof(cs))
*)
