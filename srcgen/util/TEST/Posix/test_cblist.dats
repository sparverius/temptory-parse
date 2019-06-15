(* ****** ****** *)
(*
** HX-2018-05-16
** Unit testing. Kind of.
*)
(* ****** ****** *)
//
(*
#include
"/atspre_staload.hats"
*)
#include "share/HATS/temptory_staload_bucs320.hats"

(*
#staload "libats/SATS/stdio.sats"
#staload _ = "libats/DATS/stdio.dats"

#staload "libats/SATS/print.sats"
#staload _ = "libats/DATS/print.dats"
*)
//
(* ****** ****** *)
//
#staload
"./../../SATS/cblist.sats"
#staload
"./../../SATS/Posix/cblist.sats"
//
#staload
"./../../DATS/cblist.dats"
#staload
"./../../DATS/Posix/cblist.dats"
//
(* ****** ****** *)

local
//
#include "./../../DATS/cblist.dats"
#include "./../../DATS/Posix/cblist.dats"
//
in
  // nothing
end // end of [local]

(* ****** ****** *)
//
val () =
println!
("Hello from [test_cblist]!")
//
(* ****** ****** *)

val opt =
fpath_get_cblist
("./test_cblist.dats", i2sz(16))
val-~Some_vt(cbs) = opt

(* ****** ****** *)

val () = println! ("|cbs| = ", length(cbs))

(* ****** ****** *)

(*
val () = foreach(cbs) where
{
  val out = the_stdout<>()
  impltmp
  array_print$sep<>() = ()
(*
  extern castfn
  g0ofg1_arrszref{a:tflt}{n:int}(arrayref(a, n)): arrszref(a)
*)
(*
  impltmp
  cblist_foreach$fwork<>(n, cs) = arrszref_print<uchar>(g0ofg1_arrszref(cs))
*)
}
*)

(* ****** ****** *)

implement main0() = ((*void*))

(* ****** ****** *)

(* end of [test_cblist.dats] *)
