(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS/Postiats - Unleashing the Potential of Types!
** Copyright (C) 2018 Hongwei Xi, ATS Trustful Software, Inc.
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of  the GNU GENERAL PUBLIC LICENSE (GPL) as published by the
** Free Software Foundation; either version 3, or (at  your  option)  any
** later version.
**
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
**
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)
//
// Author: Hongwei Xi
// Start Time: June, 2018
// Authoremail: gmhwxiATgmailDOTcom
//
(* ****** ****** *)
//
(*
#staload "libats/SATS/stdio.sats"
#staload _ = "libats/DATS/stdio.dats"

#staload "libats/SATS/gint.sats"
#staload _ = "libats/DATS/gint.dats"

#staload "libats/SATS/string.sats"
#staload _ = "libats/DATS/string.dats"

#staload "libats/SATS/print.sats"
#staload _ = "libats/DATS/print.dats"
*)

#include "share/HATS/temptory_staload_bucs320.hats"

#staload
UN =
"libats/SATS/unsafe.sats"
//
(* ****** ****** *)
//
#staload "./../SATS/basics.sats"
//
(* ****** ****** *)

implement
print_valkind(vlk) =
(
//
case+ vlk of
| VLKval() => print("VLKval")
| VLKvalp() => print("VLKvalp")
| VLKvaln() => print("VLKvaln")
(*
| VLKmcval() => print("VLKprval")
*)
| VLKprval() => print("VLKprval")
//
) (* end of [fprint_valkind] *)

(* ****** ****** *)

implement
print_funkind(fnk) =
(
//
case+ fnk of
| FNKfn0() => print("FNKfn0")
| FNKfnx() => print("FNKfnx")
| FNKfn1() => print("FNKfn1")
| FNKfun() => print("FNKfun")
//
| FNKprfn0() => print("FNKprfn0")
| FNKprfn1() => print("FNKprfn1")
| FNKprfun() => print("FNKprfun")
| FNKpraxi() => print("FNKpraxi")
//
| FNKcastfn() => print("FNKcastfn")
//
) (* end of [fprint_funkind] *)

(* ****** ****** *)

implement
print_impkind(knd) =
(
case+ knd of
| IMPtmp() => print!("IMPtmp")
| IMPfun() => print!("IMPfun")
| IMPval() => print!("IMPval")
| IMPprf() => print!("IMPprf")
) (* end of [fprint_impkind] *)

(* ****** ****** *)

implement
FC2clo_ = FC2clo(0)
implement
FC2cloptr = FC2clo(1)
implement
FC2cloref = FC2clo(~1)

(* ****** ****** *)
//
(*
implement
print_funclo2(fc2) =
fprint_funclo2(stdout_ref, fc2)
implement
prerr_funclo2(fc2) =
fprint_funclo2(stderr_ref, fc2)
*)
//
implement
print_funclo2(fc2) =
(
case+ fc2 of
| FC2fun() =>
  print!("FC2fun()")
| FC2clo(knd) =>
  print!("FC2clo(", knd, ")")
)


local
//
#staload
"libats/SATS/string.sats"
#staload
_ = "libats/DATS/string.dats"
//
in (* in-of-local *)

implement
xats_string_append
  (xs, ys) = let
  val xs = g1ofg0(xs)
  and ys = g1ofg0(ys)
in
  $effmask_all
  ((* strptr2string *)(string0_append<>(xs, ys)))
end // end of [xats_string_append]

end // end of [local]

(* ****** ****** *)


implement
show_valkind(vlk) =
(
//
case+ vlk of
| VLKval() => print("val")
| VLKvalp() => print("val+")
| VLKvaln() => print("val-")
(*
| VLKmcval() => print("VLKprval")
*)
| VLKprval() => print("prval")
//
) (* end of [fprint_valkind] *)

impltmp show$val<valkind>(x) = show_valkind(x)
//

implement
show_funkind(fnk) =
(
//
case+ fnk of
| FNKfn0() => print("fn")
| FNKfnx() => print("fnx")
| FNKfn1() => print("fun")
| FNKfun() => print("fun")

| FNKprfn0() => print("prfn0")
| FNKprfn1() => print("prfn1")
| FNKprfun() => print("prfun")
| FNKpraxi() => print("praxi")
//
| FNKcastfn() => print("castfn")
//
) (* end of [fprint_funkind] *)

impltmp show$val<funkind>(x) = show_funkind(x)

implement
show_impkind(knd) =
(
case+ knd of
| IMPtmp() => print!("impltmp")
| IMPfun() => print!("implfun")
| IMPval() => print!("implement")
| IMPprf() => print!("implprf")
) (* end of [fprint_impkind] *)

impltmp show$val<impkind>(x) = show_impkind(x)

impltmp show$sep<>() = ()
impltmp show$beg<>() = ()
impltmp show$end<>() = ()
//fprint$val<string>(the_stdout<>(), " ")

(* ****** ****** *)

(* end of [xats_basics.dats] *)
