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
// Start Time: August, 2018
// Authoremail: gmhwxiATgmailDOTcom
//
(* ****** ****** *)


#include "./share.sats"

//
#staload
LOC = "./location.sats"
//
  typedef loc_t = $LOC.loc_t
//
(* ****** ****** *)
//
datatype
assoc =
ASSOCnon | ASSOClft | ASSOCrgt
//
fun
print_assoc(assoc): void
(*
fun
prerr_assoc(assoc): void
fun
fprint_assoc: fprint_type(assoc)
*)
//
#symload print with print_assoc
(*
#symload prerr with prerr_assoc
#symload fprint with fprint_assoc
*)
//
(* ****** ****** *)
//
// HX: precedence value
//
abstflt prcdv_tflat = int
//
  typedef prcdv = prcdv_tflat
//
(* ****** ****** *)
//
fun
prcdv2int: prcdv -> int
and
int2prcdv: int -> prcdv
//
fun
print_prcdv(prcdv): void
fun
prerr_prcdv(prcdv): void
fun
fprint_prcdv: fprint_type(prcdv)
//
#symload print with print_prcdv
#symload prerr with prerr_prcdv
#symload fprint with fprint_prcdv
//
(* ****** ****** *)
//
fun
add_prcdv_int(prcdv, int): prcdv
and
sub_prcdv_int(prcdv, int): prcdv
//
#symload + with add_prcdv_int
#symload - with sub_prcdv_int
//
fun
compare_prcdv_prcdv: (prcdv, prcdv) -> int
//
#symload compare with compare_prcdv_prcdv
//
(* ****** ****** *)
//
val
the_neginf_prcdv: prcdv // lowest precedence value
and
the_posinf_prcdv: prcdv // highest precedence value
//
(* ****** ****** *)
//
val app_prcdv : prcdv
//
val select_prcdv : prcdv
//
val exists_prcdv : prcdv
and forall_prcdv : prcdv
//
(* ****** ****** *)
//
val backslash_prcdv : prcdv
val infixtemp_prcdv : prcdv // for temp infix status
//
(* ****** ****** *)
//
datatype
fixty =
| FIXTYnon
| FIXTYpre of prcdv
| FIXTYpos of prcdv
| FIXTYinf of (prcdv, assoc)
| FIXTYpreinf of (prcdv, prcdv, assoc)
(*
| FIXTYposinf of (prcdv, prcdv, assoc)
*)
// end of [fixty]
//
(* ****** ****** *)
//
fun
print_fixty (fixty): void
(*
fun
prerr_fixty (fixty): void
fun
fprint_fixty: fprint_type(fixty)
*)
//
#symload print with print_fixty
(*
#symload prerr with prerr_fixty
#symload fprint with fprint_fixty
*)
//
(* ****** ****** *)
//
datatype
fxitm(a:tflt) =
| FXITMatm(a) of a
| FXITMopr(a) of (loc_t, fxopr(a))
//
and
fxopr(a:tflt) =
| FXOPRinf(a) of
  (prcdv, assoc, (a, a) -<cloref1> fxitm(a))
| FXOPRpre(a) of (prcdv, a -<cloref1> fxitm(a))
| FXOPRpos(a) of (prcdv, a -<cloref1> fxitm(a))
//
(* ****** ****** *)

(* end of [xats_fixity.sats] *)
