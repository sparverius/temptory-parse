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
#include "./share.sats"

#define INFIX 0
#define INFIXL 1
#define INFIXR 2
//
#define PREFIX 3
#define POSTFIX 4
//
(* ****** ****** *)

#define BOXFLG (0x1 << 0)
#define LINFLG (0x1 << 1)
#define PRFFLG (0x1 << 2)

(* ****** ****** *)
//
(*
#define PROPSORT 0
#define VIEWSORT 0
//
#define TYPESORT 0
#define TBOXSORT 0
#define TFLATSORT 0
#define VTYPESORT 0
#define VTBOXSORT 0
#define VTFLATSORT 0
*)
//
(* ****** ****** *)

(* ****** ****** *)

(*
overload head with list_head
overload tail with list_tail
*)

(* ****** ****** *)


fun sortbox(x: int): int // 0,1
fun sortlin(x: int): int // 0,1
fun sortprf(x: int): int // 0,1
fun sortpol(x: int): int // -1,0,1

(* ****** ****** *)
//
#define PROPSORT 4 // 00100
//
#define VIEWSORT 6 // 00110
//
#define TYPESORT 0 // 00000
#define TBOXSORT 1 // 00001
#define TFLTSORT 0 // 00000
#define TFLATSORT 0 // 00000
//
#define VTYPESORT 2 // 00010
#define VTBOXSORT 3 // 00011
#define VTFLTSORT 2 // 00010
#define VTFLATSORT 2 // 00010
//
(* ****** ****** *)

#define POLPOS(x) (x + 0010)
#define POLNEG(x) (x + 0100)

(* ****** ****** *)

#define PROPSORT00 PROPSORT
#define PROPSORT01 %(POLPOS(PROPSORT))
#define PROPSORT10 %(POLNEG(PROPSORT))

#define VIEWSORT00 VIEWSORT
#define VIEWSORT01 %(POLPOS(VIEWSORT))
#define VIEWSORT10 %(POLNEG(VIEWSORT))

#define TYPESORT00 TYPESORT
#define TYPESORT01 %(POLPOS(TYPESORT))
#define TYPESORT10 %(POLNEG(TYPESORT))

#define TBOXSORT00 TBOXSORT
#define TBOXSORT01 %(POLPOS(TBOXSORT))
#define TBOXSORT10 %(POLNEG(TBOXSORT))

#define TFLTSORT00 TFLTSORT
#define TFLTSORT01 %(POLPOS(TFLTSORT))
#define TFLTSORT10 %(POLNEG(TFLTSORT))

#define TFLATSORT00 TFLATSORT
#define TFLATSORT01 %(POLPOS(TFLATSORT))
#define TFLATSORT10 %(POLNEG(TFLATSORT))

#define VTYPESORT00 VTYPESORT
#define VTYPESORT01 %(POLPOS(VTYPESORT))
#define VTYPESORT10 %(POLNEG(VTYPESORT))

#define VTBOXSORT00 VTBOXSORT
#define VTBOXSORT01 %(POLPOS(VTBOXSORT))
#define VTBOXSORT10 %(POLNEG(VTBOXSORT))

#define VTFLTSORT00 VTFLTSORT
#define VTFLTSORT01 %(POLPOS(VTFLTSORT))
#define VTFLTSORT10 %(POLNEG(VTFLTSORT))

#define VTFLATSORT00 VTFLATSORT
#define VTFLATSORT01 %(POLPOS(VTFLATSORT))
#define VTFLATSORT10 %(POLNEG(VTFLATSORT))

(* ****** ****** *)
//
fun sortpolpos(x: int): int
fun sortpolneg(x: int): int
//
(* ****** ****** *)
//
fun
subsort_int_int(int, int): bool
//
#symload subsort with subsort_int_int
//
(* ****** ****** *)

datatype
dctkind =
  | DCKfun of ()
  | DCKval of ()
  | DCKpraxi of ()
  | DCKprfun of ()
  | DCKprval of ()
  | DCKcastfn of ()
// end of [dcstkind]

(* ****** ****** *)
//
fun
print_dctkind: print_type(dctkind)
#symload print with print_dctkind
//
(* ****** ****** *)
//
datatype
valkind =
| VLKval // val
//
| VLKvalp // val+
| VLKvaln // val-
(*
| VLKmcval // mcval: for model-checking
*)
| VLKprval // prval: for theorem-proving
// end of [valkind]
//
fun
print_valkind: print_type(valkind)
#symload print with print_valkind
//
fun
show_valkind: print_type(valkind)
#symload show with show_valkind
//
(* ****** ****** *)

datatype
funkind =
//
| FNKfn0 // nonrec fun
| FNKfnx // tailrec fun
| FNKfn1 // recursive fun
| FNKfun // recursive fun
//
| FNKprfn0 // nonrec proof fun
| FNKprfn1 // recursive proof fun
| FNKprfun // recursive proof fun
//
| FNKpraxi // proof axiom
//
| FNKcastfn // casting fun
// end of [funkind]
//
fun
print_funkind: print_type(funkind)
#symload print with print_funkind
//
fun
show_funkind: print_type(funkind)
#symload show with show_funkind
//
(* ****** ****** *)
//
datatype
impkind =
//
| IMPtmp // template
| IMPfun // fun implementation
| IMPprf // proof implementation
| IMPval // value implementation
//
fun
print_impkind: print_type(impkind)
#symload
print with print_impkind
//
fun
show_impkind: print_type(impkind)
#symload show with show_impkind
//
(* ****** ****** *)
//
(*
HX: level-2 syntax
*)
//
#define CLOFLT (0) // flat
#define CLOPTR (1) // linear-boxed
#define CLOREF %(~1) // non-linear-boxed
//
// function/closure
datatype
funclo2 =
  | FC2fun of ((*fun*))
  | FC2clo of int(*knd*) // closure: knd=1/0/~1: ptr/clo/ref
//
val FC2clo_: funclo2 // flat
val FC2cloptr: funclo2 // linear
val FC2cloref: funclo2 // nonlin
//
fun
print_funclo2: print_type(funclo2)
(*
fun
prerr_funclo2: prerr_type(funclo2)
fun
fprint_funclo2: fprint_type(funclo2)
*)
//
#symload print with print_funclo2

fun show_funclo2: print_type(funclo2)
#symload show with show_funclo2
(*
overload prerr with prerr_funclo2
overload fprint with fprint_funclo2
*)

(* ****** ****** *)
//
typedef
eq_type(a:tflt) = (a, a) -> bool
typedef
neq_type(a:tflt) = (a, a) -> bool
//
typedef
compare_type(a:tflt) = (a, a) -> int
//
(* ****** ****** *)
//
fun
xats_string_append
  : (string, string) -<fun> string
//
#symload + with xats_string_append of 100
//
(* ****** ****** *)

fun
{a:vtflt}
show$val(x: !a): void

fun {} show$sep(): void
fun {} show$beg(): void
fun {} show$end(): void
fun {} show$before(): void
fun {} show$after(): void

fun {} template$sep(): void
fun {} template$beg(): void
fun {} template$end(): void
fun {} template$before_each(): void
fun {} template$after_each(): void

fun {} template$none(): void

fun {} template$arg$sep(): void

fun {} ti0arg$sep(): void


fun {} tq0arg$beg(): void
fun {} tq0arg$end(): void
fun {} tq0arg$sep(): void
fun {} tq0arg$none(): void

fun {} sq0arg$beg(): void
fun {} sq0arg$end(): void

fun {} sq0arg$none(): void
fun {} sq0arg$sep(): void

fun {} q0arg$colon$beg(): void
fun {} q0arg$colon$end(): void

fun {} staexp$sep(): void

// =>
fun {} eqgt$beg(): void
fun {} eqgt$end(): void

// ->
fun {} arrow$beg(): void
fun {} arrow$end(): void

fun {} smcln$beg(): void
fun {} smcln$end(): void


fun {} def$id$beg(): string
fun {} def$id$end(): string

fun {} impdecl$beg(): void
fun {} impdecl$end(): void

fun {} impdecl$eq$beg(): void
fun {} impdecl$eq$end(): void


fun {} eq$beg(): string
fun {} eq$end(): string

fun {} paren$beg(): void
fun {} paren$end(): void

fun {} case$beg(): void
fun {} case$end(): void
fun {} case$endall(): void
fun {} case$sep(): void

fun {} colon$sep(): void

fun {} comma$sep(): void
fun {} bar$sep(): void

fun {} let$beg(): void
fun {} let$end(): void

fun {} in$beg(): void
fun {} in$end(): void

fun {} end$beg(): void
fun {} end$end(): void

fun {} if$beg(): void
fun {} if$end(): void

fun {} then$beg(): void
fun {} then$end(): void

fun {} else$beg(): void
fun {} else$end(): void

fun {} ldelay$beg(): void
fun {} ldelay$end(): void

fun {} delay$beg(): void
fun {} delay$end(): void

(* end of [xats_basics.sats] *)
