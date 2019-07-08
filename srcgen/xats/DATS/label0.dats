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
// Start Time: July, 2018
// Authoremail: gmhwxiATgmailDOTcom
//
(* ****** ****** *)

(*
#staload "libats/SATS/stdio.sats"
#staload _ = "libats/DATS/stdio.dats"
#staload "libats/SATS/print.sats"
#staload _ = "libats/DATS/print.dats"
#staload "libats/SATS/gint.sats"
#staload _ = "libats/DATS/gint.dats"
*)
#include "share/HATS/temptory_staload_bucs320.hats"

(* ****** ****** *)

#staload "./../SATS/label0.sats"
#staload "./../SATS/symbol.sats"
#staload _ = "./symbol.dats"

(* ****** ****** *)
//
(*
implement
print_label(l0) =
  fprint_label(stdout_ref, l0)
//
implement
prerr_label(l0) =
  fprint_label(stderr_ref, l0)
*)
//
(* ****** ****** *)

local
//
datatype
label =
| LABint of int // digits
| LABsym of symbol // alnums
//
absimpl
label_tbox = label
//
in (* in-of-local *)

implement
print_label(l0) =
(
case+ l0 of
| LABint(i0) =>
  print!("LABint(", i0, ")")
| LABsym(s0) =>
  print!("LABsym(", s0, ")")
)

implement
show_label(l0) =
(
case+ l0 of
| LABint(i0) => print(i0)
  (* print!("LABint(", i0, ")") *)
| LABsym(s0) => print(s0)
  (* print!("LABsym(", s0, ")") *)
)


implement
label_nil = LABsym(symbol_nil)

implement
label_make_int(i0) = LABint(i0)
implement
label_make_sym(s0) = LABsym(s0)
implement
label_make_name(s0) = LABsym(symbol_make(s0))

end // end of [local]

(* ****** ****** *)

(* end of [xats_label0.dats] *)
