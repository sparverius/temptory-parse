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
// Start Time: May, 2018
// Authoremail: gmhwxiATgmailDOTcom
//
(* ****** ****** *)

#include "./share.sats"

(* ****** ****** *)

%{#
#include "CATS/location.cats"
%} // end of [%{#]

(* ****** ****** *)

abstbox location_type
typedef loc_t = location_type
typedef location = location_type

(* ****** ****** *)
//
(*
HX: atstflat = atst@ype
*)
abstflt
position_tflat =
$extype"xats_position_struct"
  typedef pos_t = position_tflat
  typedef position = position_tflat
//
(* ****** ****** *)
//
#staload
FIL = "./filepath.sats"
  typedef fil_t = $FIL.filepath
  typedef filepath = $FIL.filepath
//
(* ****** ****** *)
//
fun{}
position_initize
( pos0: &pos_t? >> _
, ntot: int, nrow: int, ncol: int): void
//
fun{}
position_copyfrom
  (pos0: &pos_t? >> _, pos1: &pos_t): void
//
(* ****** ****** *)
//
fun{}
position_get_ntot : (&pos_t) -> int
fun{}
position_get_nrow : (&pos_t) -> int
fun{}
position_get_ncol : (&pos_t) -> int
//
#symload .ntot with position_get_ntot
#symload .nrow with position_get_nrow
#symload .ncol with position_get_ncol
//
(* ****** ****** *)
//
fun{}
position_set_ntot
  (pos: &pos_t >> _, ntot: int): void
fun{}
position_set_nrow
  (pos: &pos_t >> _, nrow: int): void
fun{}
position_set_ncol
  (pos: &pos_t >> _, ncol: int): void
//
#symload .ntot with position_set_ntot
#symload .nrow with position_set_nrow
#symload .ncol with position_set_ncol
//
(* ****** ****** *)
//
fun
print_position(pos: position): void
fun
prerr_position(pos: position): void
fun
fprint_position(out: FILEref, pos: position): void
//
#symload print with print_position
#symload prerr with prerr_position
#symload fprint with fprint_position
//
(* ****** ****** *)
//
fun
position_incby_1
  (pos: &pos_t >> _): void
//
fun
position_incby_eol
  (pos: &pos_t >> _): void
//
fun
position_incby_char
  (pos: &pos_t >> _, uc: int): void
//
fun
position_incby_text
  (pos: &pos_t >> _, cs: string): void
fun
position_incby_neol
  (pos: &pos_t >> _, cs: string): void
//
(* ****** ****** *)
//
fun
location_filepath
  (loc: loc_t): fil_t
//
#symload
.filepath with location_filepath
//
fun
location_beg_ntot(loc: loc_t): int
fun
location_beg_nrow(loc: loc_t): int
fun
location_beg_ncol(loc: loc_t): int
//
fun
location_end_ntot(loc: loc_t): int
fun
location_end_nrow(loc: loc_t): int
fun
location_end_ncol(loc: loc_t): int
//
#symload .beg_ntot with location_beg_ntot
#symload .beg_nrow with location_beg_nrow
#symload .beg_ncol with location_beg_ncol
#symload .end_ntot with location_end_ntot
#symload .end_nrow with location_end_nrow
#symload .end_ncol with location_end_ncol
//
(* ****** ****** *)
//
fun
location_make_pos_pos
  (bpos: &pos_t, cpos: &pos_t): loc_t
fun
location_make_fil_pos_pos
  (fil: fil_t, bpos: &pos_t, cpos: &pos_t): loc_t
//
(* ****** ****** *)
//
fun
location_combine
  (loc1: loc_t, loc2: loc_t): loc_t
//
fun
location_leftmost(loc: loc_t): loc_t
fun
location_rightmost(loc: loc_t): loc_t
//
#symload + with location_combine
#symload leftmost with location_leftmost
#symload rightmost with location_rightmost
//
(* ****** ****** *)
//
fun
print_location(loc: location): void
(*
fun
prerr_location(loc: location): void
*)
fun
print_locrange(loc: location): void
(*
fun
fprint_location(out: FILEref, loc: location): void
*)
//
#symload print with print_location
(*
#symload prerr with prerr_location
#symload fprint with fprint_location
*)
//
(* ****** ****** *)

(* end of [xats_location.sats] *)
