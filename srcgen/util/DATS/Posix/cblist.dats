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
// Start Time: April, 2018
// Authoremail: gmhwxiATgmailDOTcom
//
(* ****** ****** *)
//

#staload "libats/SATS/gint.sats"
#staload _ = "libats/DATS/gint.dats"


#staload STDIO = "libats/SATS/stdio.sats"
#staload _ = "libats/DATS/stdio.dats"

#staload "libats/SATS/print.sats"
#staload _ = "libats/DATS/print.dats"

#staload "libats/SATS/optn.sats"
#staload _ = "libats/DATS/optn.dats"

#staload "libats/SATS/filebas.sats"
#staload _ = "libats/DATS/filebas.dats"

#staload "libats/SATS/array.sats"
#staload _ = "libats/DATS/array.dats"


#define None_vt optn1_vt_none
#define Some_vt optn1_vt_some

(* ****** ****** *)

#staload
UN = "libats/SATS/unsafe.sats"
//
(* ****** ****** *)

(*
#staload "libats/libc/SATS/stdio.sats"
*)

(* ****** ****** *)

#staload "./../../SATS/cblist.sats"
#staload "./../../SATS/Posix/cblist.sats"

(* ****** ****** *)

impltmp
{}(*tmp*)
fpath_get_cblist
  (path, bsz) = let
//
val
opt =
FILEref_open_opt
(path, "r"(* file_mode_r *))
//
in
  case+ opt of
  | ~optn0_vt_none() =>
     None_vt(*void*)
  | ~optn0_vt_some(inp) =>
     fileref_get_cblist(inp, bsz)
//
end // end of [fpath_get_cblist]

(* ****** ****** *)

impltmp
{}(*tmp*)
fpath_get_cblist_vt
  (path, bsz) = let
//
val
opt =
FILEref_open_opt(path, "r"(* file_mode_r *))
//
in
  case+ opt of
  | ~optn0_vt_none() =>
     None_vt(*void*)
  | ~optn0_vt_some(inp) =>
     fileref_get_cblist_vt(inp, bsz)
//
end // end of [fpath_get_cblist_vt]

(* ****** ****** *)
//
implement
fileref_get_cblist(inp, bsz) =
(
  $UN.castvwtp0
    (fileref_get_cblist_vt(inp, bsz))
  // $UN.castvwtp0
)
//
(* ****** ****** *)

implement
cblist_vt_free(cbs) =
(
case+ cbs of
| ~cblist_vt_nil() => ()
| ~cblist_vt_cons(n, A, cbs) =>
   (arrayptr_free(A); cblist_vt_free(cbs))
)

(* ****** ****** *)

implement
fileref_get_cblist_vt
  (inp, bsz) = let
//
fun
loop
(res: &ptr? >> cblist_vt): void =
(
if ($STDIO.feof(inp) != 0)
//FILEref_is_eof(inp)
then
(
  res := cblist_vt_nil()
)
else let
//
  val buf =
  arrayptr_make_none<uchar>(g1ofg0(bsz))
  val bufp = ptrof(buf)
  val nread =
  $extfcall(size, "fread", bufp, 1, bsz, inp)
//
in
  if
  (nread > 0)
  then let
    val ind = g1ofg0(nread)
    val () = assertloc(ind > 0)

    val ((*void*)) =
    res :=
    cblist_vt_cons(ind(* nread *), $UN.castvwtp0(buf), _)
    val+cblist_vt_cons(_, _, res2) = res
    val ((*void*)) = loop(res2)
    prval ((*folded*)) = fold@(res)
  in
    // nothing
  end // end of [then]
  else let
    val () =
    arrayptr_free(buf) in res := cblist_vt_nil()
  end // end of [else]
end // else
) (* end of [if] *)
//
in
  let var res: ptr in loop(res); Some_vt(res) end
end // end of [fileref_get_cblist_vt]

(* ****** ****** *)

(* end of [Posix_cblist.dats] *)
