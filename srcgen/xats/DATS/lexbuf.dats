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
//
#staload "libats/SATS/stdio.sats"
#staload _ = "libats/DATS/stdio.dats"
#staload "libats/SATS/print.sats"
#staload _ = "libats/DATS/print.dats"
#staload "libats/SATS/gint.sats"
#staload _ = "libats/DATS/gint.dats"
#staload "libats/SATS/gptr.sats"
#staload _ = "libats/DATS/gptr.dats"

#staload "libats/SATS/string.sats"
#staload _ = "libats/DATS/string.dats"

#staload "libats/SATS/stropt.sats"
#staload _ = "libats/DATS/stropt.dats"

#staload "libats/SATS/array.sats"
#staload _ = "libats/DATS/array.dats"

(* ****** ****** *)

extern castfn
stropt0_unsome(opt: stropt):<> string0

(* ****** ****** *)

#staload
UN =
"libats/SATS/unsafe.sats"
//
(* ****** ****** *)
//
#staload
"./../SATS/location.sats"
//
(* ****** ****** *)
//
#staload "./../SATS/lexbuf.sats"
//
(* ****** ****** *)
//
#staload
"./../../util/SATS/cblist.sats"
//
(* ****** ****** *)
//
absimpl
lexbuf_tflat =
$extype_struct
"xats_lexbuf_struct" of
{
/*
  ntot= int
, nrow= int
, ncol= int
*/
/*
, char= int // ungetc
*/
, cbuf= stropt
, begp= ptr, endp= ptr, curp= ptr
, cbhead= arrayref(uchar,0), cbtail= cblist
} (* end of [lexbuf] *)
//
(* ****** ****** *)

#define NULL the_null_ptr

(* ****** ****** *)

implement
lexbuf_initize_cblist
  (buf, cbs) = let
//
(*
val () = buf.ntot := 0
val () = buf.nrow := 0
val () = buf.ncol := 0
val () = buf.nspc := 0
*)
//
val () = buf.cbuf := stropt0_none()
//
val () = buf.begp := NULL
val () = buf.endp := NULL
val () = buf.curp := NULL
//
val () = buf.cbtail := cbs//char-block-list
val () = buf.cbhead := $UN.cast(the_null_ptr)
//
in
  // nothing
end // end of [lexbuf_initize_cblist]

(* ****** ****** *)

(*
implement
lexbuf_get_ntot(buf) = buf.ntot
implement
lexbuf_get_nspc(buf) = buf.nspc
*)

(* ****** ****** *)

implement
lexbuf_get_none
  (buf) =
(
  buf.begp := buf.curp;
  buf.cbuf := stropt0_none()
)

(* ****** ****** *)

implement
lexbuf_get_fullseg
  (buf) = let
//
#define CNUL '\000'
//
val
cbf = buf.cbuf
val ((*void*)) =
buf.cbuf := stropt0_none()
//
in
//
(
if
stropt0_iseqz(cbf)
then let
  val A0 =
  (* arrayptr_make_none<char>(sz+1) *)
  arrayptr_make_none<char>(g1ofg0(sz+$UN.cast{usize}1))
//
  val p0 = ptrof(A0)
  val () =
  $extfcall(void, "memcpy", p0, bp, sz)
in
  $UN.castvwtp0(A0) where {
    val () = //$UN.ptr0_set_at<char>(p0, sz, CNUL)
    $UN.ptr0_set<char>(ptr0_add_size<char>(p0, sz), CNUL);
  }
end // end of [then]
else let
  val cs =
  stropt0_unsome(cbf)
  val n0 = length(cs)
//
(*
  val () = println!("cs = ", cs)
*)
//
  val num0 = $UN.cast{uint}(n0 + g1ofg0(sz))
  val num1 = g1ofg0(g0add_uint0_usize(num0, $UN.cast{usize}(1)))
  val A0 =
  arrayptr_make_none<char>(num1)//n0+sz+1)
//
  val p0 = ptrof(A0)
  val p1 = ptr0_add<char>(p0, n0)
  val () =
    $extfcall(void, "memcpy", p0, cs, n0)
  // end of [val]
  val () =
    $extfcall(void, "memcpy", p1, bp, sz)
  // end of [val]
//
in
  $UN.castvwtp0(A0) where {
    val () = //$UN.ptr0_set_at<char>(p1, sz, CNUL)
    $UN.ptr0_set<char>(ptr0_add_size<char>(p1, sz), CNUL);
  }
end // end of [else]
) where
{
  val bp = buf.begp
  val cp = buf.curp
  val () = buf.begp := cp
  //val sz = $UN.cast{size}(ptr0_diff<char>(cp, bp))
  val sz = $UN.cast{size}((g0sub_ptr_ptr(cp, bp)/sizeof<char>))
}
//
end // end of [lexbuf_get_fullseg]

(* ****** ****** *)

local

#define EOF %(~1)
#define CNUL '\000'

fun
cbf_update
(buf: &lexbuf >> _): void =
(
if
stropt0_iseqz(cbf)
then let
  val A0 =
  arrayptr_make_none<char>(g1ofg0(sz+$UN.cast{usize}1))//sz+1)
//
  val p0 = ptrof(A0)
  val () =
  $extfcall(void, "memcpy", p0, bp, sz)
in
  $UN.ptr0_set<char>(ptr0_add_size<char>(p0, sz), CNUL);
  (* $UN.ptr0_set_at<char>(p0, sz, CNUL); *)
  buf.cbuf := stropt0_some($UN.castvwtp0(A0))
end // end of [then]
else let
//
  val cs =
  stropt0_unsome(cbf)
  val n0 = length(cs)
//
(*
  val () = println!("cs = ", cs)
*)
//
  val num0 = $UN.cast{uint}(n0 + g1ofg0(sz))
  val num1 = g1ofg0(g0add_uint0_usize(num0, $UN.cast{usize}(1)))

  val A0 =
  arrayptr_make_none<char>(num1)//n0+sz+1)
//
  val p0 = ptrof(A0)
  val p1 = ptr0_add<char>(p0, n0)
  val () =
    $extfcall(void, "memcpy", p0, cs, n0)
  // end of [val]
  val () =
    $extfcall(void, "memcpy", p1, bp, sz)
  // end of [val]
//
in
  (* $UN.ptr0_set_at<char>(p1, sz, CNUL); *)
  $UN.ptr0_set<char>(ptr0_add_size<char>(p1, sz), CNUL);
  buf.cbuf := stropt0_some($UN.castvwtp0(A0))
end // end of [else]
) where
{
  val bp = buf.begp
  val ep = buf.endp
  (* val sz = $UN.cast{size}(ptr0_diff<char>(ep, bp)) *)
  val sz = $UN.cast{size}((g0sub_ptr_ptr(ep, bp)/sizeof<char>))
  val cbf = buf.cbuf
} (* end of [cbf_update] *)

in (* in-of-local *)

implement
lexbuf_getc
  (buf) = let
//
val cp = buf.curp
val ep = buf.endp
//
in
//
if
(cp < ep)
then let
  val uc =
  $UN.ptr0_get<uchar>(cp)
  val () =
  buf.curp := ptr0_succ<uchar>(cp)
in
  let val uc = (* uchar2int0 *)$UN.cast{sint}(uc) in uc end
end // end of [then]
else let
  val cbs = buf.cbtail
in
  case+ cbs of
  | cblist_nil
      ((*void*)) => EOF
  | cblist_cons
      (sz, A0, cbs) =>
    (
//
      cbf_update(buf);
//
      buf.begp := bp;
      buf.endp := ep;
      buf.curp := bp;
      buf.cbhead := A0;
      buf.cbtail := cbs;
//
      lexbuf_getc(buf)
//
    ) where
    {
      val bp = ptrof(A0)
      val ep = ptr0_add<char>(bp, sz)
      val A0 = $UN.cast(A0)
    } (* end of [cblist_cons] *)
end // end of [else]
//
end // end of [lexbuf_getc]

implement
lexbuf_unget
  (buf, i0) =
(
if
(
i0 >= 0
) then let
//
val bp = buf.begp
val cp = buf.curp
//
in
  if cp > bp
    then buf.curp := ptr0_pred<char>(cp)
    else ((*void*))
  // end of [if]
end // end of [let]
) (* end of [lexbuf_unget] *)

end // end of [local]

(* ****** ****** *)

(*
implement
lexbuf_get_pos
  (buf, pos) =
(
  pos.ntot(buf.ntot);
  pos.nrow(buf.nrow);
  pos.ncol(buf.ncol);
) where
{
  prval () = _initize_(pos) where
  {
    extern
    praxi _initize_(&pos_t? >> pos_t): void
  }
} (* end of [lexbuf_get_pos] *)
*)

(*
implement
lexbuf_set_pos
  (buf, pos) =
(
  buf.ntot := pos.ntot();
  buf.nrow := pos.nrow();
  buf.ncol := pos.ncol();
)
*)

(* ****** ****** *)
(*
implement
lexbufpos_get_loc
  (buf, cpos) = let
  var bpos: position
  val ((*void*)) =
    lexbuf_get_pos(buf, bpos)
  // end of [val]
in
  $LOC.location_make_pos_pos(bpos, cpos)
end // end of [lexbufpos_get_loc]
*)
(* ****** ****** *)

(* end of [xats_lexbuf.dats] *)
