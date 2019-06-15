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

#staload "libats/SATS/gint.sats"
#staload _ = "libats/DATS/gint.dats"

#staload "libats/SATS/list.sats"
#staload _ = "libats/DATS/list.dats"

#staload "libats/SATS/gptr.sats"
#staload _ = "libats/DATS/gptr.dats"

#staload "libats/SATS/array.sats"
#staload _ = "libats/DATS/array.dats"

(* ****** ****** *)

#staload
UN = "libats/SATS/unsafe.sats"
//
(* ****** ****** *)

#staload "./../SATS/lexing.sats"
#staload "./../SATS/parsing.sats"

(* ****** ****** *)
//
absimpl
tokbuf_tflat =
$extype_struct
"xats_tokbuf_struct" of
{
  begp= ptr, endp= ptr, curp= ptr
} (* end of [tokbuf] *)
//
(* ****** ****** *)

extern fun
{a:tflt}
arrayptr_make_list
{n:int}
( asz: int(n)
, xs0: list1(INV(a), n)):<!wrt> arrayptr(a, n)

extern castfn
arrayptr_encode :
  {a:vtflt}
  {l:addr}{n:int}
  (array_v(INV(a), l, n), mfree_gc_v(l) | ptr(l)) -<0> arrayptr(a, l, n)


extern prfun
lemma_list1_param
{a:tflt}{n:int}
(list1(INV(a), n)): [n >= 0] void

extern fun{a:tflt}
array_initize_list{n:int}
(
  A: &(@[a?][n]) >> @[a][n], asz: int n, xs: list1(INV(a), n)
) :<!wrt> void // end of [array_initize_list]


impltmp
{a}(*tmp*)
array_initize_list
  {n} (A, asz, xs) = let
//
typedef list00 = list1_0(a)
typedef list11 = list1_1(a)
//
fun loop
(
  p0: ptr, p1: ptr, xs: list00
) : void = let
//
(*
//
val () =
println!
  ("array_initize_list: loop")
//
*)
//
in
//
if (
p0 < p1
) then let
  val xs =
    $UN.cast{list11}(xs)
  // end of [val]
  val+list1_cons(x, xs) = xs
  val () =
    $UN.ptr0_set<a>(p0, x)
  // end of [val]
  val p0 = ptr0_succ<a>(p0)
in
  loop(p0, p1, xs)
end else () // end of [if]
//
end // end of [loop]
//
prval() = lemma_list1_param(xs)
//
val p0 = addr@(A)
val p1 = ptr0_add<a>(p0, asz)
val () = $effmask_all(loop(p0, p1, xs))
//
prval() =
__assert(A) where
{
//
extern
praxi
__assert (A: &array(a?, n) >> array(a, n)): void
//
} (* end of [prval] *)
//
in
  // nothing
end // end of [array_initize_list]



extern fun
{a:vtflt}
array_ptr_alloc
{n:int}(asz: size n) :<!wrt> [l:agz]
(array_v (a?, l, n), mfree_gc_v (l) | ptr l) (* end of [array_ptr_alloc] *)


extern fun
minit_gc(): void = "ext#ATS_MINIT"
//
(* ****** ****** *)

extern fun
mfree_gc
{l:addr}{n:int}
(pfat: bytes(n)@l, pfgc: mfree_gc_v(l) | ptr(l)) :<!wrt> void = "ext#ATS_MFREE"

extern fun
malloc_gc{n:int}
(bsz: size(n) ) :<!wrt> [l:agz]
(bytes(n)@l, mfree_gc_v(l) | ptr(l)) = "ext#ATS_MALLOC" // endfun

(*
#staload "libats/SATS/memory.sats"
*)

impltmp
{a}(*tmp*)
array_ptr_alloc
  {n}(asz) = let
//
val
[l:addr]
(
  pf, pfgc | p
) = malloc_gc (asz * sizeof<a>)
prval pf =
__assert(pf) where
{
extern praxi __assert
  (pf: bytes (n*sizeof(a)) @ l): array_v (a?, l, n)
// end of [__assert]
} // end of [where] // end of [prval]
//
in
  (pf, pfgc | p)
end // end of [array_ptr_alloc]


impltmp{a}(*tmp*)
arrayptr_make_list(asz, xs) = let
//
prval () = lemma_list1_param (xs)
//
val (
  pf, pfgc | p
) = array_ptr_alloc<a>(i2sz(asz))
//
val () = array_initize_list<a>(!p, asz, xs)
//
in
  arrayptr_encode(pf, pfgc | p)
end // end of [arrayptr_make_list]


(* ****** ****** *)

implement
tokbuf_initize_list(buf, xs) = let
//
  val n0 = length(xs)
//
val A0 = arrayptr_make_list<token>(n0, xs)
//
val p0 = $UN.castvwtp0{ptr}( A0 )
//
in
//
buf.begp := p0;
buf.curp := p0;
buf.endp :=
ptr0_add<token>(p0, n0);
//
end // end of [tokbuf_make_list]

(* ****** ****** *)
//
implement
tokbuf_getok0(buf) =
(
  $UN.ptr0_get<token>(buf.curp)
)
//
(*
HX-2018-06-24:
This function is only safe
if the current token is not EOF
*)
implement
tokbuf_incby1
  (buf) = let
//
  val p0 = buf.curp
//
in
  buf.curp := ptr0_succ<token>(p0)
end // end of [tokbuf_incby1]
//
(* ****** ****** *)

implement
tokbuf_getok1
  (buf) = let
//
  val p0 = buf.curp
//
  val tok =
  $UN.ptr0_get<token>(p0)
//
in
//
case+
tok.node() of
| T_EOF() => tok
| _ (* non-EOF *) => tok where
  {
    val () =
    buf.curp := ptr0_succ<token>(p0)
  }
//
end // end of [tokbuf_getok1]

(* ****** ****** *)

local

absimpl
tokbuf_mark_vtbox = ptr

in (* in-of-local *)

implement
tokbuf_get_mark(buf) = buf.curp
implement
tokbuf_set_mark(buf, mk0) = buf.curp := mk0
implement
tokbuf_clear_mark(buf, mk0) = () // discard

end // end of [local]

(* ****** ****** *)

(* end of [xats_parsing_tokbuf.dats] *)
