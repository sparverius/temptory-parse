(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS/Xanadu - Unleashing the Potential of Types!
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

(*
#staload "libats/SATS/gint.sats"
#staload _ = "libats/DATS/gint.dats"

#staload "libats/SATS/stdio.sats"
v#staload _ = "libats/DATS/stdio.dats"

#staload "libats/SATS/print.sats"
#staload _ = "libats/DATS/print.dats"
*)

#include "share/HATS/temptory_staload_bucs320.hats"

(* ****** ****** *)

#staload UN = "libats/SATS/unsafe.sats"

(* ****** ****** *)
//
#staload "./../SATS/label0.sats"
#staload _ = "./label0.dats"
#staload "./../SATS/lexing.sats"
#staload "./../SATS/staexp0.sats"
#staload _ = "./staexp0.dats"
//
(* ****** ****** *)

impltmp
{a}//tmp
optn1_print(t0) =
(
case+ t0 of
| optn1_none() =>
  (
   optn0_print$beg();
   optn0_print$end();
  )
| optn1_some(x0) =>
  (
   optn0_print$beg<>();
   print$val<a>(x0);
   optn0_print$end<>();
  )
)
//
impltmp
(a:tflt)
print$val<optn1(a)>(xs) = optn1_print<a>(xs)


impltmp
{a}(*tmp*)
list1_print(xs) =
(
list0_print$beg<>();
loop(0, xs);
list0_print$end<>();
) where
{
fun
loop
( i0: int
, xs: list1(a)): void =
(
case+ xs of
| list1_nil() => ()
| list1_cons(x0, xs) =>
  (
  if i0 > 0
    then list0_print$sep<>();
  // end of [if]
  print$val<a>(x0); loop(i0+1, xs)
  )
)
} (* end of [list0_print] *)

impltmp
(a:tflt)
print$val<list1(a)>(xs) = list1_print<a>(xs)


impltmp
(a:vtflt)
print$val<list1_vt(a)> = list1_vt_print<a>

impltmp
(a:vtflt)(*tmp*)
list1_vt_print<a>(xs) =
(
list0_print$beg<>();
loop(0, xs);
list0_print$end<>();
) where
{
fun
loop
( i0: int
, xs: !list1_vt(a)): void =
(
case+ xs of
| list1_vt_nil() => ()
| list1_vt_cons(x0, xs) =>
  (
  if
  (i0 > 0)
  then list1_print$sep<>();
  print$val<a>(x0); loop(i0+1, xs)
  )
)
}
(* ****** ****** *)
//
impltmp
print$val<token> x = print_token x
//
impltmp
print$val<t0int> x = print_t0int x
impltmp
print$val<t0chr> x = print_t0chr x
impltmp
print$val<t0flt> x = print_t0flt x
impltmp
print$val<t0str> x = print_t0str x
//
impltmp
print$val<i0dnt> x = print_i0dnt x
//
impltmp
print$val<l0abl> x = print_l0abl x
//
impltmp
print$val<s0ymb> x = print_s0ymb x
//
(* ****** ****** *)

impltmp
print$val<sort0> x = print_sort0 x

(* ****** ****** *)

impltmp
print$val<s0rtcon> x = print_s0rtcon x
impltmp
print$val<d0tsort> x = print_d0tsort x
impltmp
print$val<s0rtdef> x = print_s0rtdef x

(* ****** ****** *)
//
impltmp
print$val<s0arg> x = print_s0arg x
impltmp
print$val<s0marg> x = print_s0marg x
//
impltmp
print$val<t0arg> x = print_t0arg x
impltmp
print$val<t0marg> x = print_t0marg x
//
(* ****** ****** *)

impltmp
print$val<s0qua> x = print_s0qua x
impltmp
print$val<s0uni> x = print_s0uni x

impltmp
print$val<s0exp> x = print_s0exp x

(* ****** ****** *)

impltmp
print$val<d0atype> x = print_d0atype x
impltmp
print$val<d0atcon> x = print_d0atcon x

(* ****** ****** *)

impltmp
(a)//tmp
print$val<sl0abled(a)> x = print_sl0abled<a> x

(* ****** ****** *)

(*
implement
print_t0int(x0) =
print_t0int(stdout_ref, x0)
implement
prerr_t0int(x0) =
fprint_t0int(stderr_ref, x0)
*)

implement
print_t0int(x0) =
(
case+ x0.node() of
| T0INTnone(tok) =>
  print!("T0INTnone(", tok, ")")
| T0INTsome(tok) =>
  print!("T0INTsome(", tok, ")")
)

(* ****** ****** *)

(*
implement
print_t0chr(x0) =
fprint_t0chr(stdout_ref, x0)
implement
prerr_t0chr(x0) =
fprint_t0chr(stderr_ref, x0)
*)

implement
print_t0chr(x0) =
(
case+ x0.node() of
| T0CHRnone(tok) =>
  print!("T0CHRnone(", tok, ")")
| T0CHRsome(tok) =>
  print!("T0CHRsome(", tok, ")")
)

(* ****** ****** *)

(*
implement
print_t0flt(x0) =
fprint_t0flt(stdout_ref, x0)
implement
prerr_t0flt(x0) =
fprint_t0flt(stderr_ref, x0)
*)

implement
print_t0flt(x0) =
(
case+ x0.node() of
| T0FLTnone(tok) =>
  print!("T0FLTnone(", tok, ")")
| T0FLTsome(tok) =>
  print!("T0FLTsome(", tok, ")")
)

(* ****** ****** *)

(*
implement
print_t0str(x0) =
fprint_t0str(stdout_ref, x0)
implement
prerr_t0str(x0) =
fprint_t0str(stderr_ref, x0)
*)

implement
print_t0str(x0) =
(
case+ x0.node() of
| T0STRnone(tok) =>
  print!("T0STRnone(", tok, ")")
| T0STRsome(tok) =>
  print!("T0STRsome(", tok, ")")
)

(* ****** ****** *)

(*
implement
print_i0dnt(x0) =
fprint_i0dnt(stdout_ref, x0)
implement
prerr_i0dnt(x0) =
fprint_i0dnt(stderr_ref, x0)
*)

implement
print_i0dnt(x0) =
(
case+ x0.node() of
| I0DNTnone(tok) =>
  print!("I0DNTnone(", tok, ")")
| I0DNTsome(tok) =>
  print!("I0DNTsome(", tok, ")")
)

(* ****** ****** *)
//
(*
implement
print_l0abl(l0) =
fprint_l0abl(stdout_ref, l0)
implement
prerr_l0abl(l0) =
fprint_l0abl(stderr_ref, l0)
*)
//
implement
print_l0abl(l0) =
(
case+
l0.node() of
| L0ABsome(lab) =>
  print!("L0ABsome(", lab, ")")
| L0ABnone(tok) =>
  print!("L0ABnone(", tok, ")")
)
//
(* ****** ****** *)
//
(*
implement
print_s0ymb(x0) =
fprint_s0ymb(stdout_ref, x0)
implement
prerr_s0ymb(x0) =
fprint_s0ymb(stderr_ref, x0)
*)
//
implement
print_s0ymb(x0) =
(
case+
x0.node() of
//
| S0YMBi0dnt(id0) =>
  print!("S0YMBi0dnt(", id0, ")")
//
| S0YMBdtlab(dot1, lab2) =>
  print!("S0YMBdtlab(", dot1, "; ", lab2, ")")
| S0YMBbrack(tok1, tok2) =>
  print!("S0YMBbrack(", tok1, "; ", tok2, ")")
//
) (* end of [fprint_s0ymb] *)
//
(* ****** ****** *)

(*
implement
print_sq0eid(x0) =
fprint_sq0eid(stdout_ref, x0)
implement
prerr_sq0eid(x0) =
fprint_sq0eid(stderr_ref, x0)
*)

implement
print_sq0eid(x0) =
(
case+ x0 of
| SQ0EIDnone(sid) =>
  print!("SQ0EIDnone(", sid, ")")
| SQ0EIDsome(tok, sid) =>
  print!("SQ0EIDsome(", tok, "; ", sid, ")")
)

(* ****** ****** *)

(*
implement
print_dq0eid(x0) =
fprint_dq0eid(stdout_ref, x0)
implement
prerr_dq0eid(x0) =
fprint_dq0eid(stderr_ref, x0)
*)

implement
print_dq0eid(x0) =
(
case+ x0 of
| DQ0EIDnone(sid) =>
  print!("DQ0EIDnone(", sid, ")")
| DQ0EIDsome(tok, sid) =>
  print!("DQ0EIDsome(", tok, "; ", sid, ")")
)

(* ****** ****** *)

(*
implement
print_sort0(x0) =
fprint_sort0(stdout_ref, x0)
implement
prerr_sort0(x0) =
fprint_sort0(stderr_ref, x0)
*)

local

impltmp
print$val<sort0> x = print_sort0 x

in (* in-of-local *)

implement
print_sort0(x0) =
(
case+ x0.node() of
//
| S0Tid(tid) =>
  print!("S0Tid(", tid, ")")
//
| S0Tint(int) =>
  print!("S0Tint(", int, ")")
//
| S0Tapps(s0ts) =>
  print!("S0Tapps(", s0ts, ")")
//
| S0Tlist(t0, s0ts, t1) =>
  print!("S0Tlist(", t0, "; ", s0ts, "; ", t1, ")")
//
| S0Tqual(q0, s0t) =>
  print!("S0Tqid(", q0, ", ", s0t, ")")
//
| S0Tnone(tok) =>
    print!("S0Tnone(", tok, ")")
  // end of [S0Tnone]
//
) (* end of [fprint_sort0] *)

end // end of [local]

(* ****** ****** *)

(*
implement
print_s0rtcon(x0) =
fprint_s0rtcon(stdout_ref, x0)
implement
prerr_s0rtcon(x0) =
fprint_s0rtcon(stderr_ref, x0)
*)

implement
print_s0rtcon(x0) =
(
case+ x0.node() of
| S0RTCON(sid, opt) =>
  print!("S0RTCON(", sid, ", ", opt, ")")
) (* end of [fprint_s0rtcon] *)

(* ****** ****** *)

(*
implement
print_d0tsort(x0) =
fprint_d0tsort(stdout_ref, x0)
implement
prerr_d0tsort(x0) =
fprint_d0tsort(stderr_ref, x0)
*)
implement
print_d0tsort(x0) =
(
case+ x0.node() of
| D0TSORT(tid, tok, s0cs) =>
  print!("D0TSORT(", tid, "; ", tok, "; ", s0cs, ")")
) (* end of [fprint_d0tsort] *)

(* ****** ****** *)

(*
implement
print_s0rtdef(x0) =
fprint_s0rtdef(stdout_ref, x0)
implement
prerr_s0rtdef(x0) =
fprint_s0rtdef(stderr_ref, x0)
*)
implement
print_s0rtdef(x0) =
(
case+ x0.node() of
| S0RTDEFsort(s0t) =>
  print!("S0RTDEFsort(", s0t, ")")
| S0RTDEFsbst(tbeg, s0a0, tbar, s0es, tend) =>
  print!
  ("S0RTDEFsbst("
  , tbeg, "; ", s0a0, "; ", tbar, "; ", s0es, "; ", tend, ")")
) (* end of [fprint_s0rtdef] *)

(* ****** ****** *)

(*
implement
print_s0arg(x0) =
fprint_s0arg(stdout_ref, x0)
implement
prerr_s0arg(x0) =
fprint_s0arg(stderr_ref, x0)
*)

(*
implement
print_s0marg(x0) =
fprint_s0marg(stdout_ref, x0)
implement
prerr_s0marg(x0) =
fprint_s0marg(stderr_ref, x0)
*)

implement
print_s0arg(x0) =
(
case+
x0.node() of
| S0ARGnone(tok) =>
  print!("S0ARGnone(", tok, ")")
| S0ARGsome(sid, opt) =>
  print!("S0ARGsome(", sid, ", ", opt, ")")
) (* fprint_s0arg *)

implement
print_s0marg(x0) =
(
case+
x0.node() of
| S0MARGnone(tok) =>
  print!("S0MARGnone(", tok, ")")
| S0MARGsing(tid) =>
  print!("S0MARGsing(", tid, ")")
| S0MARGlist(tbeg, s0as, tend) =>
  print!("S0MARGlist(", tbeg, "; ", s0as, "; ", tend, ")")
) (* fprint_s0marg *)

(* ****** ****** *)

(*
implement
print_t0arg(x0) =
fprint_t0arg(stdout_ref, x0)
implement
prerr_t0arg(x0) =
fprint_t0arg(stderr_ref, x0)
*)

(*
implement
print_t0marg(x0) =
fprint_t0marg(stdout_ref, x0)
implement
prerr_t0marg(x0) =
fprint_t0marg(stderr_ref, x0)
*)

implement
print_t0arg(x0) =
(
case+
x0.node() of
| T0ARGsome(s0t, opt) =>
  print!("T0ARGsome(", s0t, ", ", opt, ")")
) (* fprint_t0arg *)

implement
print_t0marg(x0) =
(
case+
x0.node() of
| T0MARGnone(tok) =>
  print!("T0MARGnone(", tok, ")")
| T0MARGlist(tbeg, t0as, tend) =>
  print!("T0MARGlist(", tbeg, ", ", t0as, ", ", tend, ")")
) (* fprint_t0marg *)

(* ****** ****** *)

(*
implement
print_s0qua(x0) =
fprint_s0qua(stdout_ref, x0)
implement
prerr_s0qua(x0) =
fprint_s0qua(stderr_ref, x0)
*)

implement
print_s0qua(x0) =
(
case+ x0.node() of
| S0QUAprop(s0e) =>
  print!("S0QUAprop(", s0e, ")")
| S0QUAvars(ids, opt) =>
  print!("S0QUAvars(", ids, "; ", opt, ")")
)

(* ****** ****** *)

(*
implement
print_s0uni(x0) =
fprint_s0uni(stdout_ref, x0)
implement
prerr_s0uni(x0) =
fprint_s0uni(stderr_ref, x0)
*)

implement
print_s0uni(x0) =
(
case+ x0.node() of
| S0UNInone(tok) =>
  print!("S0UNInone(", tok, ")")
| S0UNIsome(tbeg, s0qs, tend) =>
  print!("S0UNIsome(", tbeg, "; ", s0qs, "; ", tend, ")")
)

(* ****** ****** *)

impltmp
{a}(*tmp*)
print_sl0abled(x0) = let
//
val+SL0ABLED(l0, t0, x1) = x0
//
in
  print!("SL0ABLED(");
  print!(l0, ", ", t0, ", ");
  print$val<a>(x1); print!(")")
end // end of [fprint_sl0abled]

(* ****** ****** *)

(*
implement
print_s0exp(x0) =
fprint_s0exp(stdout_ref, x0)
implement
prerr_s0exp(x0) =
fprint_s0exp(stderr_ref, x0)
*)

local

impltmp
print$val<s0exp> x = print_s0exp x

in (* in-of-local *)

implement
print_s0exp(x0) =
(
case+ x0.node() of
//
| S0Eid(sid) =>
  print!("S0Eid(", sid, ")")
//
| S0Eop1(opid) =>
  print!("S0Eop1(", opid, ")")
| S0Eop2(tbeg, opid, tend) =>
  print!("S0Eop2(", tbeg, "; ", opid, "; ", tend, ")")
//
| S0Eint(i0) =>
  print!("S0Eint(", i0, ")")
| S0Echr(c0) =>
  print!("S0Echr(", c0, ")")
| S0Eflt(f0) =>
  print!("S0Eflt(", f0, ")")
| S0Estr(s0) =>
  print!("S0Estr(", s0, ")")
//
| S0Eapps(s0es) =>
  print!("S0Eapps(", s0es, ")")
//
| S0Eimp(tbeg, s0es, tend) =>
  print!("S0Eimp(", tbeg, "; ", s0es, "; ", tend, ")")
//
| S0Eparen
  (tbeg, s0es, tend) =>
  print!("S0Eparen(", tbeg, "; ", s0es, "; ", tend, ")")
//
| S0Eforall(tbeg, s0qs, tend) =>
  print!("S0Eforall(", tbeg, "; ", s0qs, "; ", tend, ")")
| S0Eexists(tbeg, s0qs, tend) =>
  print!("S0Eexists(", tbeg, "; ", s0qs, "; ", tend, ")")
//
| S0Etuple
  (tbeg, topt, s0es, tend) =>
  print!("S0Etuple("
  , tbeg, "; ", topt, "; ", s0es, "; ", tend, ")")
| S0Erecord
  (tbeg, topt, s0es, tend) =>
  print!("S0Erecord("
  , tbeg, "; ", topt, "; ", s0es, "; ", tend, ")")
//
| S0Elam
  ( tbeg
  , arg0, res1
  , tok1, s0e0, tend) =>
  print!("S0Elam("
  , tbeg, ";", arg0, ";", res1, "; "
  , tok1, "; ", s0e0, "; ", tend, ")")
//
| S0Eanno
  (s0e, ann) =>
  print!("S0Eanno(", s0e, "; ", ann, ")")
//
| S0Equal
  (tok, s0e) =>
  print!("S0Equal(", tok, "; ", s0e, ")")
//
| S0Enone(token) => print!("S0Enone(", token, ")")
//
) (* end of [fprint_s0exp] *)

end // end of [local]

(* ****** ****** *)

(*
implement
print_s0exp_RPAREN(x0) =
fprint_s0exp_RPAREN(stdout_ref, x0)
implement
prerr_s0exp_RPAREN(x0) =
fprint_s0exp_RPAREN(stderr_ref, x0)
*)

local

impltmp
print$val<s0exp> x = print_s0exp x

in (* in-of-local *)

implement
print_s0exp_RPAREN(x0) =
(
case+ x0 of
| s0exp_RPAREN_cons0(tok) =>
  print!("s0exp_RPAREN_cons0(", tok, ")")
| s0exp_RPAREN_cons1(tok1, s0es, tok2) =>
  print!("s0exp_RPAREN_cons1(", tok1, ", ", s0es, ", ", tok2, ")")
) (* end of [fprint_s0exp_RPAREN] *)

end // end of [local]

(* ****** ****** *)

(*
implement
print_labs0exp_RBRACE(x0) =
fprint_labs0exp_RBRACE(stdout_ref, x0)
implement
prerr_labs0exp_RBRACE(x0) =
fprint_labs0exp_RBRACE(stderr_ref, x0)
*)

local

impltmp
print$val<s0exp> x = print_s0exp x

in (* in-of-local *)

implement
print_labs0exp_RBRACE(x0) =
(
case+ x0 of
| labs0exp_RBRACE_cons0(tok) =>
  print!("labs0exp_RBRACE_cons0(", tok, ")")
| labs0exp_RBRACE_cons1(tok1, ls0es, tok2) =>
  print!("labs0exp_RBRACE_cons1(", tok1, ", ", ls0es, ", ", tok2, ")")
) (* end of [fprint_labs0exp_RBRACE] *)

end // end of [local]

(* ****** ****** *)

(*
implement
print_s0eff(x0) =
fprint_s0eff(stdout_ref, x0)
implement
prerr_s0eff(x0) =
fprint_s0eff(stderr_ref, x0)
implement
fprint_s0eff
  (out, x0) =
(
case+ x0 of
| S0EFFnone(tok) =>
  print!("S0EFFnone(", tok, ")")
| S0EFFsome
  (tbeg, s0es, tend) =>
  print!("S0EFFsome("
  , tbeg, "; ", s0es, "; ", tend, ")")
) (* end of [fprint_s0eff] *)
*)

(* ****** ****** *)

(*
implement
print_effs0expopt(x0) =
fprint_effs0expopt(stdout_ref, x0)
implement
prerr_effs0expopt(x0) =
fprint_effs0expopt(stderr_ref, x0)
*)
implement
print_effs0expopt(x0) =
(
case+ x0 of
| EFFS0EXPnone() =>
  print!("EFFS0EXPnone(", ")")
| EFFS0EXPsome(s0e) =>
  print!("EFFS0EXPsome(", s0e, ")")
(*
| EFFS0EXPsome(s0f, s0e) =>
  print!("EFFS0EXPsome(", s0f, "; ", s0e, ")")
*)
) (* end of [fprint_effs0expopt] *)

(* ****** ****** *)

(*
implement
print_d0atype(x0) =
fprint_d0atype(stdout_ref, x0)
implement
prerr_d0atype(x0) =
fprint_d0atype(stderr_ref, x0)
*)
implement
print_d0atype(x0) =
(
case+ x0.node() of
| D0ATYPE(tid, arg, res, teq, d0cs) =>
  print!("D0ATYPE("
  , tid, "; "
  , arg, "; ", res, "; ", teq, "; ", d0cs, ")")
) (* end of [fprint_d0atype] *)

(* ****** ****** *)

(*
implement
print_d0atcon(x0) =
fprint_d0atcon(stdout_ref, x0)
implement
prerr_d0atcon(x0) =
fprint_d0atcon(stderr_ref, x0)
*)
implement
print_d0atcon(x0) =
(
case+ x0.node() of
| D0ATCON(s0us, dcon, s0is, argopt) =>
  print!("D0ATCON("
  , s0us, "; ", dcon, "; ", s0is, "; ", argopt, ")")
) (* end of [fprint_d0atcon] *)

(* ****** ****** *)


(* end of [xats_staexp0_print.dats] *)
