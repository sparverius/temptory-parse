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
// Start Time: July, 2018
// Authoremail: gmhwxiATgmailDOTcom
//
(* ****** ****** *)
//
(*
#include
"share/atspre_staload.hats"
#staload
UN = "prelude/SATS/unsafe.sats"
*)
(* ****** ****** *)

#staload "libats/SATS/stdio.sats"
#staload _ = "libats/DATS/stdio.dats"
#staload "libats/SATS/print.sats"
#staload _ = "libats/DATS/print.dats"

#staload
UN =
"libats/SATS/unsafe.sats"

//
(* ****** ****** *)
//
#staload "./../SATS/lexing.sats"
//
#staload "./../SATS/staexp0.sats"
//
#staload "./../SATS/dynexp0.sats"
//
(* ****** ****** *)
//
#staload
_(*TMP*) = "./../DATS/staexp0_print.dats"
//
(* ****** ****** *)
//
impltmp
print$val<i0dnt> x = print_i0dnt x

impltmp
print$val<token> x = print_token x
//
impltmp
print$val<t0int> x = print_t0int x
//
(* ****** ****** *)
//
impltmp
print$val<s0exp> x = print_s0exp x
impltmp
print$val<s0qua> x = print_s0qua x
//
(* ****** ****** *)
//
impltmp
print$val<d0pat> x = print_d0pat x
impltmp
print$val<d0exp> x = print_d0exp x
//
impltmp
print$val<q0arg> x = print_q0arg x
//
impltmp
print$val<a0typ> x = print_a0typ x
impltmp
print$val<d0arg> x = print_d0arg x
//
impltmp
print$val<f0arg> x = print_f0arg x
//
impltmp
print$val<sq0arg> x = print_sq0arg x
//
impltmp
print$val<tq0arg> x = print_tq0arg x
//
impltmp
print$val<ti0arg> x = print_ti0arg x
//
(* ****** ****** *)
//
impltmp
print$val<d0ecl> x = print_d0ecl x
//
(* ****** ****** *)
//
impltmp
(a)//tmp
print$val<dl0abeled(a)> x = print_dl0abeled<a> x
//
(* ****** ****** *)

impltmp
print$val<g0marg> x = print_g0marg x
impltmp
print$val<g0exp> x = print_g0exp x
impltmp
print$val<g0eid> x = print_i0dnt x


(* ****** ****** *)

implement
print_q0arg(x0) =
(
//
case+
x0.node() of
(*
| Q0ARGnone(tok) =>
  fprint!(out, "Q0ARGnone(", tok, ")")
*)
| Q0ARGsome(sid, opt) =>
  print!("Q0ARGsome(", sid, "; ", opt, ")")
//
) (* end of [fprint_q0arg] *)

(* ****** ****** *)

implement
print_a0typ(x0) =
(
//
case+ x0.node() of
(*
| A0TYPnone(tok) =>
  fprint!(out, "A0TYPnone(", tok, ")")
*)
| A0TYPsome(s0e, opt) =>
  print!("A0TYPsome(", s0e, "; ", opt, ")")
//
) (* end of [fprint_a0typ] *)

(* ****** ****** *)

local
//
fun
print_a0typlstopt
(opt: a0typlstopt): void =
(
case+ opt of
| optn1_none() => print!("None()")
| optn1_some(a0ts) => print!("Some(", a0ts, ")")
)
//
#symload print with print_a0typlstopt of 100
//
in (* in-of-local *)

implement
print_d0arg(x0) =
(
//
case+ x0.node() of
//
| D0ARGnone(tok) =>
  print!("D0ARGnone(", tok, ")")
//
| D0ARGsome_sta
  (tbeg, s0qs, tend) =>
  print!("D0ARGsome_sta("
  , tbeg, "; ", s0qs, "; ", tend, ")")
//
| D0ARGsome_dyn1
  (tok) =>
  print!("D0ARGsome_dyn1(", tok, ")")
| D0ARGsome_dyn2
  (tbeg, arg0, opt1, tend) =>
  print!("D0ARGsome_dyn("
  , tbeg, "; ", arg0, "; ", opt1, "; ", tend, ")")
//
) (* end of [fprint_d0arg] *)

end // end of [local]

(* ****** ****** *)

implement
print_f0arg(x0) =
(
//
case+
x0.node() of
| F0ARGnone(tok) =>
  print!("F0ARGnone(", tok, ")")
| F0ARGsome_dyn(d0p) =>
  print!("F0ARGsome_dyn(", d0p, ")")
| F0ARGsome_sta(tbeg, s0qs, tend) =>
  print!("F0ARGsome_sta(", tbeg, "; ", s0qs, "; ", tend, ")")
| F0ARGsome_met(tbeg, s0es, tend) =>
  print!("F0ARGsome_met(", tbeg, "; ", s0es, "; ", tend, ")")
//
) (* end of [fprint_f0arg] *)

(* ****** ****** *)

implement
print_sq0arg(x0) =
(
//
case+
x0.node() of
| SQ0ARGnone(tok) =>
  print!("SQ0ARGnone(", tok, ")")
(*
| SQ0ARGs0rtsome(q0as) =>
  print!("SQ0ARGs0rtsome(", q0as, ")")
*)
| SQ0ARGsome(tbeg, q0as, tend) =>
  print!("SQ0ARGsome(", tbeg, "; ", q0as, "; ", tend, ")")
//
) (* end of [fprint_sq0arg] *)

(* ****** ****** *)

implement
print_tq0arg(x0) =
(
//
case+
x0.node() of
| TQ0ARGnone(tok) =>
  print!("TQ0ARGnone(", tok, ")")
| TQ0ARGsome(tbeg, q0as, tend) =>
  print!("TQ0ARGsome(", tbeg, "; ", q0as, "; ", tend, ")")
//
) (* end of [fprint_tq0arg] *)

(* ****** ****** *)

implement
print_ti0arg(x0) =
(
//
case+
x0.node() of
| TI0ARGnone(tok) =>
  print!("TI0ARGnone(", tok, ")")
| TI0ARGsome(tbeg, q0as, tend) =>
  print!("TI0ARGsome(", tbeg, "; ", q0as, "; ", tend, ")")
//
) (* end of [fprint_ti0arg] *)

(* ****** ****** *)

impltmp
{a}(*tmp*)
print_dl0abeled(x0) = let
//
val+DL0ABELED(l0, t0, x1) = x0
//
in
  print!("SL0ABELED(");
  print!(l0, ", ", t0, ", ");
  print$val<a>(x1); print!(")")
end // end of [fprint_dl0abeled]

(* ****** ****** *)

(*
implement
print_d0pat(x0) =
fprint_d0pat(stdout_ref, x0)
implement
prerr_d0pat(x0) =
fprint_d0pat(stderr_ref, x0)
*)

local

impltmp
print$val<d0pat> x = print_d0pat x

in (* in-of-local *)

implement
print_d0pat(x0) =
(
case+ x0.node() of
//
| D0Pid(id) =>
  print!("D0Pid(", id, ")")
//
| D0Pint(i0) =>
  print!("D0Pint(", i0, ")")
| D0Pchr(c0) =>
  print!("D0Pchr(", c0, ")")
| D0Pflt(f0) =>
  print!("D0Pflt(", f0, ")")
| D0Pstr(s0) =>
  print!("D0Pstr(", s0, ")")
//
| D0Papps(d0ps) =>
  print!("D0Papps(", d0ps, ")")
//
| D0Psqarg
  (tbeg, s0as, tend) =>
  print!("D0Psqarg("
  , tbeg, "; ", s0as, "; ", tend, ")")
//
| D0Pparen
  (tbeg, d0ps, tend) =>
  print!("D0Pparen("
  , tbeg, "; ", d0ps, "; ", tend, ")")
//
| D0Ptuple
  (tbeg, topt, d0ps, tend) =>
  print!("D0Ptuple("
  , tbeg, "; ", topt, "; ", d0ps, "; ", tend, ")")
| D0Precord
  (tbeg, topt, ld0ps, tend) =>
  print!("D0Precord("
  , tbeg, "; ", topt, "; ", ld0ps, "; ", tend, ")")
//
| D0Panno
  (d0p, ann) =>
  print!("D0Panno(", d0p, "; ", ann, ")")
//
| D0Pqual
  (tok, d0p) =>
  print!("D0Pqual(", tok, "; ", d0p, ")")
//
| D0Pnone(tok) => print!("D0Pnone(", tok, ")")
//
) (* end of [fprint_d0pat] *)

end // end of [local]

(* ****** ****** *)

(*
implement
print_d0pat_RPAREN(x0) =
fprint_d0pat_RPAREN(stdout_ref, x0)
implement
prerr_d0pat_RPAREN(x0) =
fprint_d0pat_RPAREN(stderr_ref, x0)
*)

local

impltmp
print$val<d0pat> x = print_d0pat x

in (* in-of-local *)

implement
print_d0pat_RPAREN(x0) =
(
case+ x0 of
| d0pat_RPAREN_cons0(tok) =>
  print!("d0pat_RPAREN_cons0(", tok, ")")
| d0pat_RPAREN_cons1(tok1, d0ps, tok2) =>
  print!("d0pat_RPAREN_cons1(", tok1, ", ", d0ps, ", ", tok2, ")")
) (* end of [fprint_d0pat_RPAREN] *)

end // end of [local]

(* ****** ****** *)

(*
implement
print_labd0pat_RBRACE(x0) =
fprint_labd0pat_RBRACE(stdout_ref, x0)
implement
prerr_labd0pat_RBRACE(x0) =
fprint_labd0pat_RBRACE(stderr_ref, x0)
*)

local

impltmp
print$val<d0pat> x = print_d0pat x

in (* in-of-local *)

implement
print_labd0pat_RBRACE(x0) =
(
case+ x0 of
| labd0pat_RBRACE_cons0(tok) =>
  print!("labd0pat_RBRACE_cons0(", tok, ")")
| labd0pat_RBRACE_cons1(tok1, ld0ps, tok2) =>
  print!("labd0pat_RBRACE_cons1(", tok1, ", ", ld0ps, ", ", tok2, ")")
) (* end of [fprint_labd0pat_RBRACE] *)

end // end of [local]

(* ****** ****** *)

(*
implement
print_d0exp(x0) =
fprint_d0exp(stdout_ref, x0)
implement
prerr_d0exp(x0) =
fprint_d0exp(stderr_ref, x0)
*)

local

impltmp
print$val<d0exp> x = print_d0exp x

in (* in-of-local *)

implement
print_d0exp(x0) =
(
case+ x0.node() of
//
| D0Eid(id) =>
  print!("D0Eid(", id, ")")
//
| D0Eint(i0) =>
  print!("D0Eint(", i0, ")")
| D0Echr(c0) =>
  print!("D0Echr(", c0, ")")
| D0Eflt(f0) =>
  print!("D0Eflt(", f0, ")")
| D0Estr(s0) =>
  print!("D0Estr(", s0, ")")
//
| D0Eapps(d0es) =>
  print!("D0Eapps(", d0es, ")")
//
| D0Esqarg
  (tbeg, s0es, tend) =>
  print!
  ("D0Esqarg("
  , tbeg, "; ", s0es, "; ", tend, ")")
| D0Etqarg
  (tbeg, s0es, tend) =>
  print!("D0Etqarg("
  , tbeg, "; ", s0es, "; ", tend, ")")
//
| D0Eparen
  (tbeg, d0es, tend) =>
  print!("D0Eparen("
  , tbeg, "; ", d0es, "; ", tend, ")")
//
| D0Etuple
  (tbeg, topt, d0es, tend) =>
  print!("D0Etuple("
  , tbeg, "; ", topt, "; ", d0es, "; ", tend, ")")
| D0Erecord
  (tbeg, topt, ld0es, tend) =>
  print!("D0Erecord("
  , tbeg, "; ", topt, "; ", ld0es, "; ", tend, ")")
//
| D0Eif0
  (tif0, d0e1, d0e2, d0e3, tend) =>
  print!("D0Eif0(", tif0, "; "
  , d0e1, "; ", d0e2, "; ", d0e3, "; ", tend, ")")
//
| D0Ecase
  (tok0, d0e1, tof2, tbar, d0cs, tend) =>
  print!("D0Ecase(", tok0, "; "
  , d0e1, "; ", tof2, "; ", tbar, "; ", "...", "; ", tend, ")")
//
| D0Elet
  (tok0, d0cs, tok1, d0es, tok2) =>
  print!("D0Elet(", tok0, "; "
  , d0cs, "; ", tok1, "; ", d0es, "; ", tok2, ")")
//
| D0Ewhere(d0e1, d0cs) =>
  print!("D0Ewhere(", d0e1, "; ", d0cs, ")")
//
| D0Edtsel
  (tok, lab, arg) =>
  print!("D0Edtsel(", tok, "; ", lab, "; ", arg, ")")
//
| D0Elam
  (tok0, arg1, res2, farrw, fbody, tend) =>
  print!("D0Elam(", tok0, "; "
  , arg1, "; ", res2, "; ", farrw, "; ", fbody, "; ", tend, ")")
//
| D0Eanno
  (d0e, ann) =>
  print!("D0Eanno(", d0e, "; ", ann, ")")
//
| D0Equal
  (tok, d0e) =>
  print!("D0Equal(", tok, "; ", d0e, ")")
//
| D0Enone(tok) => print!("D0Enone(", tok, ")")
//
) (* end of [fprint_d0exp] *)

end // end of [local]

(* ****** ****** *)

(*
implement
print_d0exp_RPAREN(x0) =
fprint_d0exp_RPAREN(stdout_ref, x0)
implement
prerr_d0exp_RPAREN(x0) =
fprint_d0exp_RPAREN(stderr_ref, x0)
*)

local

impltmp
print$val<d0exp> x = print_d0exp x

in (* in-of-local *)

implement
print_d0exp_RPAREN(x0) =
(
case+ x0 of
| d0exp_RPAREN_cons0(tok) =>
  print!("d0exp_RPAREN_cons0(", tok, ")")
| d0exp_RPAREN_cons1(tok1, d0es, tok2) =>
  print!("d0exp_RPAREN_cons1(", tok1, ", ", d0es, ", ", tok2, ")")
| d0exp_RPAREN_cons2(tok1, d0es, tok2) =>
  print!("d0exp_RPAREN_cons2(", tok1, ", ", d0es, ", ", tok2, ")")
) (* end of [fprint_d0exp_RPAREN] *)

end // end of [local]

(* ****** ****** *)

(*
implement
print_labd0exp_RBRACE(x0) =
fprint_labd0exp_RBRACE(stdout_ref, x0)
implement
prerr_labd0exp_RBRACE(x0) =
fprint_labd0exp_RBRACE(stderr_ref, x0)
*)

local

impltmp
print$val<d0exp> x = print_d0exp x

in (* in-of-local *)

implement
print_labd0exp_RBRACE(x0) =
(
case+ x0 of
| labd0exp_RBRACE_cons0(tok) =>
  print!("labd0exp_RBRACE_cons0(", tok, ")")
| labd0exp_RBRACE_cons1(tok1, ld0es, tok2) =>
  print!("labd0exp_RBRACE_cons1(", tok1, ", ", ld0es, ", ", tok2, ")")
) (* end of [fprint_labd0exp_RBRACE] *)

end // end of [local]

(* ****** ****** *)

(*
implement
print_d0exp_THEN(x0) =
fprint_d0exp_THEN(stdout_ref, x0)
implement
prerr_d0exp_THEN(x0) =
fprint_d0exp_THEN(stderr_ref, x0)
*)
implement
print_d0exp_THEN(x0) =
(
case+ x0 of
| d0exp_THEN(tok, d0e) =>
  print!("d0exp_THEN(", tok, "; ", d0e, ")")
) (* end of [fprint_d0exp_THEN] *)

(* ****** ****** *)

(*
implement
print_d0exp_ELSE(x0) =
fprint_d0exp_ELSE(stdout_ref, x0)
implement
prerr_d0exp_ELSE(x0) =
fprint_d0exp_ELSE(stderr_ref, x0)
*)
implement
print_d0exp_ELSE(x0) =
(
case+ x0 of
| d0exp_ELSEnone() =>
  print!("d0exp_ELSEnone(", ")")
| d0exp_ELSEsome(tok, d0e) =>
  print!("d0exp_ELSEsome(", tok, "; ", d0e, ")")
) (* end of [fprint_d0exp_ELSE] *)

(* ****** ****** *)
//
(*
implement
print_endwhere(x0) =
fprint_endwhere(stdout_ref, x0)
implement
prerr_endwhere(x0) =
fprint_endwhere(stderr_ref, x0)
*)
//
implement
print_endwhere(x0) =
(
case+ x0 of
| endwhere_cons1(tok) =>
  print!("endwhere_cons1(", tok, ")")
| endwhere_cons2(tok1, opt2) =>
  print!("endwhere_cons2(", tok1, "; ", opt2, ")")
)
//
(* ****** ****** *)
//
(*
implement
print_d0eclseq_WHERE(x0) =
fprint_d0eclseq_WHERE(stdout_ref, x0)
implement
prerr_d0eclseq_WHERE(x0) =
fprint_d0eclseq_WHERE(stderr_ref, x0)
*)
implement
print_d0eclseq_WHERE(x0) =
(
case+ x0 of
| d0eclseq_WHERE
  (tok0, opt1, d0cs, opt2) =>
  print!("d0eclseq_WHERE("
  , tok0, "; ", opt1, "; ", d0cs, "; ", opt2, ")")
) (* end of [fprint_d0eclseq_WHERE] *)
//
(* ****** ****** *)

(*
implement
print_f0unarrow(x0) =
fprint_f0unarrow(stdout_ref, x0)
implement
prerr_f0unarrow(x0) =
fprint_f0unarrow(stderr_ref, x0)
*)
implement
print_f0unarrow(x0) =
(
case+ x0 of
| F0UNARROWnone(tok) =>
  print!("F0UNARROWnone(", tok, ")")
| F0UNARROWdflt(tok) =>
  print!("F0UNARROWdflt(", tok, ")")
| F0UNARROWlist(tbeg, s0es, tend) =>
  print!("F0UNARROWlist(", tbeg, "; ", s0es, "; ", tend, ")")
) (* end of [fprint_f0unarrow] *)

(* ****** ****** *)

(*
implement
print_declmodopt(x0) =
fprint_declmodopt(stdout_ref, x0)
implement
prerr_declmodopt(x0) =
fprint_declmodopt(stderr_ref, x0)
*)
implement
print_declmodopt(x0) =
(
case+ x0 of
//
| DECLMODnone() =>
  print!("DECLMODnone(", ")")
//
| DECLMODsing(tok, id0) =>
  print!("DECLMODsing(", tok, "; ", id0, ")")
| DECLMODlist(tok, tbeg, ids, tend) =>
  print!("DECLMODlist("
  , tok, "; ", tbeg, "; ", ids, "; ", tend, ")")
)

(* ****** ****** *)

(*
implement
print_teqd0expopt(x0) =
fprint_teqd0expopt(stdout_ref, x0)
implement
prerr_teqd0expopt(x0) =
fprint_teqd0expopt(stderr_ref, x0)
*)
implement
print_teqd0expopt(x0) =
(
case+ x0 of
| TEQD0EXPnone() =>
  print!("TEQD0EXPnone(", ")")
| TEQD0EXPsome(tok, d0e) =>
  print!("TEQD0EXPsome(", tok, "; ", d0e, ")")
)

(* ****** ****** *)

(*
implement
print_wths0expopt(x0) =
fprint_wths0expopt(stdout_ref, x0)
implement
prerr_wths0expopt(x0) =
fprint_wths0expopt(stderr_ref, x0)
*)
implement
print_wths0expopt(x0) =
(
case+ x0 of
| WTHS0EXPnone() =>
  print!("WTHS0EXPnone(", ")")
| WTHS0EXPsome(tok, d0e) =>
  print!("WTHS0EXPsome(", tok, "; ", d0e, ")")
)

(* ****** ****** *)

(*
implement
print_d0ecl(x0) =
fprint_d0ecl(stdout_ref, x0)
implement
prerr_d0ecl(x0) =
fprint_d0ecl(stderr_ref, x0)
*)

local

impltmp
print$val<d0ecl> x = print_d0ecl x
impltmp
print$val<v0aldecl> x = print_v0aldecl x
impltmp
print$val<v0ardecl> x = print_v0ardecl x
impltmp
print$val<f0undecl> x = print_f0undecl x
impltmp
print$val<d0cstdecl> x = print_d0cstdecl x

in (* in-of-local *)

implement
print_d0ecl(x0) =
(
case+ x0.node() of
//
| D0Cnone(tok) =>
  print!("D0Cnone(", tok, ")")
//
| D0Ctokerr(tok) =>
  print!("D0Ctokerr(", tok, ")")
//
| D0Cnonfix(tok, ids) =>
  print!("D0Cnonfix(", tok, "; ", ids, ")")
| D0Cfixity(tok, ids, opt) =>
  print!("D0Cfixity(", tok, "; ", ids, "; ", opt, ")")
//
| D0Cstatic(tok, d0c) =>
  print!("D0Cstatic(", tok, "; ", d0c, ")")
| D0Cextern(tok, d0c) =>
  print!("D0Cextern(", tok, "; ", d0c, ")")
//

| D0Cdefine
  (tok, gid, gmas, gdef) =>
  print!
  ("D0Cdefine(", gid, "; ", gmas, "; ", gdef, ")")
//
| D0Cmacdef
  (tok, gid, gmas, mdef) =>
  print!
  ("D0Cmacdef("
  , gid, "; ", gmas, "; ", mdef, ")")

//
| D0Cinclude(tok, d0e) =>
  print!("D0Cinclude(", tok, "; ", d0e, ")")
//
| D0Cstaload(tok, d0e) =>
  print!("D0Cstaload(", tok, "; ", d0e, ")")
(*
| D0Cdynload(tok, d0e) =>
  print!("D0Cdynload(", tok, "; ", d0e, ")")
*)
//
| D0Cabssort(tok, tid) =>
  print!("D0Cabssort(", tok, "; ", tid, ")")
//
| D0Cstacst0
  (tok, sid, tmas, tok1, s0t2) =>
print!("D0Cstacst0("
  , tok, "; ", sid, "; ", tmas, "; ", tok1, "; ", s0t2, ")")
//
| D0Csortdef
  (tok, tid, tok1, def2) =>
print!("D0Csortdef("
  , tok, "; ", tid, "; ", tok1, "; ", def2, ")")
| D0Csexpdef
  ( tok, sid
  , arg, res, tok1, tdef) =>
  print!("D0Csexpdef("
  , tok, "; ", sid, "; "
  , arg, "; ", res, "; ", tok1, "; ", tdef, ")")
//
| D0Cabstype
  (tok, sid, arg, res, tdef, eq0opt) =>
print!("D0Cabstype("
  , tok, "; ", sid, "; ", arg, "; ", res, "; ", tdef, ")")
//
| D0Cabsimpl
  (tok, sqid, smas, res0, teq1, def2) =>
print!("D0Cabsimpl("
  , tok, "; ", sqid, "; "
  , smas, "; ", res0, "; ", teq1, "; ", def2, ")")
//
| D0Cvaldecl
  (tok, mopt, d0cs) =>
  print!("D0Cvaldecl(", tok, "; ", mopt, "; ", d0cs, ")")
//
| D0Cvardecl
  (tok, d0cs) =>
  (
    print!("D0Cvardecl(", tok, "; ", d0cs, ")")
  ) (*D0Cvardecl*)
//
| D0Cfundecl
  (tok, mopt, tqas, d0cs) =>
  print!("D0Cfundecl(", tok, "; ", mopt, "; ", tqas, "; ", d0cs, ")")
//
| D0Cimpdecl
  ( tok, mopt //, s0as
  , sqas, tqas
  , dqid, tias, f0as, res0, teq1, d0e2) =>

  print!("D0Cimpdecl("
  , tok, "; ", mopt, "; sq0arglst"
  , sqas, "; tq0arglst", tqas, "; "
  , dqid, "; ti0arglst", tias, "; f0arglst", f0as, "; "
  , res0, "; ", teq1, "; ", d0e2, ")")
//
| D0Csymload
  (tok, sym, twth, dqid, tint) =>
print!("D0Csymload("
  , tok, "; ", sym, "; "
  , twth, "; ", dqid, "; ", tint, ")")
//
| D0Cdatasort(tok, d0cs) =>
  print!("D0Cdatasort(", tok, "; ", d0cs, ")")
//
| D0Cdatatype(tok, d0cs, wopt) =>
  print!("D0Cdatatype(", tok, "; ", d0cs, "; ", wopt, ")")
//
| D0Cdynconst
  (tok, tqas, d0cs) =>
  print!("D0Cdynconst(", tok, "; ", tqas, "; ", d0cs, ")")
//
| D0Clocal
  (tok, d0cs0, tok1, d0cs1, tok2) =>
    print!("D0Clocal("
  , tok, "; ", d0cs0, "; ", tok1, "; ", d0cs1, "; ", tok2, ")")
//
(*
| _(*rest-of-d1ecl*) =>
    print!("fprint_d1ecl: D0C...: not-yet-implemented")
*)
//
) (* end of [fprint_d0ecl] *)

end // end of [local]

(* ****** ****** *)
//
(*
implement
print_precopt(x0) =
fprint_precopt(stdout_ref, x0)
implement
prerr_precopt(x0) =
fprint_precopt(stderr_ref, x0)
*)
//
impltmp
print$val<precmod> x = print_precmod x
impltmp
print$val<i0dnt> x = print_i0dnt x

implement
print_precopt(x0) =
(
case+ x0 of
| PRECOPTnil() =>
  print!("PRECOPTnil()")
| PRECOPTint(tint) =>
  print!("PRECOPTint(", tint, ")")
| PRECOPTopr(topr, pmod) => (print!("PRECOPTopr(", topr, "; ", pmod, ")"))

(*
  where
  {
    val _ = $showtype(topr)
    val _ = $showtype(pmod)
  }
*)
) (* end of [fprint_precopt] *)
//
implement
print_signint(x0) =
(
case+ x0 of
| SIGNINTint(tint) =>
  print!("SIGNINTint(", tint, ")")
| SIGNINTopr(topr, tint) =>
  print!("SIGNINTopr(", topr, "; ", tint, ")")
)
//
implement
print_precmod(x0) =
(
case+ x0 of
| PRECMODnone() =>
  print!("PRECMODnone()")
| PRECMODsome(tbeg, sint, tend) =>
  let
    (* val _ = $showtype(tbeg) *)
  in
  print!("PRECMODsome(", tbeg, "; ", sint, "; ", tend, ")")
  end
)
//
(* ****** ****** *)

(*
implement
print_abstdf0(x0) =
fprint_abstdf0(stdout_ref, x0)
implement
prerr_abstdf0(x0) =
fprint_abstdf0(stderr_ref, x0)
*)

implement
print_abstdf0(x0) =
(
case+ x0 of
| ABSTDF0nil() =>
  print("ABSTDF0nil()")
| ABSTDF0lteq(tok, s0e) =>
  print!("ABSTDF0lteq(", tok, "; ", s0e, ")")
| ABSTDF0eqeq(tok, s0e) =>
  print!("ABSTDF0eqeq(", tok, "; ", s0e, ")")
) (* end of [fprint_abstdf0] *)

(* ****** ****** *)

implement
print_g0expdef(x0) =
(
case+ x0 of
| G0EDEFnone() =>
  print("G0EDEFnone()")
| G0EDEFsome(tokopt, g0exp(*def*)) =>
  print!("G0EDEFsome(", tokopt, "; ", g0exp, ")")
)

implement
print_d0macdef(x0) =
(
case+ x0 of
| D0MDEFnone() =>
  print("D0MDEFnone()")
| D0MDEFsome(tokopt, d0exp(*def*)) =>
  print!("D0MDEFsome(", tokopt, "; ", d0exp,")")
)

(* ****** ****** *)

(*
implement
print_wd0eclseq(x0) =
fprint_wd0eclseq(stdout_ref, x0)
implement
prerr_wd0eclseq(x0) =
fprint_wd0eclseq(stderr_ref, x0)
*)

implement
print_wd0eclseq(x0) =
(
case+ x0 of
| WD0CSnone() =>
  print("WD0CSnone()")
| WD0CSsome(tbeg, topt, d0cs, tend) =>
  print!("WD0CSsome("
  , tbeg, "; ", topt, "; ", d0cs, "; ", tend, ")")
) (* end of [fprint_wd0eclseq] *)

(* ****** ****** *)

(*
implement
print_v0aldecl(x0) =
fprint_v0aldecl(stdout_ref, x0)
implement
prerr_v0aldecl(x0) =
fprint_v0aldecl(stderr_ref, x0)
*)

implement
print_v0aldecl(x0) = let
//
val+V0ALDECL(rcd) = x0
//
in
  print!("V0ALDECL@{"
  , ", pat=", rcd.pat, ", teq=", rcd.teq
  , ", def=", rcd.def, ", wtp=", rcd.wtp, "}")
end // end of [fprint_v0aldecl]

(* ****** ****** *)

(*
implement
print_v0ardecl(x0) =
fprint_v0ardecl(stdout_ref, x0)
implement
prerr_v0ardecl(x0) =
fprint_v0ardecl(stderr_ref, x0)
*)

implement
print_v0ardecl(x0) = let
//
val+V0ARDECL(rcd) = x0
//
in
  print!("V0ARDECL@{"
  , ", nam=", rcd.nam
  , ", wth=", rcd.wth
  , ", res=", rcd.res, ", ini=", rcd.ini, "}")
end // end of [fprint_v0ardecl]

(* ****** ****** *)

(*
implement
print_f0undecl(x0) =
fprint_f0undecl(stdout_ref, x0)
implement
prerr_f0undecl(x0) =
fprint_f0undecl(stderr_ref, x0)
*)

implement
print_f0undecl(x0) = let
//
val+F0UNDECL(rcd) = x0
//
in
  print!("F0UNDECL@{"
  , ", nam=", rcd.nam
  , ", arg=", "f0arglst", rcd.arg
  , ", res=", rcd.res, ", teq=", rcd.teq
  , ", def=", rcd.def, ", wtp=", rcd.wtp, "}")
end // end of [fprint_f0undecl]

(* ****** ****** *)

(*
implement
print_d0cstdecl(x0) =
fprint_d0cstdecl(stdout_ref, x0)
implement
prerr_d0cstdecl(x0) =
fprint_d0cstdecl(stderr_ref, x0)
*)

implement
print_d0cstdecl(x0) = let
//
val+D0CSTDECL(rcd) = x0
//
in
  print!("D0CSTDECL@{"
  , ", nam=", rcd.nam, ", arg=", rcd.arg
  , ", res=", rcd.res, ", def=", rcd.def, "}")
end // end of [fprint_d0cstdecl]

(* ****** ****** *)

(* end of [xats_dynexp0_print.dats] *)
