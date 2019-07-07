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
// Start Time: April, 2018
// Authoremail: gmhwxiATgmailDOTcom
//
(* ****** ****** *)

#include "./share.sats"

#staload "./staexp0.sats"

(* ****** ****** *)
//
typedef
t0intopt = optn1(t0int)
//
(* ****** ****** *)
//
datatype
dl0abeled(a:tbox) =
  DL0ABELED of (l0abl, token, a)
//
fun
{a:tbox}
print_dl0abeled(x0: dl0abeled(a)): void
fun
{a:tbox}
show_dl0abeled(x0: dl0abeled(a)): void
//
(* ****** ****** *)

abstbox d0pat_tbox = ptr
typedef d0pat = d0pat_tbox
typedef d0patlst = list1_0(d0pat)
typedef d0patopt = optn1(d0pat)
typedef labd0pat = dl0abeled(d0pat)
typedef labd0patlst = list1_0(labd0pat)

(* ****** ****** *)

abstbox d0exp_tbox = ptr
typedef d0exp = d0exp_tbox
typedef d0explst = list1_0(d0exp)
typedef d0expopt = optn1(d0exp)
typedef labd0exp = dl0abeled(d0exp)
typedef labd0explst = list1_0(labd0exp)

(* ****** ****** *)
//
abstbox d0gua_tbox = ptr
typedef d0gua = d0gua_tbox
typedef d0gualst = list1_0(d0gua)
//
abstbox dg0pat_tbox = ptr
typedef dg0pat = dg0pat_tbox
//
abstbox d0clau_tbox = ptr
typedef d0clau = d0clau_tbox
typedef d0claulst = list1_0(d0clau)
//
(* ****** ****** *)
//
abstbox d0ecl_tbox = ptr
typedef d0ecl = d0ecl_tbox
typedef d0eclist = list1_0(d0ecl)
typedef d0eclopt = optn1(d0ecl)
//
vtypedef d0eclist_vt = list1_0_vt(d0ecl)
//
(* ****** ****** *)

abstbox q0arg_tbox = ptr
typedef q0arg = q0arg_tbox
typedef q0arglst = list1_0(q0arg)

datatype
q0arg_node =
  (*
  | Q0ARGnone of token
  *)
  | Q0ARGsome of (i0dnt, sort0opt)

  fun
  print_q0arg : print_type(q0arg)
  #symload print with print_q0arg
  fun
  show_q0arg : print_type(q0arg)
  #symload show with show_q0arg

  fun
  q0arg_get_loc(q0arg): loc_t
  fun
  q0arg_get_node(q0arg): q0arg_node

  #symload .loc with q0arg_get_loc
  #symload .node with q0arg_get_node

  fun
  q0arg_make_node
  (loc: loc_t, node: q0arg_node): q0arg

(* ****** ****** *)

abstbox sq0arg_tbox = ptr

typedef sq0arg = sq0arg_tbox
typedef sq0arglst = list1_0(sq0arg)


datatype
sq0arg_node =
  | SQ0ARGnone of token
  (* RK : to account for `impltmp(a:tflt) F(con(a)) ...`
  | SQ0ARGs0rtsome of (s0marg)
  *)
  | SQ0ARGsome of
    (token(*'{'*), q0arglst, token(*'}'*))

  fun
  print_sq0arg : print_type(sq0arg)
  #symload print with print_sq0arg
  fun
  show_sq0arg : print_type(sq0arg)
  #symload show with show_sq0arg

  fun
  sq0arg_get_loc(sq0arg): loc_t
  fun
  sq0arg_get_node(sq0arg): sq0arg_node

  #symload .loc with sq0arg_get_loc
  #symload .node with sq0arg_get_node

  fun
  sq0arg_make_node
  (loc: loc_t, node: sq0arg_node): sq0arg

(* ****** ****** *)

abstbox eq0arg_tbox = ptr

typedef eq0arg = eq0arg_tbox
typedef eq0argopt = optn1(eq0arg)

datatype
eq0arg_node =
  | EQ0ARGnone of token
  | EQ0ARGsome of
    (token(*'='*), s0exp)

  fun
  print_eq0arg : print_type(eq0arg)
  #symload print with print_eq0arg
  fun
  show_eq0arg : print_type(eq0arg)
  #symload show with show_eq0arg

  fun
  eq0arg_get_loc(eq0arg): loc_t
  fun
  eq0arg_get_node(eq0arg): eq0arg_node

  #symload .loc with eq0arg_get_loc
  #symload .node with eq0arg_get_node

  fun
  eq0arg_make_node
  (loc: loc_t, node: eq0arg_node): eq0arg


abstbox eq0opt_tbox = ptr

typedef eq0opt = eq0opt_tbox

datatype
eq0opt_node =
  | EQ0ARGopt of (token(*'='*), eq0argopt)

  fun
  print_eq0opt : print_type(eq0opt)
  #symload print with print_eq0opt
  fun
  show_eq0opt : print_type(eq0opt)
  #symload show with show_eq0opt

  fun
  eq0opt_get_loc(eq0opt): loc_t
  fun
  eq0opt_get_node(eq0opt): eq0opt_node

  #symload .loc with eq0opt_get_loc
  #symload .node with eq0opt_get_node

  fun
  eq0opt_make_node
  (loc: loc_t, node: eq0opt_node): eq0opt

(* ****** ****** *)

(*
abstbox i0mparg_tbox = ptr

typedef i0mparg = i0mparg_tbox
typedef i0mparglst = list1_0(sq0arg)


datatype
i0mparg_node =
  | I0MPARGnone of token
  | I0MPARGs0marg of (s0marg)
  | I0MPARGsq0arg of (sq0arglst)

  fun
  print_i0mparg : print_type(i0mparg)
  #symload print with print_i0mparg
  fun
  show_i0mparg : print_type(i0mparg)
  #symload show with show_i0mparg

  fun
  i0mparg_get_loc(sq0arg): loc_t
  fun
  i0mparg_get_node(sq0arg): i0mparg_node

  #symload .loc with i0mparg_get_loc
  #symload .node with i0mparg_get_node

  fun
  i0mparg_make_node
  (loc: loc_t, node: i0mparg_node): i0mparg
*)

(* ****** ****** *)

abstbox tq0arg_tbox = ptr

typedef tq0arg = tq0arg_tbox
typedef tq0arglst = list1_0(tq0arg)

datatype
tq0arg_node =
  | TQ0ARGnone of token
  | TQ0ARGsome of
    (token(*'<'*), q0arglst, token(*'>'*))

  fun
  print_tq0arg : print_type(tq0arg)
  #symload print with print_tq0arg
  fun
  show_tq0arg : print_type(tq0arg)
  #symload show with show_tq0arg

  fun
  tq0arg_get_loc(tq0arg): loc_t
  fun
  tq0arg_get_node(tq0arg): tq0arg_node

  #symload .loc with tq0arg_get_loc
  #symload .node with tq0arg_get_node

  fun
  tq0arg_make_node
  (loc: loc_t, node: tq0arg_node): tq0arg

(* ****** ****** *)

abstbox ti0arg_tbox = ptr

typedef ti0arg = ti0arg_tbox
typedef ti0arglst = list1_0(ti0arg)


datatype
ti0arg_node =
  | TI0ARGnone of token
  | TI0ARGsome of
    (token(*'<'*), s0explst, token(*'>'*))

  fun
  print_ti0arg : print_type(ti0arg)
  #symload print with print_ti0arg
  fun
  show_ti0arg : print_type(ti0arg)
  #symload show with show_ti0arg

  fun
  ti0arg_get_loc(ti0arg): loc_t
  fun
  ti0arg_get_node(ti0arg): ti0arg_node

  #symload .loc with ti0arg_get_loc
  #symload .node with ti0arg_get_node

  fun
  ti0arg_make_node
  (loc: loc_t, node: ti0arg_node): ti0arg

(* ****** ****** *)


abstbox a0typ_tbox = ptr
typedef a0typ = a0typ_tbox
typedef a0typlst = list1_0(a0typ)
typedef a0typopt = optn1(a0typ)
typedef a0typlstopt = optn1(a0typlst)


datatype
a0typ_node =
  (*
  | A0TYPnone of token
  *)
  | A0TYPsome of (s0exp, tokenopt)

  fun
  print_a0typ : print_type(a0typ)
  #symload print with print_a0typ
  fun
  show_a0typ : print_type(a0typ)
  #symload show with show_a0typ

  fun
  a0typ_get_loc(a0typ): loc_t
  fun
  a0typ_get_node(a0typ): a0typ_node

  #symload .loc with a0typ_get_loc
  #symload .node with a0typ_get_node

  fun
  a0typ_make_node
  (loc: loc_t, node: a0typ_node): a0typ

(* ****** ****** *)

abstbox d0arg_tbox = ptr
typedef d0arg = d0arg_tbox
typedef d0arglst = list1_0(d0arg)

datatype
d0arg_node =
  | D0ARGnone of token
  | D0ARGsome_sta of (token, s0qualst, token)
  | D0ARGsome_dyn1 of s0eid
  | D0ARGsome_dyn2 of (token, a0typlst, a0typlstopt, token)

  fun
  print_d0arg : print_type(d0arg)
  #symload print with print_d0arg
  fun
  show_d0arg : print_type(d0arg)
  #symload show with show_d0arg

  fun
  d0arg_get_loc(d0arg): loc_t
  fun
  d0arg_get_node(d0arg): d0arg_node

  #symload .loc with d0arg_get_loc
  #symload .node with d0arg_get_node

  fun
  d0arg_make_node
  (loc: loc_t, node: d0arg_node): d0arg

(* ****** ****** *)


abstbox f0arg_tbox = ptr
typedef f0arg = f0arg_tbox
typedef f0arglst = list1_0(f0arg)


datatype
f0arg_node =
  | F0ARGnone of (token)
  | F0ARGsome_dyn of (d0pat)
  | F0ARGsome_sta of (token, s0qualst, token)
  | F0ARGsome_met of (token, s0explst, token)

  fun
  print_f0arg : print_type(f0arg)
  #symload print with print_f0arg
  fun
  show_f0arg : print_type(f0arg)
  #symload show with show_f0arg

  fun
  f0arg_get_loc(f0arg): loc_t
  fun
  f0arg_get_node(f0arg): f0arg_node

  #symload .loc with f0arg_get_loc
  #symload .node with f0arg_get_node

  fun
  f0arg_make_node
  (loc: loc_t, node: f0arg_node): f0arg


(* ****** ****** *)

datatype
d0pat_node =
  | D0Pid of d0pid

  | D0Pint of t0int
  | D0Pchr of t0chr
  | D0Pflt of t0flt
  | D0Pstr of t0str

  | D0Papps of d0patlst

  | D0Psqarg of//sqparg
    (token, s0arglst, token)

  | D0Pparen of
    (token, d0patlst, d0pat_RPAREN)

  | D0Ptuple of
    ( token, tokenopt
    , d0patlst, d0pat_RPAREN)
  | D0Precord of
    ( token, tokenopt
    , labd0patlst, labd0pat_RBRACE)

  | D0Panno of (d0pat, s0exp)

  | D0Pqual of (token, d0pat) // qualified

  | D0Pnone of (token) // HX-2018-09-15: indicating error
  // end of [d0pat_node]

  and
  d0pat_RPAREN =
    | d0pat_RPAREN_cons0 of token
    | d0pat_RPAREN_cons1 of (token, d0patlst, token)

  and
  labd0pat_RBRACE =
    | labd0pat_RBRACE_cons0 of token
    | labd0pat_RBRACE_cons1 of (token, labd0patlst, token)

(* ****** ****** *)

  fun
  print_d0pat : (d0pat) -> void
  #symload print with print_d0pat
  fun
  show_d0pat : print_type(d0pat)
  #symload show with show_d0pat

  fun
  d0pat_get_loc(d0pat): loc_t
  fun
  d0pat_get_node(d0pat): d0pat_node

  #symload .loc with d0pat_get_loc
  #symload .node with d0pat_get_node

  fun
  d0pat_make_node
  (loc: loc_t, node: d0pat_node): d0pat
  fun
  d0pat_anno_opt
  (d0p: d0pat, opt: s0expopt): d0pat

  //

  fun
  print_d0pat_RPAREN: print_type(d0pat_RPAREN)
  #symload print with print_d0pat_RPAREN
  fun
  show_d0pat_RPAREN : print_type(d0pat_RPAREN)
  #symload show with show_d0pat_RPAREN

  fun
  d0pat_RPAREN_loc(d0pat_RPAREN): loc_t

  //

  fun
  print_labd0pat_RBRACE: print_type(labd0pat_RBRACE)
  #symload print with print_labd0pat_RBRACE
  fun
  show_labd0pat_RBRACE : print_type(labd0pat_RBRACE)
  #symload show with show_labd0pat_RBRACE

  fun
  labd0pat_RBRACE_loc(labd0pat_RBRACE): loc_t

(* ****** ****** *)


datatype
d0exp_node =
  | D0Eid of d0eid

  | D0Eint of t0int
  | D0Echr of t0chr
  | D0Eflt of t0flt
  | D0Estr of t0str

  | D0Eapps of d0explst

  | D0Esqarg of // sexparg
    (token, s0explst, token)

  | D0Etqarg of // temparg
    (token, s0explst, token)

  | D0Eparen of
    (token, d0explst, d0exp_RPAREN)

  | D0Etuple of
    (token, tokenopt, d0explst, d0exp_RPAREN)

  | D0Erecord of
    (token, tokenopt, labd0explst, labd0exp_RBRACE)

  | D0Eif0 of
    (token, d0exp, d0exp_THEN, d0exp_ELSE, tokenopt)

  | D0Ecase of
    (token, d0exp, token(*OF*), tokenopt(*BAR*), d0claulst, tokenopt)

  | D0Elet of
    (token, d0eclist, token, d0explst, token)

  | D0Ewhere of
    (d0exp, d0eclseq_WHERE)

  | D0Edtsel of
    (token, l0abl, d0expopt)

  | D0Elam of
    (token(*lam/lam@*), f0arglst(*arglst*), effs0expopt, f0unarrow, d0exp, tokenopt)

  | D0Eanno of (d0exp, s0exp)

  | D0Equal of (token, d0exp) // qualified

  | D0Enone of (token) // HX-2018-07-08: indicating error
  // end of [d0exp_node]

  and
  d0exp_RPAREN =
    | d0exp_RPAREN_cons0 of token // (d0es1)
    | d0exp_RPAREN_cons1 of
      (token, d0explst, token) // (d0es1 | d0es2)
    | d0exp_RPAREN_cons2 of
      (token, d0explst, token) // (d0es1 ; d0es2)

  and
  labd0exp_RBRACE =
    | labd0exp_RBRACE_cons0 of token
    | labd0exp_RBRACE_cons1 of
      (token, labd0explst, token)

  and
  d0exp_THEN =
    | d0exp_THEN of (token, d0exp)

  and
  d0exp_ELSE =
    | d0exp_ELSEnone of ()
    | d0exp_ELSEsome of (token, d0exp)

  and
  endwhere =
    | endwhere_cons1 of token
    | endwhere_cons2 of (token, tokenopt)

  and
  d0eclseq_WHERE =
    | d0eclseq_WHERE of (token, tokenopt, d0eclist, endwhere)

  and
  f0unarrow =
    | F0UNARROWnone of (token(*error*))
    | F0UNARROWdflt of (token(*=>*))
    | F0UNARROWlist of (token(*=<*), s0explst, token (*>*))

  //

  fun
  print_d0exp : (d0exp) -> void
  #symload print with print_d0exp
  fun
  show_d0exp : print_type(d0exp)
  #symload show with show_d0exp

  fun
  d0exp_get_loc(d0exp): loc_t
  fun
  d0exp_get_node(d0exp): d0exp_node

  #symload .loc with d0exp_get_loc
  #symload .node with d0exp_get_node

  fun
  d0exp_make_node
  (loc: loc_t, node: d0exp_node): d0exp
  fun
  d0exp_anno_opt
  (d0e: d0exp, opt: s0expopt): d0exp

  //

  fun
  print_d0exp_RPAREN: print_type(d0exp_RPAREN)
  #symload print with print_d0exp_RPAREN
  fun
  show_d0exp_RPAREN : print_type(d0exp_RPAREN)
  #symload show with show_d0exp_RPAREN

  fun
  d0exp_RPAREN_loc(x0: d0exp_RPAREN): loc_t

  //

  fun
  print_labd0exp_RBRACE: print_type(labd0exp_RBRACE)
  #symload print with print_labd0exp_RBRACE
  fun
  show_labd0exp_RBRACE : print_type(labd0exp_RBRACE)
  #symload show with show_labd0exp_RBRACE

  fun
  labd0exp_RBRACE_loc(x0: labd0exp_RBRACE): loc_t

  //

  fun
  print_d0exp_THEN: print_type(d0exp_THEN)
  #symload print with print_d0exp_THEN
  fun
  show_d0exp_THEN : print_type(d0exp_THEN)
  #symload show with show_d0exp_THEN

  fun
  print_d0exp_ELSE: print_type(d0exp_ELSE)
  #symload print with print_d0exp_ELSE
  fun
  show_d0exp_ELSE : print_type(d0exp_ELSE)
  #symload show with show_d0exp_ELSE

  //

  fun
  print_endwhere: print_type(endwhere)
  #symload print with print_endwhere
  fun
  show_endwhere : print_type(endwhere)
  #symload show with show_endwhere

  fun
  endwhere_get_loc(endwhere): loc_t
  #symload .loc with endwhere_get_loc

  //

  fun
  print_d0eclseq_WHERE: print_type(d0eclseq_WHERE)
  #symload print with print_d0eclseq_WHERE
  fun
  show_d0eclseq_WHERE : print_type(d0eclseq_WHERE)
  #symload show with show_d0eclseq_WHERE

  (*
  fun d0eclseq_WHERE_get_loc(x0: d0eclseq_WHERE): loc_t
  *)

  //

  fun
  print_f0unarrow: print_type(f0unarrow)
  #symload print with print_f0unarrow
  fun
  show_f0unarrow : print_type(f0unarrow)
  #symload show with show_f0unarrow

(* ****** ****** *)

datatype
d0gua_node =
  | D0GUAexp of (d0exp)
  | D0GUAmat of (d0exp, token(*AS*), d0pat)

  fun
  print_d0gua: print_type(d0gua)
  #symload print with print_d0gua
  fun
  show_d0gua : print_type(d0gua)
  #symload show with show_d0gua

  fun
  d0gua_get_loc(d0gua): loc_t
  fun
  d0gua_get_node(d0gua): d0gua_node

  #symload .loc with d0gua_get_loc
  #symload .node with d0gua_get_node

  fun
  d0gua_make_node
  (loc: loc_t, node: d0gua_node): d0gua

(* ****** ****** *)
//
datatype
d0clau_node =
  | D0CLAUgpat of (dg0pat)
  | D0CLAUclau of (dg0pat, token(*EQGT*), d0exp)
  and
  dg0pat_node =
    | DG0PATpat of (d0pat)
    | DG0PATgua of (d0pat, token(*WHEN*), d0gualst)
//
  fun
  print_d0clau: print_type(d0clau)
  #symload print with print_d0clau
  fun
  show_d0clau: print_type(d0clau)
  #symload show with show_d0clau
  fun
  print_dg0pat: print_type(dg0pat)
  #symload print with print_dg0pat
  fun
  show_dg0pat: print_type(dg0pat)
  #symload show with show_dg0pat

  fun
  d0clau_get_loc(d0clau): loc_t
  fun
  d0clau_get_node(d0clau): d0clau_node

  #symload .loc with d0clau_get_loc
  #symload .node with d0clau_get_node

  fun
  dg0pat_get_loc(dg0pat): loc_t
  fun
  dg0pat_get_node(dg0pat): dg0pat_node

  #symload .loc with dg0pat_get_loc
  #symload .node with dg0pat_get_node

  fun
  d0clau_make_node(loc: loc_t, node: d0clau_node): d0clau
  fun
  dg0pat_make_node(loc: loc_t, node: dg0pat_node): dg0pat

(* ****** ****** *)

datatype
declmodopt =
  | DECLMODnone of ((*void*)) // end of [DECLMODnone]
  | DECLMODsing of (token(*COLON*), i0dnt) // end of [DECLMODsing]
  | DECLMODlist of (token(*COLON*), token, i0dntlst(*ids*), token) // end of [DECLMODlist]

  fun
  print_declmodopt: print_type(declmodopt)
  #symload print with print_declmodopt
  fun
  show_declmodopt : print_type(declmodopt)
  #symload show with show_declmodopt

(* ****** ****** *)

datatype
teqd0expopt =
  | TEQD0EXPnone of ((*void*))
  | TEQD0EXPsome of (token(*EQ*), d0exp)

datatype
wths0expopt =
  | WTHS0EXPnone of ((*void*))
  | WTHS0EXPsome of (token(*WITHTYPE*), s0exp)

  fun
  print_teqd0expopt: print_type(teqd0expopt)
  #symload print with print_teqd0expopt
  fun
  show_teqd0expopt : print_type(teqd0expopt)
  #symload show with show_teqd0expopt

  fun
  print_wths0expopt: print_type(wths0expopt)
  #symload print with print_wths0expopt
  fun
  show_wths0expopt : print_type(wths0expopt)
  #symload show with show_wths0expopt

(* ****** ****** *)
//
datatype
v0aldecl =
V0ALDECL of @{
  loc= loc_t
, pat= d0pat
, teq= token
, def= d0exp
, wtp= wths0expopt
}

  typedef
  v0aldeclist = list1_0(v0aldecl)

  fun
  print_v0aldecl: print_type(v0aldecl)
  #symload print with print_v0aldecl
  fun
  show_v0aldecl : print_type(v0aldecl)
  #symload show with show_v0aldecl

  fun
  v0aldecl_get_loc(v0aldecl): loc_t
  #symload .loc with v0aldecl_get_loc

(* ****** ****** *)
//
(*
var
foo:
s0exp with pfat = d0exp
*)
datatype
v0ardecl =
V0ARDECL of @{
  loc= loc_t
, nam= d0pid
, wth= d0pidopt
, res= s0expopt
, ini= teqd0expopt
} where
  d0pidopt = optn1(d0pid)


  typedef
  v0ardeclist = list1_0(v0ardecl)

  fun
  print_v0ardecl: print_type(v0ardecl)
  #symload print with print_v0ardecl
  fun
  show_v0ardecl : print_type(v0ardecl)
  #symload show with show_v0ardecl

  fun
  v0ardecl_get_loc(v0ardecl): loc_t
  #symload .loc with v0ardecl_get_loc

(* ****** ****** *)


datatype
f0undecl =
F0UNDECL of @{
  loc= loc_t
, nam= d0pid
, arg= f0arglst
, res= effs0expopt
, teq= token
, def= d0exp
, wtp= wths0expopt
} (* f0undecl *)

  typedef
  f0undeclist = list1_0(f0undecl)

  fun
  print_f0undecl: print_type(f0undecl)
  #symload print with print_f0undecl
  fun
  show_f0undecl : print_type(f0undecl)
  #symload show with show_f0undecl

  fun
  f0undecl_get_loc(f0undecl): loc_t
  #symload .loc with f0undecl_get_loc

(* ****** ****** *)

datatype
d0cstdecl =
D0CSTDECL of @{
  loc= loc_t
, nam= d0pid
, arg= d0arglst
, res= effs0expopt
, def= teqd0expopt
}

  typedef
  d0cstdeclist = list1_0(d0cstdecl)

  fun
  print_d0cstdecl: print_type(d0cstdecl)
  #symload print with print_d0cstdecl
  fun
  show_d0cstdecl : print_type(d0cstdecl)
  #symload show with show_d0cstdecl

  fun
  d0cstdecl_get_loc(d0cstdecl): loc_t
  #symload .loc with d0cstdecl_get_loc

(* ****** ****** *)


datatype
d0ecl_node =
  (*indicating error*)
  | D0Cnone of token
  (*for skipping error*)
  | D0Ctokerr of token // error
  //
  // HX: delete fixity
  //
  | D0Cnonfix of
    (token, i0dntlst)
  //
  // HX: attach fixity
  //
  | D0Cfixity of
    (token, i0dntlst, precopt)
  //
  // HX: locally defined
  //
  | D0Cstatic of
    (token(*STATIC*), d0ecl)
  //
  // HX: globally defined
  //
  | D0Cextern of
    (token(*EXTERN*), d0ecl)
  //
  | D0Cdefine of
    ( token
    , g0eid(*fun*)
    , g0marglst(*arg*), g0expdef(*opt*))
  | D0Cmacdef of
    ( token
    , g0eid(*fun*)
    , g0marglst(*arg*), d0macdef(*d0exp*))

  | D0Cinclude of
    (token(*INCLUDE*), d0exp)
  // HX: for file inclusion
  //
  // HX: for static loading
  //
  | D0Cstaload of
    (token(*STALOAD*), d0exp)
  (*
  | D0Cdynload of
    (token(*DYNLOAD*), d0exp)
  *)
  //
  | D0Cabssort of (token, s0tid)
  //
  | D0Cstacst0 of
    (token, s0eid, t0marglst, token, sort0)
  //
  | D0Csortdef of
    (token, s0tid, token, s0rtdef)
  //
  | D0Csexpdef of
    ( token
    , s0eid
    , s0marglst, sort0opt, token, s0exp)
  //
  | D0Cabstype of
    ( token
    , s0eid, t0marglst, sort0opt, abstdf0, eq0opt)
  //
  | D0Cabsimpl of
    ( token
    , sq0eid
    , s0marglst, sort0opt, token(*EQ*), s0exp)
    // D0Cabsimpl
  //
  | D0Cvaldecl of
    ( token(*valkind*)
    , declmodopt, v0aldeclist)
  //
  | D0Cvardecl of
      (token(*varkind*), v0ardeclist)
    // end of [D0Cvardecl]
  //
  | D0Cfundecl of
    ( token(*funkind*)
    , declmodopt, tq0arglst, f0undeclist)
  //
  | D0Cimpdecl of
    ( token(*impkind*)
    , declmodopt//modifier
    //, s0marg
    , sq0arglst, tq0arglst
    , dq0eid, ti0arglst, f0arglst, effs0expopt, token, d0exp)
  //
  | D0Csymload of
    (token(*SYMLOAD*), s0ymb, token, dq0eid, t0intopt)
  //
  | D0Cdatasort of
    (token(*datasort*), d0tsortlst)
    // D0Cdatasort
  //
  | D0Cdatatype of
    (token(*datatype*), d0atypelst, wd0eclseq)
    // D0Cdatatype
  //
  | D0Cdynconst of
    (token(*dyncstkind*), tq0arglst, d0cstdeclist)
  //
  | D0Clocal of
    (token(*LOCAL*), d0eclist, token(*IN*), d0eclist, token(*END*))
  // end of [d0ecl_node]
  (* ****** ****** *)

  and
  precopt =
    | PRECOPTnil of ()
    | PRECOPTint of (token)
    | PRECOPTopr of (i0dnt, precmod)

  and
  precmod =
    | PRECMODnone of ()
    | PRECMODsome of (token, signint, token)

  and
  signint =
    | SIGNINTint of (token(*int*))
    | SIGNINTopr of (token(*opr*), token(*int*))
  //
  and
  abstdf0 =
    | ABSTDF0nil of () // unspecified
    | ABSTDF0lteq of (token(*"<="*), s0exp)
    | ABSTDF0eqeq of (token(*"=="*), s0exp)
  //
  and
g0expdef =
  | G0EDEFnone of ()
  | G0EDEFsome of (tokenopt, g0exp(*def*))

and
d0macdef =
  | D0MDEFnone of ()
  | D0MDEFsome of (tokenopt, d0exp(*def*))
  //
  and
  wd0eclseq =
    | WD0CSnone of ()
    | WD0CSsome of
      (token(*where*), tokenopt, d0eclist, token)

(* ****** ****** *)

  fun
  print_d0ecl : (d0ecl) -> void
  #symload print with print_d0ecl
  fun
  show_d0ecl : print_type(d0ecl)
  #symload show with show_d0ecl

  fun
  d0ecl_get_loc(d0ecl): loc_t
  fun
  d0ecl_get_node(d0ecl): d0ecl_node

  #symload .loc with d0ecl_get_loc
  #symload .node with d0ecl_get_node

  fun
  d0ecl_make_node
  (loc: loc_t, node: d0ecl_node): d0ecl

(* ****** ****** *)

  fun
  print_precopt : (precopt) -> void
  #symload print with print_precopt
  fun
  show_precopt : print_type(precopt)
  #symload show with show_precopt

  fun
  print_precmod : precmod -> void //print_type(precmod)
  #symload print with print_precmod
  fun
  show_precmod : print_type(precmod)
  #symload show with show_precmod

  fun
  print_signint : print_type(signint)
  #symload print with print_signint
  fun
  show_signint : print_type(signint)
  #symload show with show_signint

  fun
  print_abstdf0 : (abstdf0) -> void
  #symload print with print_abstdf0
  fun
  show_abstdf0 : print_type(abstdf0)
  #symload show with show_abstdf0


  fun
  print_g0expdef : print_type(g0expdef)
  #symload print with print_g0expdef
  fun
  show_g0expdef : print_type(g0expdef)
  #symload show with show_g0expdef

  fun
  print_d0macdef : print_type(d0macdef)
  #symload print with print_d0macdef
  fun
  show_d0macdef : print_type(d0macdef)
  #symload show with show_d0macdef



  fun
  print_wd0eclseq : (wd0eclseq) -> void
  #symload print with print_wd0eclseq
  fun
  show_wd0eclseq : print_type(wd0eclseq)
  #symload show with show_wd0eclseq

(* ****** ****** *)

(* end of [xats_dynexp0.sats] *)
