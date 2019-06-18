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

#include "share/HATS/temptory_staload_bucs320.hats"

(* ****** ****** *)

#staload "./../SATS/location.sats"
#staload _ = "./location.dats"

(* ****** ****** *)

#staload "./../SATS/lexing.sats"
#staload "./../SATS/staexp0.sats"
#staload _ = "./staexp0.dats"
#staload "./../SATS/dynexp0.sats"
#staload _ = "./dynexp0.dats"
#staload "./../SATS/parsing.sats"


(*
#staload "libats/SATS/print.sats"
#staload _ = "libats/DATS/print.dats"

#staload "libats/SATS/gint.sats"
#staload _ = "libats/DATS/gint.dats"

#staload "libats/SATS/list.sats"
#staload _ = "libats/DATS/list.dats"

#staload "libats/SATS/list_vt.sats"
#staload _ = "libats/DATS/list_vt.dats"
*)

(* ****** ****** *)

extern castfn
ofg0
{a:tflt}
(list0(INV(a))):<> [n: int | n >= 0] list1(a, n)

extern castfn
ofg0v
{a:tflt}
(list0_vt(INV(a))):<> [n: int | n >= 0] list1_vt(a, n)

extern castfn
list1_vt2t1
{a:tflt}{n:int}
(list1_vt(INV(a),n)):<> list1(a, n)


extern fun
{x:tflt}
list1_last{n:pos} (xs: list1(INV(x), n)):<> (x)

impltmp
{x}(*tmp*)
list1_last(xs) = let
//
fun
loop
(
  xs: list1_1(x)
): (x) = let
  val+list1_cons(x, xs) = xs
in
  case+ xs of
  | list1_cons _ => loop(xs) | list1_nil _ => x
end // end of [loop]
//
in
  $effmask_all(loop(xs))
end // end of [list_last]


(* ****** ****** *)

impltmp
print$val<sort0> x = print_sort0 x
impltmp
print$val<s0exp> x = print_s0exp x
impltmp
print$val<d0exp> x = print_d0exp x

(* ****** ****** *)
//
extern
fun
p_idint: parser(token)
extern
fun
p_idintseq: parser(tokenlst)
//
implement
p_idint
  (buf, err) = let
//
val tok = buf.get0()
val tnd = tok.node()
//
in
//
case+ tnd of
//
| T_INT1 _ =>
  (buf.incby1(); tok)
//
| T_IDENT_alp _ =>
  (buf.incby1(); tok)
| T_IDENT_sym _ =>
  (buf.incby1(); tok)
//
| T_LT() => tok where
  {
    val () = buf.incby1()
    val loc = tok.loc((*void*))
    val tnd = T_IDENT_LT(*void*)
    val tok = token_make_node(loc, tnd)
  }
| T_GT() => tok where
  {
    val () = buf.incby1()
    val loc = tok.loc((*void*))
    val tnd = T_IDENT_GT(*void*)
    val tok = token_make_node(loc, tnd)
  }
//
| T_EQGT() => tok where
  {
    val () = buf.incby1()
    val loc = tok.loc((*void*))
    val tnd = T_IDENT_EQGT(*void*)
    val tok = token_make_node(loc, tnd)
  }
//
| _ (* non-IDENT *) =>
  (err := err + 1; tok)
//
end // end of [p_idint]
implement
p_idintseq
  (buf, err) =
(
//
list1_vt2t
(pstar_fun{token}(buf, err, p_idint))
//
) (* end of [p_idintseq] *)
//
(* ****** ****** *)

implement
t_d0pid(tnd) =
(
case+ tnd of
//
| T_IDENT_alp _ => true
| T_IDENT_sym _ => true
//
| T_AT((*void*)) => true // "@"
//
| T_BSLASH((*void*)) => true
//
| _ (* non-identifier *) => false
)

implement
p_d0pid(buf, err) =
let
//
val tok = buf.get0()
//
in
  case+
  tok.node() of
//
  | T_IDENT_alp _ =>
    i0dnt_some(tok) where
    {
      val () = buf.incby1()
    }
  | T_IDENT_sym _ =>
    i0dnt_some(tok) where
    {
      val () = buf.incby1()
    }
//
  | T_AT((*void*)) =>
    i0dnt_some(tok) where
    {
      val () = buf.incby1()
      val loc = tok.loc((*void*))
      val tnd = T_IDENT_AT(*void*)
      val tok = token_make_node(loc, tnd)
    }
//
  | T_BSLASH((*void*)) =>
    i0dnt_some(tok) where
    {
      val () = buf.incby1()
    }
//
  | _ (* non-identifier *) =>
    (err := err + 1; i0dnt_none(tok))
end // end of [p_d0pid]

(* ****** ****** *)

implement
t_d0eid(tnd) =
(
case+ tnd of
//
| T_IDENT_alp _ => true
| T_IDENT_sym _ => true
//
| T_IDENT_dlr _ => true
(*
| T_IDENT_srp _ => true
*)
//
| T_AT((*void*)) => true // "@"
| T_EQ((*void*)) => true // "="
//
(*
| T_LT((*void*)) => true // "<"
| T_GT((*void*)) => true // ">"
*)
//
| T_EQGT((*void*)) => true // ">"
//
| T_BSLASH((*void*)) => true
//
| _ (* non-identifier *) => false
//
) (* end of [t_d0eid] *)

implement
p_d0eid(buf, err) =
let
//
val tok = buf.get0()
//
in
  case+
  tok.node() of
  | T_IDENT_alp _ =>
    i0dnt_some(tok) where
    {
      val () = buf.incby1()
    }
  | T_IDENT_sym _ =>
    i0dnt_some(tok) where
    {
      val () = buf.incby1()
    }
//
  | T_IDENT_dlr _ =>
    i0dnt_some(tok) where
    {
      val () = buf.incby1()
    }
(*
  | T_IDENT_srp _ =>
    i0dnt_some(tok) where
    {
      val () = buf.incby1()
    }
*)
//
  | T_AT() =>
    i0dnt_some(tok) where
    {
      val () = buf.incby1()
      val loc = tok.loc((*void*))
      val tnd = T_IDENT_AT(*void*)
      val tok = token_make_node(loc, tnd)
    }
//
  | T_EQ() =>
    i0dnt_some(tok) where
    {
      val () = buf.incby1()
      val loc = tok.loc((*void*))
      val tnd = T_IDENT_EQ(*void*)
      val tok = token_make_node(loc, tnd)
    }
//
(*
  | T_LT() =>
    i0dnt_some(tok) where
    {
      val () = buf.incby1()
      val loc = tok.loc((*void*))
      val tnd = T_IDENT_LT(*void*)
      val tok = token_make_node(loc, tnd)
    }
  | T_GT() =>
    i0dnt_some(tok) where
    {
      val () = buf.incby1()
      val loc = tok.loc((*void*))
      val tnd = T_IDENT_GT(*void*)
      val tok = token_make_node(loc, tnd)
    }
*)
//
  | T_EQGT() =>
    i0dnt_some(tok) where
    {
      val () = buf.incby1()
      val loc = tok.loc((*void*))
      val tnd = T_IDENT_EQGT(*void*)
      val tok = token_make_node(loc, tnd)
    }
//
  | T_BSLASH((*void*)) =>
    i0dnt_some(tok) where
    {
      val () = buf.incby1()
    }
//
  | _ (* non-identifier *) =>
    (err := err + 1; i0dnt_none(tok))
end // end of [p_d0eid]

(* ****** ****** *)
//
implement
p_dq0eid
  (buf, err) = let
//
  val e0 = err
  val tok = buf.get0()
//
in
//
case+
tok.node() of
| T_IDENT_qual _ => let
    val () = buf.incby1()
  in
    err := e0;
    DQ0EIDsome
      (tok, p_d0eid(buf, err))
    // DQ0EIDsome
  end // end of [T_IDENT_qual]
//
| _(*non-IDENT_qual*) =>
    DQ0EIDnone(p_d0eid(buf, err))
//
end // end of [p_dq0eid]
//
(* ****** ****** *)

implement
p_i0dnt(buf, err) =
let
//
  val e0 = err
  val tok = buf.get0()
//
in
//
case+
tok.node() of
//
| tnd
  when
  t_s0eid(tnd) =>
    p_s0eid(buf, err)
//
| tnd
  when
  t_d0eid(tnd) =>
    p_d0eid(buf, err)
//
| _ (* non-i0dnt *) =>
    (err := e0 + 1; i0dnt_none(tok))
//
end // end of [p_i0dnt]

(* ****** ****** *)
//
extern
fun
p_i0dntseq: parser(i0dntlst)
extern
fun
p_i0dntseq_COMMA: parser(i0dntlst)
//
implement
p_i0dntseq
  (buf, err) =
(
//
list1_vt2t
(pstar_fun{i0dnt}(buf, err, p_i0dnt))
//
) (* end of [p_i0dntseq] *)
implement
p_i0dntseq_COMMA
  (buf, err) =
(
//
list1_vt2t
(pstar_COMMA_fun{i0dnt}(buf, err, p_i0dnt))
//
) (* end of [p_i0dntseq_COMMA] *)
//
(* ****** ****** *)
//
extern
fun
p_q0arg: parser(q0arg)
extern
fun
p_q0argseq_COMMA: parser(q0arglst)
//
(* ****** ****** *)
//
implement
p_q0arg
(buf, err) = let
//
val e0 = err
//
val sid =
  p_s0aid(buf, err)
//
val tok = buf.get0()
//
val opt =
(
case+
tok.node() of
| T_CLN() =>
  optn1_some
  (
   p_appsort0_NGT(buf, err)
  ) where
  {
    val () = buf.incby1()
  }
| _(* non-COLON *) => optn1_none()
) : sort0opt // end of [val]
//
val loc0 = let
  val loc = sid.loc()
in
  case+ opt of
  | optn1_none() => loc
  | optn1_some(s0t) => loc+s0t.loc()
end : loc_t // end of [val]
//
in
  err := e0;
  q0arg_make_node(loc0, Q0ARGsome(sid, opt))
end (* end of [p_q0arg] *)

(* ****** ****** *)
//
implement
p_q0argseq_COMMA
  (buf, err) =
(
  list1_vt2t
  (pstar_COMMA_fun{q0arg}(buf, err, p_q0arg))
) (* end of [p_q0argseq_COMMA] *)
//
(* ****** ****** *)
//
extern
fun
p_sq0arg: parser(sq0arg)
extern
fun
p_sq0argseq: parser(sq0arglst)
//
implement
p_sq0arg
  (buf, err) = let
//
val e0 = err
val tok = buf.get0()
//
(*
val () =
println! ("p_sq0arg: tok = ", tok)
*)
//
in
case+
tok.node() of
| T_LBRACE() => let
    val () = buf.incby1()
    val q0as =
      p_q0argseq_COMMA(buf, err)
    // end of [val]
    val tbeg = tok
    val tend = p_RBRACE(buf, err)
    val loc_res = tbeg.loc() + tend.loc()
  in
    err := e0;
    sq0arg_make_node
    (loc_res, SQ0ARGsome(tbeg, q0as, tend))
  end // end of [T_LBRACE]
| _(* non-LBRACE *) =>
  ( err := e0 + 1;
    sq0arg_make_node(tok.loc(), SQ0ARGnone(tok))
  ) (* end of [non-LBRACE] *)
//
end // end of [p_sq0arg]
//
implement
p_sq0argseq
  (buf, err) =
(
  list1_vt2t
  (pstar_fun{sq0arg}(buf, err, p_sq0arg))
) (* end of [p_sq0argseq] *)
//
(* ****** ****** *)
//
extern
fun
p_tq0arg: parser(tq0arg)
extern
fun
p_tq0argseq: parser(tq0arglst)
//
implement
p_tq0arg
  (buf, err) = let
//
val e0 = err
val tok = buf.get0()
//
(*
val () =
println! ("p_tq0arg: tok = ", tok)
*)
//
in
//
case+
tok.node() of
| T_LT() => let
    val () = buf.incby1()
    val q0as =
      p_q0argseq_COMMA(buf, err)
    // end of [val]
    val tbeg = tok
    val tend = p_GT(buf, err)
    val loc_res = tbeg.loc() + tend.loc()
  in
    err := e0;
    tq0arg_make_node
    (loc_res, TQ0ARGsome(tbeg, q0as, tend))
  end
| T_LTGT() => let
    val () = buf.incby1()
    val q0as = list1_nil(*void*)
    val tbeg = tok
    val tend = tok
    val loc_res = tok.loc()
  in
    tq0arg_make_node
    (loc_res, TQ0ARGsome(tbeg, q0as, tend))
  end
| _(* non-LT/GT *) =>
  ( err := e0 + 1;
    tq0arg_make_node(tok.loc(), TQ0ARGnone(tok))
  ) (* end of [non-LT/GT] *)
//
end // end of [p_tq0arg]
//
implement
p_tq0argseq
  (buf, err) =
(
  list1_vt2t
  (pstar_fun{tq0arg}(buf, err, p_tq0arg))
) (* end of [p_tq0argseq] *)
//
(* ****** ****** *)
//
extern
fun
p_ti0arg: parser(ti0arg)
extern
fun
p_ti0argseq: parser(ti0arglst)
//
implement
p_ti0arg
  (buf, err) = let
//
val e0 = err
val tok = buf.get0()
//
(*
val () =
println! ("p_ti0arg: tok = ", tok)
*)
//
in
//
case+
tok.node() of
//
| T_LT() => let
    val () = buf.incby1()
    val s0es =
    list1_vt2t
    (
      pstar_COMMA_fun
      {s0exp}(buf, err, p_apps0exp_NGT)
    )
    val tbeg = tok
    val tend = p_GT(buf, err)
    val loc_res = tbeg.loc() + tend.loc()
  in
    err := e0;
    ti0arg_make_node
    (loc_res, TI0ARGsome(tbeg, s0es, tend))
  end // end of [T_LT]
| _(* non-LT/GT *) =>
  ( err := e0 + 1;
    ti0arg_make_node(tok.loc(), TI0ARGnone(tok))
  ) (* end of [non-LT/GT] *)
//
end // end of [p_ti0arg]
//
implement
p_ti0argseq
  (buf, err) =
(
  list1_vt2t
  (pstar_fun{ti0arg}(buf, err, p_ti0arg))
) (* end of [p_ti0argseq] *)
//
(* ****** ****** *)
(*
//
a0typ ::=
  | token
  | d0pid COLON s0exp
//
*)
extern
fun
p_a0typ: parser(a0typ)
extern
fun
p_a0typseq_COMMA: parser(a0typlst)
extern
fun
p_a0typseqopt_COMMA: parser(a0typlstopt)
//
(* ****** ****** *)
//
implement
p_a0typ
  (buf, err) = let
//
val e0 = err
//
val mark =
  buf.get_mark()
//
val tok0 = buf.get1()
val tok1 = buf.get0()
//
in
//
case+
tok1.node() of
| T_CLN() => let
    val () =
    buf.clear_mark(mark)
    val () = buf.incby1()
    val s0e = p_s0exp(buf, err)
    val loc_res = tok0.loc() + s0e.loc()
  in
    err := e0;
    a0typ_make_node
    (loc_res, A0TYPsome(s0e, optn1_some(tok0)))
  end // end of [T_CLN]
| _(*non-COLON*) => let
    val () =
      buf.set_mark(mark)
    // end of [val]
    val s0e = p_s0exp(buf, err)
  in
    err := e0;
    a0typ_make_node
    (s0e.loc(), A0TYPsome(s0e, optn1_none(*void*)))
  end // end of [non-COLON]
//
end // end of [p_a0typ]

(* ****** ****** *)
//
implement
p_a0typseq_COMMA
  (buf, err) =
(
//
list1_vt2t
(pstar_COMMA_fun
 {a0typ}(buf, err, p_a0typ))
//
)
implement
p_a0typseqopt_COMMA
  (buf, err) = let
//
val tok = buf.get0()
//
in (* in-of-let *)
//
case+
tok.node() of
| T_BAR() => let
    val () = buf.incby1()
  in
    optn1_some(p_a0typseq_COMMA(buf, err))
  end // end of [T_BAR]
| _(* non-BAR *) => optn1_none(*void*)
//
end // end of [p_a0typseqopt]
//
(* ****** ****** *)

implement
p_d0arg
  (buf, err) = let
//
val e0 = err
val tok = buf.get0()
val tnd = tok.node()
//
in
//
case+ tnd of
//
| T_LPAREN() => let
    val () = buf.incby1()
    val arg0 =
      p_a0typseq_COMMA(buf, err)
    val opt1 =
      p_a0typseqopt_COMMA(buf, err)
    val tbeg = tok
    val tend = p_RPAREN(buf, err)
    val loc_res = tbeg.loc() + tend.loc()
  in
    err := 0;
    d0arg_make_node
    ( loc_res
    , D0ARGsome_dyn2(tbeg, arg0, opt1, tend))
  end
//
| T_LBRACE() => let
    val () = buf.incby1()
    val s0qs =
      p_s0quaseq_BARSMCLN(buf, err)
    val tbeg = tok
    val tend = p_RBRACE(buf, err)
    val loc_res = tbeg.loc() + tend.loc()
  in
    err := e0;
    d0arg_make_node
    (loc_res, D0ARGsome_sta(tbeg, s0qs, tend))
  end // end of [T_LBRACE]
//
| _ when
    t_s0eid(tnd) => let
    val sid =
      p_s0eid(buf, err)
    // end of [val]
    val loc = sid.loc()
  in
    err := e0;
    d0arg_make_node(loc, D0ARGsome_dyn1(sid))
  end
//
| _ (* error *) =>
  (
    err := e0 + 1;
    d0arg_make_node(tok.loc(), D0ARGnone(tok))
  )
//
end // end of [p_d0arg]

(* ****** ****** *)

extern
fun
p_d0argseq: parser(d0arglst)
implement
p_d0argseq
  (buf, err) =
(
  list1_vt2t
  (pstar_fun{d0arg}(buf, err, p_d0arg))
) (* end of [p_d0argseq] *)

(* ****** ****** *)
(*
atmd0pat::
//
  | d0pid
//
  | t0int // int
  | t0chr // char
  | t0flt // float
  | t0str // string
//
*)
//
extern
fun
p_atmd0pat: parser(d0pat)
extern
fun
p_atmd0patseq: parser(d0patlst)
//
(* ****** ****** *)
//
extern
fun
p_d0patseq_COMMA: parser(d0patlst)
extern
fun
p_labd0patseq_COMMA: parser(labd0patlst)
//
(* ****** ****** *)
//
(*
d0pat_RPAREN ::=
  | RPAREN
  | BAR d0patseq_COMMA RPAREN
labd0pat_RBRACE ::=
  | RPAREN
  | BAR labd0patseq_COMMA RBRACE
*)
extern
fun
p_d0pat_RPAREN: parser(d0pat_RPAREN)
extern
fun
p_labd0pat_RBRACE: parser(labd0pat_RBRACE)
//
(* ****** ****** *)

local

static
fun
p_napps: parser(d0pat)
implement
p_napps(buf, err) = let
//
  val e0 = err
  val tok = buf.get0()
  val tnd = tok.node()
//
in
//
case+ tnd of
| _ (* error *) =>
  ( err := e0 + 1;
    d0pat_make_node(tok.loc(), D0Pnone(tok))
  ) (* end-of-error *)
//
end // end of [p_napps]

in (* in-of-local *)

implement
p_d0pat(buf, err) =
let
  val e0 = err
  val d0ps0 =
  p_atmd0patseq(buf, err)
in
//
case+ d0ps0 of
| list1_nil
    ((*void*)) => p_napps(buf, err)
  // end of [list_nil]
| list1_cons
    (d0p0, d0ps1) => let
    val opt =
    popt_s0exp_anno(buf, err)
  in
    case+ d0ps1 of
    | list1_nil() =>
      d0pat_anno_opt(d0p0, opt)
    | list1_cons _ => let
        val d0p1 = list1_last(d0ps1)
        val loc01 = d0p0.loc()+d0p1.loc()
      in
        d0pat_anno_opt
        (d0pat_make_node(loc01, D0Papps(d0ps0)), opt)
      end // end of [list_cons]
  end (* end of [list_cons] *)
//
end // end of [let] // end of [p_d0pat]

end // end of [local]

(* ****** ****** *)

implement
p_labd0pat
  (buf, err) = let
//
val e0 = err
//
val l0 =
(
  p_l0abl(buf, err)
)
val tok = p_EQ(buf, err)
val d0p = p_d0pat(buf, err)
//
(*
val ((*void*)) =
println! ("p_labd0pat: l0 = ", l0)
val ((*void*)) =
println! ("p_labd0pat: tok = ", tok)
val ((*void*)) =
println! ("p_labd0pat: d0p = ", d0p)
*)
//
in
  err := e0; DL0ABELED(l0, tok, d0p)
end // end of [p_labd0pat]

(* ****** ****** *)

implement
p_atmd0pat
(buf, err) = let
//
val e0 = err
val tok = buf.get0()
val tnd = tok.node()
//
in
//
case+ tnd of
//
| _ when t_d0pid(tnd) =>
  let
    val id = p_d0pid(buf, err)
  in
    err := e0;
    d0pat_make_node(id.loc(), D0Pid(id))
  end // end of [t_d0pid]
//
| _ when t_t0int(tnd) =>
  let
    val i0 = p_t0int(buf, err)
  in
    err := e0;
    d0pat_make_node(i0.loc(), D0Pint(i0))
  end // end of [t_t0int]
| _ when t_t0chr(tnd) =>
  let
    val c0 = p_t0chr(buf, err)
  in
    err := e0;
    d0pat_make_node(c0.loc(), D0Pchr(c0))
  end // end of [t_t0chr]
| _ when t_t0flt(tnd) =>
  let
    val c0 = p_t0flt(buf, err)
  in
    err := e0;
    d0pat_make_node(c0.loc(), D0Pflt(c0))
  end // end of [t_t0flt]
| _ when t_t0str(tnd) =>
  let
    val c0 = p_t0str(buf, err)
  in
    err := e0;
    d0pat_make_node(c0.loc(), D0Pstr(c0))
  end // end of [t_t0str]
//
| T_LBRACE() => let
    val () = buf.incby1()
    val s0as =
    p_s0argseq_COMMA(buf, err)
    val tbeg = tok
    val tend = p_RBRACE(buf, err)
    val loc_res = tbeg.loc()+tend.loc()
  in
    err := e0;
    d0pat_make_node
    (loc_res, D0Psqarg(tbeg, s0as, tend))
  end // end of [T_LBRACE]
//
| T_LPAREN() => let
    val () = buf.incby1()
    val d0ps =
      p_d0patseq_COMMA(buf, err)
    // end of [val]
    val tbeg = tok
    val tend = p_d0pat_RPAREN(buf, err)
  in
    err := e0;
    d0pat_make_node
    ( loc_res
    , D0Pparen(tbeg, d0ps, tend)) where
    {
      val loc_res =
        tbeg.loc()+d0pat_RPAREN_loc(tend)
      // end of [val]
    }
  end // end of [T_LPAREN]
//
| T_TUPLE(k0) => let
    val () = buf.incby1()
    val topt =
    ( if
      (k0 <= 1)
      then optn1_none()
      else optn1_some(p_LPAREN(buf, err))
    ) : tokenopt // end of [val]
    val d0ps =
      p_d0patseq_COMMA(buf, err)
    // end of [val]
    val tbeg = tok
    val tend = p_d0pat_RPAREN(buf, err)
  in
    err := e0;
    d0pat_make_node
    ( loc_res
    , D0Ptuple
      (tbeg, topt, d0ps, tend)) where
    {
      val loc_res =
        tbeg.loc()+d0pat_RPAREN_loc(tend)
      // end of [val]
    }
  end // end of [T_TUPLE]
//
| T_IDENT_qual _ => let
    val () = buf.incby1()
    val d0p0 = p_atmd0pat(buf, err)
  in
    err := e0;
    d0pat_make_node
    (loc_res, D0Pqual(tok, d0p0)) where
    {
      val loc_res = tok.loc()+d0p0.loc()
    }
  end // end of [T_IDENT_qual]
//
| _ (* error *) => let
    val () = (err := e0 + 1)
  in
    d0pat_make_node(tok.loc(), D0Pnone(tok))
  end // HX: indicating a parsing error
//
end // end of [p_atmd0pat]

(* ****** ****** *)

implement
p_atmd0patseq
  (buf, err) =
(
//
list1_vt2t
(pstar_fun{d0pat}(buf, err, p_atmd0pat))
//
) (* end of [p_atmd0patseq] *)

(* ****** ****** *)
//
implement
p_d0patseq_COMMA
  (buf, err) =
(
  list1_vt2t
  (pstar_COMMA_fun{d0pat}(buf, err, p_d0pat))
) (* end of [p_d0patseq_COMMA] *)

implement
p_labd0patseq_COMMA
  (buf, err) =
(
  list1_vt2t
  (pstar_COMMA_fun{labd0pat}(buf, err, p_labd0pat))
) (* end of [p_labd0patseq_COMMA] *)
//
(* ****** ****** *)

implement
p_d0pat_RPAREN
  (buf, err) = let
  val e0 = err
  val tok1 = buf.get0()
  val tnd1 = tok1.node()
in
//
case+ tnd1 of
| T_BAR() => let
    val () = buf.incby1()
    val d0ps =
      p_d0patseq_COMMA(buf, err)
    val tok2 = p_RPAREN(buf, err)
  in
    err := e0;
    d0pat_RPAREN_cons1(tok1, d0ps, tok2)
  end // end of [T_BAR]
| _ (* non-BAR *) =>
  (
    case+ tnd1 of
    | T_RPAREN() => let
        val () = buf.incby1()
      in
        err := e0; d0pat_RPAREN_cons0(tok1)
      end // end of [RPAREN]
    | _(*non-RPAREN*) =>
      (
        err := e0 + 1; d0pat_RPAREN_cons0(tok1)
      ) (* end of [non-RPAREN *)
  )
//
end // end of [p_d0pat_RPAREN]

implement
p_labd0pat_RBRACE
  (buf, err) = let
  val e0 = err
  val tok1 = buf.get0()
  val tnd1 = tok1.node()
in
//
case+ tnd1 of
| T_BAR() => let
    val () = buf.incby1()
    val ld0ps =
    p_labd0patseq_COMMA(buf, err)
    val tok2 = p_RBRACE(buf, err)
  in
    err := e0;
    labd0pat_RBRACE_cons1(tok1, ld0ps, tok2)
  end // end of [T_BAR]
| _ (* non-BAR *) =>
  (
    case+ tnd1 of
    | T_RBRACE() => let
        val () = buf.incby1()
      in
        err := e0; labd0pat_RBRACE_cons0(tok1)
      end // end of [RBRACE]
    | _(*non-RPAREN*) =>
      (
        err := e0 + 1; labd0pat_RBRACE_cons0(tok1)
      ) (* end of [non-RPAREN] *)
  )
//
end // end of [p_labd0pat_RBRACE]

(* ****** ****** *)

implement
p_f0arg
  (buf, err) = let
//
val e0 = err
val tok = buf.get0()
val tnd = tok.node()
//
in
//
case+ tnd of
//
| T_DOTLT(_) => let
    val () = buf.incby1()
    val s0es =
    p_s0expseq_COMMA(buf, err)
    val tbeg = tok
    val tend = p_GTDOT(buf, err)
    val loc_res = tbeg.loc() + tend.loc()
  in
    err := e0;
    f0arg_make_node
    (loc_res, F0ARGsome_met(tbeg, s0es, tend))
  end // end of [T_DOTLT]
//
| T_LBRACE() => let
    val () = buf.incby1()
    val s0qs =
    p_s0quaseq_BARSMCLN(buf, err)
    val tbeg = tok
    val tend = p_RBRACE(buf, err)
    val loc_res = tbeg.loc() + tend.loc()
  in
    err := e0;
    f0arg_make_node
    (loc_res, F0ARGsome_sta(tbeg, s0qs, tend))
  end // end of [T_LBRACE]
//
| _(*non-sta-met*) =>
  let
    val d0p = p_atmd0pat(buf, err)
  in
    f0arg_make_node(d0p.loc(), F0ARGsome_dyn(d0p))
  end
//
end // end of [p_f0arg]

(* ****** ****** *)

extern
fun
p_f0argseq: parser(f0arglst)
extern
fun
p_f0unarrow: parser(f0unarrow)

(* ****** ****** *)

implement
p_f0argseq
  (buf, err) =
(
  list1_vt2t
  (pstar_fun{f0arg}(buf, err, p_f0arg))
) (* end of [p_f0argseq] *)

(* ****** ****** *)
//
implement
p_f0unarrow
  (buf, err) = let
//
val e0 = err
//
val tok = buf.get0()
val tnd = tok.node()
//
in
//
case+ tnd of
//
| T_EQGT() => let
    val () =
      buf.incby1()
    // end of [val]
  in
    F0UNARROWdflt(tok)
  end // end of [T_EQGT]
//
| T_EQLT() => let
    val () =
      buf.incby1()
    // end of [val]
    val s0es =
    list1_vt2t
    (
      pstar_COMMA_fun
      {s0exp}(buf, err, p_apps0exp_NGT)
    )
    val tbeg = tok
    val tend = p_GT(buf, err)
  in
    err := e0;
    F0UNARROWlist(tbeg, s0es, tend)
  end // end of [T_EQLT]
//
| _(*non-arrow*) => F0UNARROWnone(tok)
//
end // end of [p_f0unarrow]

(* ****** ****** *)
(*
//
atmd0exp ::
//
  | d0eid
//
  | t0int // int
  | t0chr // char
  | t0flt // float
  | t0str // string
//
  | qualid atm0exp
//
  | { d0eclseq }
  | LET d0eclseq IN d0expseq END
//
  | ( d0expseq_COMMA )
  | ( d0expseq_COMMA | d0expseq_COMMA )
  | ( d0expseq_COMMA ; d0expseq_SMCLN )
//
  | { labd0expseq_COMMA }
  | { labd0expseq_COMMA | labd0expseq_COMMA }
//
*)
extern
fun
p_atmd0exp: parser(d0exp)
//
extern
fun
p_appd0exp: parser(d0exp)
//
extern
fun
p_atmd0expseq: parser(d0explst)
//
(* ****** ****** *)
//
extern
fun
p_d0expseq_COMMA: parser(d0explst)
extern
fun
p_labd0expseq_COMMA: parser(labd0explst)
//
(* ****** ****** *)
//
extern
fun
p_d0expseq_SMCLN: parser(d0explst)
//
(* ****** ****** *)
//
(*
d0exp_RPAREN ::=
  | RPAREN
  | BAR d0expseq_COMMA RPAREN
  | SMCLN d0expseq_SMCLN RPAREN
*)
(*
labd0exp_RBRACE ::=
  | RPAREN
  | BAR labd0expseq_COMMA RBRACE
*)
extern
fun
p_d0exp_RPAREN: parser(d0exp_RPAREN)
extern
fun
p_labd0exp_RBRACE: parser(labd0exp_RBRACE)
//
(* ****** ****** *)
//
extern
fun
p_d0exp_THEN: parser(d0exp_THEN)
and
p_d0exp_ELSE: parser(d0exp_ELSE)
//
(* ****** ****** *)
//
extern
fun
p_ENDWHERE: parser(endwhere)
//
extern
fun
pseq_d0eclseq_WHERE:
  parser(List0(d0eclseq_WHERE))
//
(*
extern
fun
popt_d0eclseq_WHERE:
  parser(Option(d0eclseq_WHERE))
*)
//
(* ****** ****** *)
//
extern
fun
p_d0clau: parser(d0clau)
extern
fun
p_d0clauseq_BAR: parser(d0claulst)
//
(* ****** ****** *)

local

static
fun
p_napps: parser(d0exp)
implement
p_napps(buf, err) = let
//
  val e0 = err
  val tok = buf.get0()
  val tnd = tok.node()
//
in
//
case+ tnd of
//
| T_IF() => let
//
    val () = buf.incby1()
//
    val d0e1 =
      p_appd0exp(buf, err)
    val d0e2 =
      p_d0exp_THEN(buf, err)
    val d0e3 =
      p_d0exp_ELSE(buf, err)
    val topt = popt_ENDIF(buf, err)
//
    val
    loc_res =
    (
    case+ topt of
    | optn1_none() =>
      (
      case d0e3 of
      | d0exp_ELSEnone
          () =>
        (
          case+ d0e2 of
          | d0exp_THEN
              (_, d0e) =>
              tok.loc() + d0e.loc()
            // end of [d0exp_THEN]
        )
      | d0exp_ELSEsome
          (_, d0e) =>
          tok.loc() + d0e.loc()
        // end of [d0exp_ELSEsome]
      )
    | optn1_some(tok2) => tok.loc() + tok2.loc()
    ) : loc_t // end of [val]
//
  in
    err := e0;
    d0exp_make_node
      (loc_res, D0Eif0(tok, d0e1, d0e2, d0e3, topt))
    // d0exp_make_node
  end // end of [T_IF]
//
| T_CASE _ => let
//
    val () = buf.incby1()
//
    val d0e1 =
      p_appd0exp(buf, err)
//
    val tok2 = p_OF(buf, err)
    val tbar = popt_BAR(buf, err)
    val d0cs = p_d0clauseq_BAR(buf, err)
    val tend = popt_ENDCASE(buf, err)
//
    val loc_res = let
      val loc = tok.loc()
    in
      case+ tend of
      | optn1_none() =>
        (
        case+ d0cs of
        | list1_nil() =>
          (
            case+ tbar of
            | optn1_none() => loc + tok2.loc()
            | optn1_some(tok) => loc + tok.loc()
          )
        | list1_cons(_, _) =>
          let
            val d0c =
              list1_last(d0cs) in loc + d0c.loc()
            // end of [val]
          end // end of [list1_cons]
        )
      | optn1_some(tok) => loc + tok.loc()
    end : loc_t // end of [let] // end of [val]
//
  in
    err := e0;
    d0exp_make_node
    ( loc_res
    , D0Ecase(tok, d0e1, tok2, tbar, d0cs, tend))
    // end of [d0exp_make_node]
  end // end of [T_CASE]
//
| T_LAM(k0) => let
    val () = buf.incby1()
    val arg =
      p_f0argseq(buf, err)
    val res =
      p_effs0expopt(buf, err)
    val farrw =
      p_f0unarrow(buf, err)
    val fbody = p_d0exp(buf, err)
    val tfini = popt_ENDLAM(buf, err)
  in
    err := e0;
    d0exp_make_node
    ( loc_res
    , D0Elam
      (tok, arg, res, farrw, fbody, tfini)
    ) where
    {
      val loc_res =
      (
        case+ tfini of
        | optn1_none() => tok.loc()+fbody.loc()
        | optn1_some(tok2) => tok.loc()+tok2.loc()
      ) : loc_t // end of [val]
    }
  end
//
| _ (* error *) =>
  ( err := e0 + 1;
    d0exp_make_node(tok.loc(), D0Enone(tok))
  ) (* end-of-error *)
//
end // end of [p_napps]

fun
auxlst_where
( d0e0
: d0exp
, wd0cs
: List0(d0eclseq_WHERE)
) : d0exp =
(
case+ wd0cs of
| list1_nil
  ((*void*)) => d0e0
| list1_cons
  (wd0c0, wd0cs) => let
    val d0e1 =
    (
    d0exp_make_node
    (loc1, D0Ewhere(d0e0, wd0c0))
    ) where
    {
      val loc1 =
      (
        case+ wd0c0 of
        | d0eclseq_WHERE
          (_, _, _, tend) => d0e0.loc()+tend.loc()
      ) : loc_t // end of [val]
    } (* end of [where] *)
  in
    auxlst_where(d0e1, wd0cs)
  end // end of [list1_cons]
) (* end of [auxlst_where] *)

in (* in-of-local *)

implement
p_d0exp(buf, err) =
let
//
val e0 = err
val d0es =
  p_atmd0expseq(buf, err)
// end of [val]
//
(*
val () =
println!("p_d0exp: d0es = ", d0es)
*)
//
in
//
case+ d0es of
| list1_nil
    ((*void*)) =>
    p_napps(buf, err)
  // end of [list1_nil]
| list1_cons
    (d0e1, d0es2) =>
  (
  let
    val d0e0 =
    auxlst_where(d0e0, wd0cs)
    val opt =
    popt_s0exp_anno(buf, err)
  in
    d0exp_anno_opt(d0e0, opt)
  end
  ) where
  {
    val d0e0 =
    (
    case+ d0es2 of
    | list1_nil() => d0e1
    | list1_cons _ => let
        val d0e2 = list1_last(d0es2)
        val loc0 = d0e1.loc()+d0e2.loc()
      in
        d0exp_make_node(loc0, D0Eapps(d0es))
      end // end of [list1_cons]
    ) : d0exp // end of [val]
    val wd0cs = pseq_d0eclseq_WHERE(buf, err)
  } (* end of [list1_cons] *)
//
end // end of [p_d0exp]

end // end of [local]

(* ****** ****** *)

implement
p_labd0exp
  (buf, err) = let
//
val e0 = err
//
val l0 =
(
  p_l0abl(buf, err)
)
val tok = p_EQ(buf, err)
val d0e = p_d0exp(buf, err)
//
(*
val ((*void*)) =
println! ("p_labd0exp: l0 = ", l0)
val ((*void*)) =
println! ("p_labd0exp: tok = ", tok)
val ((*void*)) =
println! ("p_labd0exp: d0e = ", d0e)
*)
//
in
  err := e0; DL0ABELED(l0, tok, d0e)
end // end of [p_labd0exp]

(* ****** ****** *)

implement
p_atmd0exp
(buf, err) = let
//
val e0 = err
val tok = buf.get0()
val tnd = tok.node()
//
in
//
case+ tnd of
//
| _ when t_d0eid(tnd) =>
  let
    val id = p_d0eid(buf, err)
  in
    err := e0;
    d0exp_make_node(id.loc(), D0Eid(id))
  end // end of [t_d0eid]
//
| _ when t_t0int(tnd) =>
  let
    val i0 = p_t0int(buf, err)
  in
    err := e0;
    d0exp_make_node(i0.loc(), D0Eint(i0))
  end // end of [t_t0int]
| _ when t_t0chr(tnd) =>
  let
    val c0 = p_t0chr(buf, err)
  in
    err := e0;
    d0exp_make_node(c0.loc(), D0Echr(c0))
  end // end of [t_t0chr]
| _ when t_t0flt(tnd) =>
  let
    val f0 = p_t0flt(buf, err)
  in
    err := e0;
    d0exp_make_node(f0.loc(), D0Eflt(f0))
  end // end of [t_t0flt]
| _ when t_t0str(tnd) =>
  let
    val s0 = p_t0str(buf, err)
  in
    err := e0;
    d0exp_make_node(s0.loc(), D0Estr(s0))
  end // end of [t_t0str]
//
| T_LT() => let
    val () =
      buf.incby1()
    val mark =
      buf.get_mark()
    // end of [val]
    val s0es =
    list1_vt2t
    (
      pstar_COMMA_fun
      {s0exp}(buf, err, p_apps0exp_NGT)
    )
    val tok2 = buf.get0()
  in
    case+
    tok2.node() of
    | T_GT() => let
        val () =
        buf.incby1()
        val () =
        buf.clear_mark(mark)
        val tbeg = tok
        val tend = tok2
        val loc_res = tok.loc() + tok2.loc()
      in
        d0exp_make_node
          (loc_res, D0Etqarg(tbeg, s0es, tend))
        // d0exp_make_node
      end // end of [T_GT]
    | _(* non-GT *) => let
        val () =
          buf.set_mark(mark)
        // end of [val]
        val loc = tok.loc()
        val tnd = T_IDENT_LT
        val tok = token_make_node(loc, tnd)
      in
        d0exp_make_node(loc, D0Eid(i0dnt_some(tok)))
      end // end of [non-GT]
  end // end of [T_LT]
//
| T_GT() => let
    val () =
      buf.incby1()
    // end of [val]
    val loc = tok.loc()
    val tnd = T_IDENT_GT
    val tok =
      token_make_node(loc, tnd)
    // end of [val]
  in
    d0exp_make_node
      (loc, D0Eid(i0dnt_some(tok)))
    // d0exp_make_node
  end // end of [T_GT]
//
| T_LTGT() => let
    val () =
      buf.incby1()
    // end of [val]
    val tbeg = tok
    val tend = tok
    val s0es = list1_nil()
    val loc_res = tok.loc()
  in
    d0exp_make_node
    (loc_res, D0Etqarg(tbeg, s0es, tend))
  end // end of [T_LTGT]
//
| T_LBRACE() => let
    val () = buf.incby1()
    val s0es =
      p_s0expseq_COMMA(buf, err)
    val tbeg = tok
    val tend = p_RBRACE(buf, err)
  in
    err := e0;
    d0exp_make_node
    ( loc_res
    , D0Esqarg(tbeg, s0es, tend)) where
    {
      val loc_res = tbeg.loc()+tend.loc()
    }
  end // end of [T_LBRACE]
//
| T_LPAREN() => let
    val () = buf.incby1()
    val d0es =
      p_d0expseq_COMMA(buf, err)
    // end of [val]
    val tbeg = tok
    val tend = p_d0exp_RPAREN(buf, err)
  in
    err := e0;
    d0exp_make_node
    ( loc_res
    , D0Eparen(tbeg, d0es, tend)) where
    {
      val loc_res =
        tbeg.loc()+d0exp_RPAREN_loc(tend)
      // end of [val]
    }
  end // end of [T_LPAREN]
//
| T_TUPLE(k0) => let
    val () = buf.incby1()
    val topt =
    ( if
      (k0 <= 1)
      then optn1_none()
      else optn1_some(p_LPAREN(buf, err))
    ) : tokenopt // end of [val]
    val d0es =
      p_d0expseq_COMMA(buf, err)
    // end of [val]
    val tbeg = tok
    val tend = p_d0exp_RPAREN(buf, err)
  in
    err := e0;
    d0exp_make_node
    ( loc_res
    , D0Etuple
      (tbeg, topt, d0es, tend)) where
    {
      val loc_res =
        tbeg.loc()+d0exp_RPAREN_loc(tend)
      // end of [val]
    }
  end // end of [T_TUPLE]
//
| T_LET() => let
//
    val () = buf.incby1()
//
    val d0cs =
    p_d0eclseq_dyn(buf, err)
//
    val tok1 = p_IN(buf, err)
//
    val d0es =
    p_d0expseq_SMCLN(buf, err)
//
    val tok2 = p_ENDLET(buf, err)
//
    val loc_res = tok.loc()+tok2.loc()
  in
    err := e0;
    d0exp_make_node
    ( loc_res
    , D0Elet(tok, d0cs, tok1, d0es, tok2))
  end // end of [T_LET]
//
| T_DOT() => let
    val () =
      buf.incby1()
    // end of [val]
    val lab =
      p_l0abl(buf, err)
    val arg = let
      val tok2 = buf.get0()
    in
      case+
      tok2.node() of
      | T_LPAREN() =>
        optn1_some( d0e ) where
        {
          val d0e =
            p_atmd0exp(buf, err)
          // end of [val]
        }
      | _(* non-LPAREN *) => optn1_none()
    end : d0expopt // end of [val]
    val loc_res = let
      val loc = tok.loc()
    in
      case+ arg of
      | optn1_none() => loc + lab.loc()
      | optn1_some(d0e) => loc + d0e.loc()
    end // end of [val]
  in
    d0exp_make_node
      (loc_res, D0Edtsel(tok, lab, arg))
    // d0exp_make_node
  end // end of [T_DOT]
//
| T_IDENT_qual _ => let
    val () = buf.incby1()
    val d0e = p_atmd0exp(buf, err)
  in
    err := e0;
    d0exp_make_node
    (loc_res, D0Equal(tok, d0e)) where
    {
      val loc_res = tok.loc()+d0e.loc()
    }
  end // end of [T_IDENT_qual]
//
| _ (* error *) => let
    val () = (err := e0 + 1)
  in
    d0exp_make_node(tok.loc(), D0Enone(tok))
  end // HX: indicating a parsing error
//
end // end of [p_atmd0exp]

(* ****** ****** *)

implement
p_appd0exp
  (buf, err) = let
//
val
d0e0 = p_atmd0exp(buf, err)
val
d0es = p_atmd0expseq(buf, err)
//
in
//
case+ d0es of
| list1_nil() => d0e0
| list1_cons _ => let
    val d0e1 = list1_last(d0es)
    val loc0 = d0e0.loc() + d0e1.loc()
  in
    d0exp_make_node
      (loc0, D0Eapps(list1_cons(d0e0, d0es)))
    // d0exp_make_node
  end // end of [list1_cons]
//
end // end of [p_appd0exp]

(* ****** ****** *)

implement
p_atmd0expseq
  (buf, err) =
(
//
list1_vt2t
(pstar_fun{d0exp}(buf, err, p_atmd0exp))
//
) (* end of [p_atmd0expseq] *)

(* ****** ****** *)

implement
p_d0expseq_COMMA
  (buf, err) =
(
//
list1_vt2t
(pstar_COMMA_fun{d0exp}(buf, err, p_d0exp))
//
) (* end of [p_d0expseq_COMMA] *)

implement
p_labd0expseq_COMMA
  (buf, err) =
(
//
list1_vt2t
(pstar_COMMA_fun
 {labd0exp}(buf, err, p_labd0exp))
//
) (* end of [p_labd0expseq_COMMA] *)

(* ****** ****** *)

implement
p_d0expseq_SMCLN
  (buf, err) =
(
//
list1_vt2t
(pstar_SMCLN_fun{d0exp}(buf, err, p_d0exp))
//
) (* end of [p_d0expseq_SMCLN] *)

(* ****** ****** *)

implement
p_d0exp_RPAREN
  (buf, err) = let
  val e0 = err
  val tok1 = buf.get0()
  val tnd1 = tok1.node()
in
//
case+ tnd1 of
| T_BAR() => let
    val () =
      buf.incby1()
    val d0es =
      p_d0expseq_COMMA
        (buf, err)
    val tok2 = p_RPAREN(buf, err)
  in
    err := e0;
    d0exp_RPAREN_cons1(tok1, d0es, tok2)
  end // end of [T_BAR]
| T_SMCLN() => let
    val () =
      buf.incby1()
    val d0es =
      p_d0expseq_SMCLN
        (buf, err)
    val tok2 = p_RPAREN(buf, err)
  in
    err := e0;
    d0exp_RPAREN_cons2(tok1, d0es, tok2)
  end // end of [T_BAR]
| _ (* non-BAR *) =>
  (
    case+ tnd1 of
    | T_RPAREN() => let
        val () = buf.incby1()
      in
        err := e0; d0exp_RPAREN_cons0(tok1)
      end // end of [RPAREN]
    | _(*non-RPAREN*) =>
      (
        err := e0 + 1; d0exp_RPAREN_cons0(tok1)
      ) (* end of [non-RPAREN *)
  )
//
end // end of [p_d0exp_RPAREN]

implement
p_labd0exp_RBRACE
  (buf, err) = let
  val e0 = err
  val tok1 = buf.get0()
  val tnd1 = tok1.node()
in
//
case+ tnd1 of
| T_BAR() => let
    val () = buf.incby1()
    val ld0es =
    p_labd0expseq_COMMA(buf, err)
    val tok2 = p_RBRACE(buf, err)
  in
    err := e0;
    labd0exp_RBRACE_cons1(tok1, ld0es, tok2)
  end // end of [T_BAR]
| _ (* non-BAR *) =>
  (
    case+ tnd1 of
    | T_RBRACE() => let
        val () = buf.incby1()
      in
        err := e0; labd0exp_RBRACE_cons0(tok1)
      end // end of [RBRACE]
    | _(*non-RPAREN*) =>
      (
        err := e0 + 1; labd0exp_RBRACE_cons0(tok1)
      ) (* end of [non-RPAREN] *)
  )
//
end // end of [p_labd0exp_RBRACE]

(* ****** ****** *)

implement
p_d0exp_THEN
  (buf, err) = let
//
val e0 = err
val tok = buf.get0()
//
in
//
case+
tok.node() of
| T_THEN() => let
    val () =
      buf.incby1()
    val d0e =
      p_d0exp(buf, err)
  in
    err := e0; d0exp_THEN(tok, d0e)
  end // end of [T_THEN]
| _(*non-THEN*) =>
  ( // HX-2018-09-25: error
    d0exp_THEN(tok, p_d0exp(buf, err))
  ) (* end of [non-THEN] *)
//
end // end of [p_d0exp_THEN]

(* ****** ****** *)

implement
p_d0exp_ELSE
  (buf, err) = let
//
val e0 = err
val tok = buf.get0()
//
in
//
case+
tok.node() of
| T_ELSE() => let
    val () =
      buf.incby1()
    val d0e =
      p_d0exp(buf, err)
  in
    err := e0; d0exp_ELSEsome(tok, d0e)
  end // end of [T_THEN]
| _(*non-ELSE*) =>
  (
    d0exp_ELSEnone((*void*)) // HX: ELSE-less
  )
//
end // end of [p_d0exp_ELSE]

(* ****** ****** *)
//
implement
p_ENDWHERE(buf, err) = let
  val e0 = err
  val tok = buf.get0()
in
  case+
  tok.node() of
//
  | T_END() =>
    let
    val () =
      buf.incby1() in endwhere_cons1(tok)
    // end of [val]
    end
//
  | T_RBRACE() =>
    let
      val () = buf.incby1()
      val tok2 = buf.get0()
    in
      case+
      tok2.node() of
      | T_END() =>
        let
          val () = buf.incby1()
        in
          endwhere_cons2(tok, optn1_some(tok2))
        // end of [val]
        end
      | T_ENDWHERE() =>
        let
          val () = buf.incby1()
        in
          endwhere_cons2(tok, optn1_some(tok2))
        // end of [val]
        end
      | _ (* non-END *) => endwhere_cons1(tok)
    end
//
  | T_ENDWHERE() =>
    let
      val () = buf.incby1() in endwhere_cons1(tok)
    end
//
  | _ (* non-END *) =>
    let
    val () = (err := e0 + 1) in endwhere_cons1(tok)
    end
end // end of [p_ENDWHERE]
//
(* ****** ****** *)

implement
pseq_d0eclseq_WHERE
  (buf, err) = let
//
val tok = buf.get0()
//
in
//
case+
tok.node() of
| T_WHERE() => let
//
    val () = buf.incby1()
//
    val opt =
      popt_LBRACE(buf, err)
    val d0cs =
      p_d0eclseq_dyn(buf, err)
//
    val tend = p_ENDWHERE(buf, err)
//
    val d0c0 =
      d0eclseq_WHERE(tok, opt, d0cs, tend)
    // end of [val]
    val d0cs = pseq_d0eclseq_WHERE(buf, err)
  in
    list1_cons(d0c0, d0cs)
  end
| _(* non-WHERE *) => list1_nil(*void*)
//
end // end of [pseq_d0eclseq_WHERE]

(* ****** ****** *)

(*
implement
popt_d0eclseq_WHERE
  (buf, err) = let
//
val tok = buf.get0()
//
in
//
case+
tok.node() of
| T_WHERE() => let
//
    val () = buf.incby1()
//
    val opt =
      popt_LBRACE(buf, err)
    val d0cs =
      p_d0eclseq_dyn(buf, err)
//
    val tend = p_ENDWHERE(buf, err)
//
  in
    optn1_some(d0eclseq_WHERE(tok, opt, d0cs, tend))
  end
| _(* non-WHERE *) => optn1_none((*void*))
//
end // end of [popt_d0eclseq_WHERE]
*)

(* ****** ****** *)
//
(*
HX-2019-02-15:
//
case [d0e] of
| [d0p] when [d0e]
| [d0p] when [d0e] as [d0p]
//
*)
//
extern
fun
p_d0gua: parser(d0gua)
extern
fun
p_d0guaseq_AND: parser(d0gualst)
//
implement
p_d0gua
  (buf, err) = let
//
val
d0e =
p_appd0exp(buf, err)
val tok = buf.get0()
//
in
//
case+
tok.node() of
| T_AS() => let
    val () = buf.incby1()
    val d0p = p_d0pat(buf, err)
    val loc_res = d0e.loc() + d0p.loc()
  in
    d0gua_make_node
      (loc_res, D0GUAmat(d0e, tok, d0p))
    // d0gua_make_node
  end // end of [T_AS]
| _(* non-AS *) =>
  (
    d0gua_make_node(d0e.loc(), D0GUAexp(d0e))
  ) (* end of [non-AS] *)
//
end // end of [p_d0gua]
//
implement
p_d0guaseq_AND
  (buf, err) =
(
//
  list1_vt2t
  (pstar_AND_fun{d0gua}(buf, err, p_d0gua))
//
) (* end of [p_d0guaseq_AND] *)
//
(* ****** ****** *)
//
extern
fun
p_dg0pat: parser(dg0pat)
//
(* ****** ****** *)

implement
p_dg0pat
  (buf, err) = let
//
val
d0p =
p_d0pat(buf, err)
//
val tok = buf.get0()
//
in
//
case+
tok.node() of
| T_WHEN() => let
    val () = buf.incby1()
    val d0gs =
      p_d0guaseq_AND(buf, err)
    // end of [val]
    val loc_res =
    (
      case+ d0gs of
      | list1_nil() =>
        (
          d0p.loc() + tok.loc()
        )
      | list1_cons _ =>
        let
          val d0g =
            list1_last(d0gs)
          // end of [val]
        in d0p.loc() + d0g.loc() end
    ) : loc_t // end of [val]
  in
    dg0pat_make_node
      (loc_res, DG0PATgua(d0p, tok, d0gs))
    // end of [dg0pat_make_node]
  end
| _ (* non-WHEN *) =>
    dg0pat_make_node(d0p.loc(), DG0PATpat(d0p))
//
end // end of [p_dg0pat]

(* ****** ****** *)

implement
p_d0clau
  (buf, err) = let
//
val
dgp =
p_dg0pat(buf, err)
//
val tok = buf.get0()
//
in
//
case+
tok.node() of
| T_EQGT() => let
    val () = buf.incby1()
    val d0e = p_d0exp(buf, err)
    val loc_res = dgp.loc() + d0e.loc()
  in
    d0clau_make_node
    (loc_res, D0CLAUclau(dgp, tok, d0e))
  end
| _ (* non-EQGT *) =>
    d0clau_make_node(dgp.loc(), D0CLAUgpat(dgp))
//
end // end of [p_d0clau]

implement
p_d0clauseq_BAR
  (buf, err) =
(
//
list1_vt2t
(pstar_BAR_fun{d0clau}(buf, err, p_d0clau))
//
) (* end of [p_d0clauseq_BAR] *)

(* ****** ****** *)

static
fun
t_dctkind
 : tnode -> bool
implement
t_dctkind
  (tnd) =
(
case+ tnd of
| T_FUN _ => true
| T_VAL _ => true | _ => false
)

(* ****** ****** *)
//
static
fun
p_precopt: parser(precopt)
and
p_precmod: parser(precmod)
and
p_signint: parser(signint)
//
(* ****** ****** *)

implement
p_precopt
  (buf, err) = let
//
val tok = buf.get0()
val tnd = tok.node()
//
in
//
case+ tnd of
//
| T_INT1 _ =>
  PRECOPTint(tok) where
  {
    val () = buf.incby1()
  }
| _ (* non-INT1 *) =>
  PRECOPTopr(id0, pmod) where
  {
    val id0 = p_i0dnt(buf, err)
    val pmod = p_precmod(buf, err)
  }
//
end // end of [p_precopt]

(* ****** ****** *)

implement
p_signint
  (buf, err) = let
//
val tok = buf.get0()
val tnd = tok.node()
//
in
//
case+ tnd of
| T_INT1 _ =>
  SIGNINTint(tok) where
  {
    val () = buf.incby1()
  }
| _ (* non-INT1 *) =>
  SIGNINTopr(tok, tint) where
  {
    val () = buf.incby1()
    val tint = buf.get1()
  }
//
end // end of [p_signint]

(* ****** ****** *)

implement
p_precmod
  (buf, err) = let
//
val tok = buf.get0()
val tnd = tok.node()
//
in
//
case+ tnd of
| T_LPAREN() =>
  PRECMODsome
  (tbeg, sint, tend) where
  {
    val () = buf.incby1()
    val sint =
      p_signint(buf, err)
    val tbeg = tok
    val tend = p_RPAREN(buf, err)
  }
| _ (* non-LPAREN *) => PRECMODnone()
//
end // end of [p_precmod]

(* ****** ****** *)

static
fun
p_abstdf0: parser(abstdf0)

(* ****** ****** *)

implement
p_abstdf0
  (buf, err) = let
//
val tok = buf.get0()
val tnd = tok.node()
//
in
//
case+ tnd of
| T_IDENT_sym("<=") => let
    val () = buf.incby1()
  in
    ABSTDF0lteq
    (tok, p_s0exp(buf, err))
  end
| T_IDENT_sym("==") => let
    val () = buf.incby1()
  in
    ABSTDF0eqeq
    (tok, p_s0exp(buf, err))
  end
| _(*non-eq-eqeq*) => ABSTDF0nil()
//
end // end of [p_abstdf0]

(* ****** ****** *)
//
static
fun
p_declmodopt: parser(declmodopt)
//
(* ****** ****** *)
//
static
fun
p_teqd0expopt: parser(teqd0expopt)
static
fun
p_wths0expopt: parser(wths0expopt)
//
(* ****** ****** *)

implement
p_declmodopt
  (buf, err) = let
//
val e0 = err
//
val tok0 = buf.get0()
//
in
//
case+
tok0.node() of
//
| T_CLN() => let
    val () = buf.incby1()
    val tok1 = buf.get0()
  in
    case+
    tok1.node() of
    | T_LPAREN() => let
        val () = buf.incby1()
//
        val ids =
          p_i0dntseq_COMMA(buf, err)
        // end of [val]
//
        val tbeg = tok1
        val tend = p_RPAREN(buf, err)
//
      in
        DECLMODlist(tok0, tbeg, ids, tend)
      end // end of [T_LPAREN]
//
    | _(*non-LPAREN*) => let
        val id0 =
          p_i0dnt(buf, err) in DECLMODsing(tok0, id0)
        // end of [val]
      end // end of [non-LPAREN]
//
  end // end of [T_CLN]
//
| _ (* non-COLON *) => DECLMODnone(*void*)
//
end // end of [p_declmodopt]

(* ****** ****** *)

implement
p_teqd0expopt
  (buf, err) = let
//
val tok = buf.get0()
//
in
//
case+
tok.node() of
| T_EQ() =>
  TEQD0EXPsome
    (tok, d0e) where
  {
    val () = buf.incby1()
    val d0e = p_d0exp(buf, err)
  }
| _(*non-EQ*) => TEQD0EXPnone(*void*)
//
end // end of [p_teqd0expopt]

(* ****** ****** *)

implement
p_wths0expopt
  (buf, err) = let
//
val tok = buf.get0()
//
in
//
case+
tok.node() of
| T_WITHTYPE _ =>
  WTHS0EXPsome
    (tok, s0e) where
  {
    val () = buf.incby1()
    val s0e = p_s0exp(buf, err)
  }
| _(*non-WITH*) => WTHS0EXPnone(*void*)
//
end // end of [p_wths0expopt]

(* ****** ****** *)

local

(* ****** ****** *)
//
static
fun
p_d0cstdecl
 : parser(d0cstdecl)
and
p_d0cstdeclseq_AND
 : parser(d0cstdeclist)
//
static
fun
p_dynconst : parser(d0ecl)
//
implement
p_d0cstdecl
  (buf, err) = let
//
val e0 = err
//
val
nam = p_d0pid(buf, err)
val
arg = p_d0argseq(buf, err)
val
res = p_effs0expopt(buf, err)
val
def = p_teqd0expopt(buf, err)
//
val
loc = nam.loc()
val
loc =
(
case+ def of
| TEQD0EXPnone() =>
  (
  case+ res of
  | EFFS0EXPnone() =>
    (case+ arg of
     | list1_nil() => loc
     | list1_cons
       (tqa, _) => loc+tqa.loc()
    )
  | EFFS0EXPsome(s0e) => loc+s0e.loc()
(*
  | EFFS0EXPsome(sfe, s0e) => loc+s0e.loc()
*)
  )
| TEQD0EXPsome(_, d0e) => loc+d0e.loc()
) : loc_t // end of [val]
//
in
  err := e0;
  D0CSTDECL
  (@{loc=loc,nam=nam,arg=arg,res=res,def=def})
end // end of [p_d0cstdecl]

(* ****** ****** *)
//
implement
p_d0cstdeclseq_AND
  (buf, err) =
(
//
list1_vt2t
(pstar_AND_fun
 {d0cstdecl}(buf, err, p_d0cstdecl))
//
) (* end of [p_d0cstdeclseq_AND] *)
//
(* ****** ****** *)
//
static
fun
p_v0aldecl
 : parser(v0aldecl)
and
p_v0aldeclseq_AND
 : parser(v0aldeclist)
//
(* ****** ****** *)

implement
p_v0aldecl
  (buf, err) = let
//
val e0 = err
//
val
d0p = p_d0pat(buf, err)
//
val teq = p_EQ(buf, err)
val d0e = p_d0exp(buf, err)
//
val
wopt = p_wths0expopt(buf, err)
//
val loc0 = d0p.loc()
//
val loc1 =
(
case+ wopt of
| WTHS0EXPnone() => loc0+d0e.loc()
| WTHS0EXPsome(_, s0e) => loc0+s0e.loc()
) : loc_t // end-of-val
//
in
  err := e0;
  V0ALDECL
  (@{loc=loc1,pat=d0p,teq=teq,def=d0e,wtp=wopt})
end // end of [p_v0aldecl]

(* ****** ****** *)
//
implement
p_v0aldeclseq_AND
  (buf, err) =
(
//
list1_vt2t
(pstar_AND_fun
 {v0aldecl}(buf, err, p_v0aldecl))
//
) (* end of [p_v0aldeclseq_AND] *)
//
(* ****** ****** *)

extern
fun
ptok_valdecl
( tok: token
, buf: &tokbuf >> _
, err: &int >> _): d0ecl
implement
ptok_valdecl
(
tok, buf, err
) = let
  val e0 = err
  val () =
    buf.incby1()
  // end of [val]
  val loc = tok.loc()
  val mopt =
    p_declmodopt(buf, err)
  val d0cs =
    p_v0aldeclseq_AND(buf, err)
  val loc_res =
  (
    case+ d0cs of
    | list1_nil() => loc
    | list1_cons _ => let
        val d0c =
          list1_last(d0cs) in loc+d0c.loc()
        // end of [val]
      end // end of [list1_cons]
  ) : loc_t // end of [val]
in
  err := e0;
  d0ecl_make_node
    ( loc_res, D0Cvaldecl(tok, mopt, d0cs) )
  // d0ecl_make_node
end // end of [ptok_valdecl]

(* ****** ****** *)
//
static
fun
p_v0ardecl
 : parser(v0ardecl)
and
p_v0ardeclseq_AND
 : parser(v0ardeclist)
//
(* ****** ****** *)
//
implement
p_v0ardecl
  (buf, err) = let
//
val e0 = err
//
val nam =
p_d0pid(buf, err)
//
(*
val res =
p_effs0expopt(buf, err)
*)
val res =
popt_s0exp_anno(buf, err)
//
val wth =
let
  val tok = buf.get0()
in (* in-of-let *)
//
case+
tok.node() of
| T_WITH() => let
    val () = buf.incby1()
  in
    optn1_some(p_d0pid(buf, err))
  end // end of [T_WITH]
| _(*non-WITH*) => optn1_none(*void*)
//
end : d0pidopt // end of [val]
//
val ini = p_teqd0expopt(buf, err)
//
val loc0 = nam.loc((*void*))
//
val loc1 =
(
case+ ini of
| TEQD0EXPnone() =>
  ( case+ wth of
    | optn1_none() =>
      (
      case+ res of
      | optn1_none() => loc0
      | optn1_some(s0e) => loc0+s0e.loc()
      )
    | optn1_some(pid) => (loc0 + pid.loc())
  )
| TEQD0EXPsome(teq, d0e) => loc0 + d0e.loc()
) : loc_t // end of [val]
//
in
  err := e0;
  V0ARDECL
  (@{loc=loc1,nam=nam,wth=wth,res=res,ini=ini})
end // end of [p_v0ardecl]
//
(* ****** ****** *)
//
implement
p_v0ardeclseq_AND
  (buf, err) =
(
//
list1_vt2t
(pstar_AND_fun
 {v0ardecl}(buf, err, p_v0ardecl))
//
) (* end of [p_v0ardeclseq_AND] *)
//
(* ****** ****** *)
//
extern
fun
ptok_vardecl
( tok: token
, buf: &tokbuf >> _
, err: &int >> _): d0ecl
implement
ptok_vardecl
(
tok, buf, err
) = let
  val e0 = err
  val () =
    buf.incby1()
  // end of [val]
  val loc = tok.loc()
//
  val d0cs =
    p_v0ardeclseq_AND(buf, err)
//
  val loc_res =
  (
    case+ d0cs of
    | list1_nil() => loc
    | list1_cons _ => let
        val d0c =
          list1_last(d0cs) in loc+d0c.loc()
        // end of [val]
      end // end of [list1_cons]
  ) : loc_t // end of [val]
in
  err := e0;
  d0ecl_make_node(loc_res, D0Cvardecl(tok, d0cs))
  // d0ecl_make_node
end // end of [ptok_vardecl]
//
(* ****** ****** *)
//
static
fun
p_f0undecl
 : parser(f0undecl)
and
p_f0undeclseq_AND
 : parser(f0undeclist)
//
(* ****** ****** *)
//
implement
p_f0undecl
  (buf, err) = let
//
val e0 = err
//
val nam =
p_d0pid(buf, err)
//
val arg =
p_f0argseq(buf, err)
val res =
p_effs0expopt(buf, err)
//
val teq = p_EQ(buf, err)
val d0e = p_d0exp(buf, err)
//
val wopt = p_wths0expopt(buf, err)
//
val loc0 = nam.loc((*void*))
//
val loc1 =
( case+ wopt of
| WTHS0EXPnone() => loc0+d0e.loc()
| WTHS0EXPsome(_, s0e) => loc0+s0e.loc()
) : loc_t // end-of-val
//
in
  err := e0;
  F0UNDECL
  (@{loc=loc1,nam=nam,arg=arg,res=res,teq=teq,def=d0e,wtp=wopt})
end // end of [p_f0undecl]
//
implement
p_f0undeclseq_AND
  (buf, err) =
(
//
list1_vt2t
(pstar_AND_fun
 {f0undecl}(buf, err, p_f0undecl))
//
) (* end of [p_f0undeclseq_AND] *)
//
(* ****** ****** *)
//
extern
fun
ptok_fundecl
( tok: token
, buf: &tokbuf >> _
, err: &int >> _): d0ecl
implement
ptok_fundecl
(
tok, buf, err
) = let
//
  val e0 = err
  val () =
    buf.incby1()
  // end of [val]
  val loc = tok.loc()
//
  val mopt =
    p_declmodopt(buf, err)
//
  val tqas =
    p_tq0argseq(buf, err)
//
  val d0cs =
    p_f0undeclseq_AND(buf, err)
//
  val loc_res =
  (
    case+ d0cs of
    | list1_nil() => loc
    | list1_cons _ => let
        val d0c =
          list1_last(d0cs) in loc+d0c.loc()
        // end of [val]
      end // end of [list1_cons]
  ) : loc_t // end of [val]
in
  err := e0;
  d0ecl_make_node
    (loc_res, D0Cfundecl(tok, mopt, tqas, d0cs))
  // d0ecl_make_node
end // end of [ptok_fundecl]
//
(* ****** ****** *)
//
extern
fun
ptok_impdecl
(tok: token, buf: &tokbuf >> _, err: &int >> _): d0ecl

implement
ptok_impdecl(tok, buf, err) = let
//
  val e0 = err
  val () =buf.incby1()

  val loc = tok.loc()
//
  val mopt = p_declmodopt(buf, err)
//
  val sqas = p_sq0argseq(buf, err)
  val tqas =
    p_tq0argseq(buf, err)
//
  val dqid = p_dq0eid(buf, err)
  val tias = p_ti0argseq(buf, err)
  val f0as = p_f0argseq(buf, err)
//
  val tres = p_effs0expopt(buf, err)
//
  val teq0 = p_EQ(buf, err)
  val d0e1 = p_d0exp(buf, err)
//
  val loc_res = tok.loc() + d0e1.loc()
in
  err := e0;
  d0ecl_make_node
  ( loc_res
  , D0Cimpdecl
    ( tok, mopt
    , sqas, tqas
    , dqid, tias, f0as, tres, teq0, d0e1)
  ) (* d0ecl_make_node *)
end // end of [ptok_impdecl]
//
(* ****** ****** *)
//
extern
fun
ptok_dynconst
( tok: token
, buf: &tokbuf >> _
, err: &int >> _): d0ecl
implement
ptok_dynconst
(
tok, buf, err
) = let
  val e0 = err
  val () =
    buf.incby1()
  // end of [val]
  val loc = tok.loc()
  val tqas =
    p_tq0argseq(buf, err)
  val d0cs =
    p_d0cstdeclseq_AND(buf, err)
  val loc_res =
  (
    case+ d0cs of
    | list1_nil() =>
      (case+ tqas of
       | list1_nil() => loc
       | list1_cons
           (tqa, _) => loc + tqa.loc()
         // list1_cons
      )
    | list1_cons(d0c, _) => loc+d0c.loc()
  ) : loc_t // end of [val]
in
  err := e0;
  d0ecl_make_node
    ( loc_res, D0Cdynconst(tok, tqas, d0cs) )
  // d0ecl_make_node
end // end of [ptok_dynconst]
//
(* ****** ****** *)

in (* in-of-local *)

implement
fp_d0ecl
(f0, buf, err) = let
//
val e0 = err
val tok = buf.get0()
//
val loc = tok.loc()
val tnd = tok.node()
//
(*
val () = print_tnode(tok.node())
*)
in
//
case+ tnd of
//
| T_LOCAL() => let
    val () =
      buf.incby1()
    // end of [val]
    val tbeg = tok
    val head =
    fp_d0eclseq(f0, buf, err)
    val tmid = p_IN(buf, err)
    val body =
    fp_d0eclseq(f0, buf, err)
    val tend = p_ENDLOCAL(buf, err)
    val loc_res = tbeg.loc() + tend.loc()
  in
    err := e0;
    d0ecl_make_node
    ( loc_res
    , D0Clocal(tbeg, head, tmid, body, tend))
  end // end of [T_LOCAL]
//
| T_ABSSORT() => let
    val () =
      buf.incby1()
    val tid =
      p_s0tid(buf, err)
    val loc_res = tok.loc() + tid.loc()
  in
    err := e0;
    d0ecl_make_node(loc_res, D0Cabssort(tok, tid))
  end
//
| T_SORTDEF() => let
//
    val () = buf.incby1()
//
    val tid =
      p_s0tid(buf, err)
    val tok1 = p_EQ(buf, err)
    val def2 = p_s0rtdef(buf, err)
    val loc_res = loc+def2.loc()
  in
    err := e0;
    d0ecl_make_node
    ( loc_res
    , D0Csortdef(tok, tid, tok1, def2)
    ) (* d0ecl_make_node *)
  end
//
(*
sexpdef::
| si0de
  s0margseq
  colons0rtopt EQ s0exp
*)
| T_SEXPDEF(k0) => let
//
    val () = buf.incby1()
//
    val sid =
      p_s0eid(buf, err)
    val s0mas =
      p_s0margseq(buf, err)
    // end of [val]
    val anno =
      popt_sort0_anno(buf, err)
    // end of [val]
    val tok1 = p_EQ(buf, err)
    val s0e0 = p_s0exp(buf, err)
    val loc_res = loc + s0e0.loc()
  in
    err := e0;
    d0ecl_make_node
    ( loc_res
    , D0Csexpdef
      (tok, sid, s0mas, anno, tok1, s0e0)
    ) (* d0ecl_make_node *)
  end
//
(*
abstype ::=
| si0de
  t0margseq
  colons0rtopt
  [LTEQ/EQEQ s0exp]
*)
| T_ABSTYPE(k0) => let
//
    val () = buf.incby1()
//
    val sid =
      p_s0eid(buf, err)
    val tmas =
      p_t0margseq(buf, err)
    // end of [val]
    val anno =
      popt_idsort0_anno(buf, err)
    // end of [val]
    val tdef = p_abstdf0(buf, err)
    val loc_res =
    (
    case+ tdef of
    | ABSTDF0nil() =>
      (
      case+ tmas of
      | list1_nil() => loc+sid.loc()
      | list1_cons _ => let
        val t0ma =
        list1_last(tmas) in loc+t0ma.loc()
        end // end of [list1_cons]
      ) (* ABSTDF0nil *)
    | ABSTDF0lteq(_, s0e) => loc+s0e.loc()
    | ABSTDF0eqeq(_, s0e) => loc+s0e.loc()
    ) : loc_t // end of [val]
  in
    err := e0;
    d0ecl_make_node
    ( loc_res
    , D0Cabstype(tok, sid, tmas, anno, tdef) )
  end
//
| T_ABSIMPL() => let
//
    val () = buf.incby1()
//
    val sqid =
    p_sq0eid(buf, err)
//
    val smas =
    p_s0margseq(buf, err)
    val res0 =
    popt_sort0_anno(buf, err)
//
    val teq1 = p_EQ(buf, err)
    val def2 = p_s0exp(buf, err)
    val loc_res = loc + def2.loc()
  in
    err := e0;
    d0ecl_make_node
    ( loc_res
    , D0Cabsimpl(tok, sqid, smas, res0, teq1, def2))
  end // end of [T_ABSIMPL]
//
| T_DATASORT() => let
//
    val () = buf.incby1()
//
    val d0cs =
      p_d0tsortseq_AND(buf, err)
    // end of [val]
    val loc_res =
    (
      case+ d0cs of
      | list1_nil() => loc
      | list1_cons _ =>
        let
        val d0c =
        list1_last(d0cs) in loc+d0c.loc()
        end
    ) : loc_t // end of [val]
  in
    err := e0;
    d0ecl_make_node
      ( loc_res, D0Cdatasort(tok, d0cs) )
    // d0ecl_make_node
  end
//
| T_DATATYPE(k0) => let
    val () = buf.incby1()
    val d0cs =
      p_d0atypeseq_AND(buf, err)
    val tok1 = buf.get0()
    val wopt =
    (
    case+
    tok1.node() of
    | T_WHERE() => let
        val () = buf.incby1()
        val topt =
        popt_LBRACE(buf, err)
        val wdcs =
        p_d0eclseq_sta(buf, err)
        val tok2 = buf.get0()
        val ((*void*)) =
        (
        case+
        tok2.node() of
        | T_END() => buf.incby1()
        | T_RBRACE() => buf.incby1()
        | T_ENDWHERE() => buf.incby1()
        | _(*non-closing*) => (err := err+1)
        ) : void // end of [val]
      in
        WD0CSsome(tok1, topt, wdcs, tok2)
      end // end of [T_WHERE]
    | _(*non-WHERE*) => WD0CSnone(*void*)
    ) : wd0eclseq // end of [val]
    val loc_res =
    (
      case+ wopt of
      | WD0CSnone() =>
        (
        case+ d0cs of
        | list1_nil() => loc
        | list1_cons _ =>
          let
          val d0c =
          list1_last(d0cs) in loc+d0c.loc()
          end
        )
      | WD0CSsome(_, _, _, tok) => loc+tok.loc()
    ) : loc_t // end of [val]
  in
    err := e0;
    d0ecl_make_node
      ( loc_res, D0Cdatatype(tok, d0cs, wopt) )
    // d0ecl_make_node
  end
//
| T_VAL _ when f0 > 0 =>
  (
    ptok_valdecl(tok, buf, err)
  )
//
| T_VAR _ when f0 > 0 =>
  (
    ptok_vardecl(tok, buf, err)
  )
//
| T_FUN _ when f0 > 0 =>
  (
    ptok_fundecl(tok, buf, err)
  )
//
| T_IMPLMNT _ =>
  (
    ptok_impdecl(tok, buf, err)
  )
//
| tnd when
  t_dctkind(tnd) =>
  (
    ptok_dynconst(tok, buf, err)
  )
//
| T_SRP_STATIC() => let
    val () =
      buf.incby1()
(*
    val d0c =
      p_d0ecl_sta(buf, err)
*)
    val d0c =
      fp_d0ecl(f0, buf, err)
    // end of [val]
    val loc_res = loc+d0c.loc()
  in
    err := e0;
    d0ecl_make_node(loc_res, D0Cstatic(tok, d0c))
  end // end of [T_SRP_STATIC]
//
| T_SRP_EXTERN() => let
    val () =
      buf.incby1()
(*
    val d0c =
      p_d0ecl_sta(buf, err)
*)
    val d0c =
      fp_d0ecl(f0, buf, err)
    // end of [val]
    val loc_res = loc+d0c.loc()
  in
    err := e0;
    d0ecl_make_node(loc_res, D0Cextern(tok, d0c))
  end // end of [T_SRP_EXTERN]
//
| T_SRP_INCLUDE() => let
    val () = buf.incby1()
    val d0e =
      p_appd0exp(buf, err)
    // end of [val]
    val loc_res = loc+d0e.loc()
  in
    err := e0;
    d0ecl_make_node(loc_res, D0Cinclude(tok, d0e))
  end // end of [#INCLUDE(...)]
//
| T_SRP_STALOAD() => let
//
    val () = buf.incby1()
//
    val d0e =
      p_appd0exp(buf, err)
    // end of [val]
    val loc_res = loc+d0e.loc()
  in
    err := e0;
    d0ecl_make_node(loc_res, D0Cstaload(tok, d0e))
  end // end of [#STALOAD(...)]
//
| T_SRP_SYMLOAD() => let
//
    val () = buf.incby1()
//
    val sym = p_s0ymb(buf, err)
    val twth = p_WITH(buf, err)
    val dqid = p_dq0eid(buf, err)
//
(*
    val () =
    println!("p_d0ecl: tok = ", tok)
    val () =
    println!("p_d0ecl: sym = ", sym)
    val () =
    println!("p_d0ecl: twth = ", twth)
    val () =
    fprintln!
    (stdout_ref, "p_d0ecl: dqid = ", dqid)
*)
//
    val tint = let
      val tok = buf.get0()
      val tnd = tok.node()
    in
      case+ tnd of
      | T_OF() =>
        (
        optn1_some(p_t0int(buf, err))
        ) where
        {
          val () = buf.incby1()
        }
      | _(*non-OF*) => optn1_none(*void*)
    end : t0intopt // end-of-let
//
    val loc_res =
    let
      val loc = tok.loc()
    in
      case+ tint of
      | optn1_none() => loc+dqid.loc()
      | optn1_some(int) => loc+int.loc()
    end : location // end of [val]
//
  in
    err := e0;
    d0ecl_make_node
    ( loc_res
    , D0Csymload(tok, sym, twth, dqid, tint))
  end // end of [#STALOAD(...)]
//
| T_SRP_STACST() => let
//
    val () = buf.incby1()
    val sid = p_s0eid(buf, err)
    val tmas = p_t0margseq(buf, err)
    val tok1 = p_COLON(buf, err)
    val s0t2 = p_sort0(buf, err)
    val loc_res = loc + s0t2.loc()
  in
    err := e0;
    d0ecl_make_node
    (loc_res, D0Cstacst0(tok, sid, tmas, tok1, s0t2))
  end // end of [T_SRP_STACST]
//
| T_SRP_NONFIX() => let
//
    val () = buf.incby1()
//
    val ids =
      p_i0dntseq(buf, err)
    // end of [val]
    val loc_res =
    (
      case+ ids of
      | list1_nil() => loc
      | list1_cons _ =>
        let
        val id1 = list1_last(ids) in loc+id1.loc()
        end // end of [list1_cons]
    ) : loc_t // end of [val]
  in
    err := e0;
    d0ecl_make_node(loc_res, D0Cnonfix(tok, ids))
  end // end of [NONFIX]
//
| T_SRP_FIXITY(knd) => let
//
    val () = buf.incby1()
//
    val ids =
      p_i0dntseq(buf, err)
    val tok2 = buf.get0()
    val opt2 =
    (
      case+
      tok2.node() of
      | T_OF() =>
        p_precopt
        (buf, err) where
        {
          val () = buf.incby1()
        }
      | _(* non-OF *) => PRECOPTnil()
    ) : precopt // end of [val]
//
    val loc_res =
    (
      case+ ids of
      | list1_nil() => loc
      | list1_cons _ =>
        let
        val id1 = list1_last(ids) in loc + id1.loc()
        end // end of [list1_cons]
    ) : loc_t // end of [val]
  in
    err := e0;
    d0ecl_make_node(loc_res, D0Cfixity(tok, ids, opt2))
  end // end of [FIXITY(knd)]
//
| _ (* errorcase *) =>
  let
    val () =
    (err := e0 + 1) in d0ecl_make_node(loc, D0Cnone(tok))
  end // end of [let]
//
end // end of [fp_d0ecl]

end // end of [local]

(* ****** ****** *)

implement
fp_d0eclseq(f0, buf, err) = let
//
val e0 = err
//
  fun loop
  (buf: &tokbuf >> _,
   err: &int >> _,
   res: &ptr? >> List0_vt(d0ecl)) : void = let
  val x0 = fp_d0ecl(f0, buf, err)
  in
    if (err = e0) then () where
    {
      val () = (res := list1_vt_cons{d0ecl}{0}(x0, _))
      val+list1_vt_cons(_, res1) = res
      val () = loop(buf, err, res1)
      prval ((*folded*)) = fold@(res)
    } else (res := list1_vt_nil()) where
    {
      val () = err := e0
    }
  end // end of [loop]
//
in
  (loop(buf, err, res); list1_vt2t{d0ecl}(res)) where
  {
    var res: ptr
  }
end // (* end of [fp_d0eclseq] *)

(* ****** ****** *)

implement
fptop_d0eclseq
(f0, buf, err) = let
//
fnx
loop1(buf: &tokbuf >> _,
      err: &int >> int,
      res: List0_vt(d0ecl)) : List0_vt(d0ecl) =
let
  val d0c = fp_d0ecl(f0, buf, err)
in
  case+
  d0c.node() of
  | D0Cnone(tok) =>
    (
      case+
      tok.node() of
      | T_EOF() => res
      | _(*non-EOF*) => let
          val loc = tok.loc()
          val n0r = loc.beg_nrow()
        in
          loop2(buf, err, n0r, res)
        end
    )
  | _ (*non-none*) =>
    loop1(buf, err, list1_vt_cons(d0c, res))
end // end of [loop]
//
and
loop2(buf: &tokbuf >> _,
      err: &int >> _,
      n0r: int,
      res: d0eclist_vt): d0eclist_vt =
let
  val tok =
  tokbuf_getok0(buf)
  val tnd = tok.node()
in
//
case+ tnd of
| T_EOF() => res
| _(*non-EOF*) => let
    val loc = tok.loc()
    val n1r = loc.beg_nrow()
(*
    val (_) = println! ("n0r = ", n0r)
    val (_) = println! ("n1r = ", n1r)
*)
  in
//
  if (n1r <= n0r)
  then let
    val () = buf.incby1()
    val () = err := err + 1
    val d0c =
      d0ecl_make_node(loc, D0Ctokerr(tok))
    // end of [val]
  in
    loop2(buf, err, n0r, list1_vt_cons(d0c, res))
  end // end of [then]
  else loop1(buf, err, res) // end of [else]
//
  end // end of [let]
//
end // end of [loop2]
//
in
//
list1_vt2t
(ofg0v(list0_vt_reverse(g0ofg1(loop1(buf, err, list1_vt_nil)))))
//
end // end of [fp_d0eclseq_top]

(* ****** ****** *)

local
//
#define STATIC 0
#define DYNAMIC 1
//
in
//
implement
p_d0ecl_sta(buf, err) =
  fp_d0ecl(STATIC, buf, err)
implement
p_d0ecl_dyn(buf, err) =
  fp_d0ecl(DYNAMIC, buf, err)
//
implement
p_d0eclseq_sta(buf, err) =
  fp_d0eclseq(STATIC, buf, err)
implement
p_d0eclseq_dyn(buf, err) =
  fp_d0eclseq(DYNAMIC, buf, err)
//
implement
ptop_d0eclseq_sta(buf, err) =
  fptop_d0eclseq(STATIC, buf, err)
implement
ptop_d0eclseq_dyn(buf, err) =
  fptop_d0eclseq(DYNAMIC, buf, err)
//
end // end of [local]

(* ****** ****** *)

(* end of [xats_parsing_dynexp.dats] *)
