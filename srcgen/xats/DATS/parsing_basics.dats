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
(*
#staload "libats/SATS/gint.sats"
#staload _ = "libats/DATS/gint.dats"

#staload "libats/SATS/stdio.sats"
#staload _ = "libats/DATS/stdio.dats"

#staload "libats/SATS/list.sats"
#staload _ = "libats/DATS/list.dats"

#staload "libats/SATS/list_vt.sats"
#staload _ = "libats/DATS/list_vt.dats"
*)

#include "share/HATS/temptory_staload_bucs320.hats"

#staload
UN =
"libats/SATS/unsafe.sats"

//
(* ****** ****** *)

#staload "./../SATS/lexing.sats"
#staload "./../SATS/parsing.sats"

(* ****** ****** *)

(*
implement
{}(*tmp*)
synent_null() = $UN.cast(the_null_ptr)
implement
{}(*tmp*)
synent_is_null(x) = iseqz($UN.cast{ptr}(x))
implement
{}(*tmp*)
synent_isnot_null(x) = isneqz($UN.cast{ptr}(x))
*)

(* ****** ****** *)

implement
p_EQ(buf, err) = let
  val e0 = err
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_EQ() => (buf.incby1(); tok)
    | _ (* non-EQ *) => ((err := e0 + 1); tok)
end // end of [p_EQ]

(* ****** ****** *)

implement
p_GT(buf, err) = let
  val e0 = err
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_GT() => (buf.incby1(); tok)
    | _ (* non-GT *) => ((err := e0 + 1); tok)
end // end of [p_GT]

(* ****** ****** *)

implement
p_BAR(buf, err) = let
  val e0 = err
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_BAR() => (buf.incby1(); tok)
    | _ (* non-BAR *) => ((err := e0 + 1); tok)
end // end of [p_BAR]

(* ****** ****** *)

implement
p_EQGT(buf, err) = let
  val e0 = err
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_EQGT() => (buf.incby1(); tok)
    | _ (* non-EQ *) => ((err := e0 + 1); tok)
end // end of [p_EQGT]

(* ****** ****** *)

implement
p_COLON(buf, err) = let
  val e0 = err
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_CLN() => (buf.incby1(); tok)
    | _ (* non-COLON *) => ((err := e0 + 1); tok)
end // end of [p_COLON]

(* ****** ****** *)

implement
p_GTDOT(buf, err) = let
  val e0 = err
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_GTDOT() => (buf.incby1(); tok)
    | _ (* non-GTDOT *) =>  ((err := e0 + 1); tok)
end // end of [p_GTDOT]

(* ****** ****** *)

implement
p_LPAREN(buf, err) = let
  val e0 = err
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_LPAREN() => (buf.incby1(); tok)
    | _ (* non-RLAREN *) =>  ((err := e0 + 1); tok)
end // end of [p_LPAREN]

implement
p_RPAREN(buf, err) = let
  val e0 = err
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_RPAREN() => (buf.incby1(); tok)
    | _ (* non-RPAREN *) =>  ((err := e0 + 1); tok)
end // end of [p_RPAREN]

(* ****** ****** *)

implement
p_LBRACE(buf, err) = let
  val e0 = err
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_LBRACE() => (buf.incby1(); tok)
    | _ (* non-RLRACE *) =>  ((err := e0 + 1); tok)
end // end of [p_LBRACE]

implement
p_RBRACE(buf, err) = let
  val e0 = err
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_RBRACE() => (buf.incby1(); tok)
    | _ (* non-RBRACE *) => ((err := e0 + 1); tok)
end // end of [p_RBRACE]

(* ****** ****** *)

implement
p_LBRACK(buf, err) = let
  val e0 = err
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_LBRACK() => (buf.incby1(); tok)
    | _ (* non-LBRACK *) => ((err := e0 + 1); tok)
end // end of [p_LBRACK]

implement
p_RBRACK(buf, err) = let
  val e0 = err
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_RBRACK() => (buf.incby1(); tok)
    | _ (* non-RBRACK *) => ((err := e0 + 1); tok)
end // end of [p_RBRACK]

(* ****** ****** *)

implement
p_OF(buf, err) = let
  val e0 = err
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_OF() => (buf.incby1(); tok)
    | _ (* non-OF *) => ((err := e0 + 1); tok)
end // end of [p_OF]

(* ****** ****** *)

implement
p_IN(buf, err) = let
  val e0 = err
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_IN() => (buf.incby1(); tok)
    | _ (* non-IN *) => ((err := e0 + 1); tok)
end // end of [p_IN]

(* ****** ****** *)

implement
p_WITH(buf, err) = let
  val e0 = err
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_WITH() => (buf.incby1(); tok)
    | _ (* non-WITH *) =>  ((err := e0 + 1); tok)
end // end of [p_WITH]

(* ****** ****** *)

implement
p_END(buf, err) = let
  val e0 = err
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_END() => (buf.incby1(); tok)
    | _ (* non-END *) => ((err := e0 + 1); tok)
end // end of [p_END]

implement
p_ENDLET(buf, err) = let
  val e0 = err
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_END() => (buf.incby1(); tok)
    | T_ENDLET() => (buf.incby1(); tok)
    | _ (* non-END *) => ((err := e0 + 1); tok)
end // end of [p_ENDLET]

(* ****** ****** *)

implement
p_ENDLOCAL(buf, err) = let
  val e0 = err
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_END() => (buf.incby1(); tok)
    | T_ENDLOCAL() => (buf.incby1(); tok)
    | _ (* non-END *) => ((err := e0 + 1); tok)
end // end of [p_ENDLOCAL]

(* ****** ****** *)

implement
popt_BAR(buf, err) = let
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_BAR() => (buf.incby1(); optn1_some(tok))
    | _ (* non-BAR *) => optn1_none(*void*)
end // end of [popt_BAR]

(* ****** ****** *)

implement
popt_SMCLN(buf, err) = let
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_SMCLN() => (buf.incby1(); optn1_some(tok))
    | _ (* non-SMCLN *) => optn1_none(*void*)
end // end of [popt_SMCLN]

(* ****** ****** *)

implement
popt_LBRACE(buf, err) = let
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_LBRACE() => (buf.incby1(); optn1_some(tok))
    | _ (* non-LBRACE *) => optn1_none(*void*)
end // end of [popt_LBRACE]

(* ****** ****** *)

implement
popt_ENDIF(buf, err) = let
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_END() => (buf.incby1(); optn1_some(tok))
    | T_ENDIF() => (buf.incby1(); optn1_some(tok))
    | _ (* non-BAR *) => optn1_none(*void*)
end // end of [popt_ENDIF]

implement
popt_ENDCASE(buf, err) = let
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_END() => (buf.incby1(); optn1_some(tok))
    | T_ENDCASE() => (buf.incby1(); optn1_some(tok))
    | _ (* non-BAR *) => optn1_none(*void*)
end // end of [popt_ENDCASE]

(* ****** ****** *)

implement
popt_ENDLAM(buf, err) = let
  val tok = buf.get0()
in
  case+ tok.node() of
    | T_END() => (buf.incby1(); optn1_some(tok))
    | T_ENDLAM() => (buf.incby1(); optn1_some(tok))
    | _ (* non-BAR *) => optn1_none(*void*)
end // end of [popt_ENDLAM]

(* ****** ****** *)
//
(*
fun
pstar_fun
  {a:type}
(
  buf: &tokbuf >> _
, err: &int >> _, fpar: parser(a)
) : List0_vt(a) // end of [pstar_fun]
*)
//
implement
pstar_fun{a}(buf, err, fpar) = let
//
val e0 = err
//
fun
loop
(
  buf: &tokbuf >> _,
  err: &int >> _,
  res: &ptr? >> List0_vt(a)
)
: void = let

  val x0 = fpar(buf, err)

in
  if (err = e0)
  then {(* let *)
    val () = (res := list1_vt_cons{a}{0}(x0, _))

    val+list1_vt_cons(_, res1) = res

    val () = loop(buf, err, res1)
    prval ((*folded*)) = fold@(res)

  (* in *)
  (*   // nothing *)
  (* end // end of [then] *)
  }
  else let
    val () = err := e0
  in
    res := list1_vt_nil((*void*))
  end // end of [else]
end // end of [loop]
//
in
  let var res: ptr in loop(buf, err, res); res end
end // end of [pstar_fun]
//
(* ****** ****** *)

(*
fun
pstar_sep_fun
  {a:type}
(
  buf: &tokbuf >> _, err: &int >> _
, fsep: (tnode) -> bool, fpar: parser(a)
) : List0_vt(a) // end of [pstar_sep_fun]
*)
implement
pstar_sep_fun{a}(buf, err, fsep, fpar) = let
//
fun
loop
(buf: &tokbuf >> _, err: &int >> _, res: &ptr? >> List0_vt(a)) : void = let
  val sep = buf.get0()
in
  if
  fsep(sep.node())
  then let
    val () = buf.incby1()
    val x0 = fpar(buf, err)
    val () =
    (
      res :=
      list1_vt_cons{a}{0}(x0, _)
    )
    val+list1_vt_cons(_, res1) = res
    val () = loop(buf, err, res1)
    prval ((*folded*)) = fold@(res)
  in
    // nothing
  end // end of [then]
  else (res := list1_vt_nil(*void*))
end // end of [loop]
//
val e0 = err
val x0 = fpar(buf, err)
//
in
  if
  (err = e0)
  then let
    var res: ptr
  in
    loop(buf, err, res);
    list1_vt_cons(x0, res)
  end // end of [then]
  else let
    val () = (err := e0) in list1_vt_nil(*void*)
  end // end of [else]
end // end of [pstar_sep_fun]

(* ****** ****** *)
//
implement
pstar_AND_fun{a}(buf, err, fpar) = (
//
pstar_sep_fun
(buf, err, tnode_is_AND, fpar)
//
) (* end of [pstar_AND_fun] *)
//
implement
pstar_BAR_fun{a}(buf, err, fpar) =
(
  pstar_sep_fun
  (buf, err, tnode_is_BAR, fpar)
)
(* end of [pstar_BAR_fun] *)
//
(* ****** ****** *)
//
implement
pstar_COMMA_fun{a}(buf, err, fpar) =
(
  pstar_sep_fun
  (buf, err, tnode_is_COMMA, fpar)
)
(* end of [pstar_COMMA_fun] *)
//
(* ****** ****** *)
//
implement
pstar_SMCLN_fun{a}(buf, err, fpar) =
(
  pstar_sep_fun
  (buf, err, tnode_is_SMCLN, fpar)
)
(* end of [pstar_SMCLN_fun] *)
//
implement
pstar_BARSMCLN_fun{a}(buf, err, fpar) =
(
  pstar_sep_fun
  (buf, err, tnode_is_BARSMCLN, fpar)
)
(* end of [pstar_BARSMCLN_fun] *)
//
  implement
  pstar_COMMA_SMCLN_fun{a}(buf, err, fpar) =
  (
    pstar_sep_fun
    (buf, err, tnode_is_COMMA_SMCLN, fpar)
  )


//
(* ****** ****** *)
//
// HX-2018-10-07
// static/dynamic: 0/1
//
implement
parse_from_stdin_toplevel(stadyn) =
  parse_from_fileref_toplevel(stadyn, the_stdin<>())
  //stdin_ref)
// end of [parser_from_stdin_toplevel]
//
implement
parse_from_fileref_toplevel(stadyn, inp) = let
//
val toks = fileref_tokenize(inp)

(*
val _ = $showtype(toks)
*)

extern castfn
list1_vt2t1
{a:tflt}{n:int}
(list1_vt(INV(a),n)):<> list1(a, n)

val toks1 = list1_vt2t1{token}(toks)


val () = list0_foreach<token>(g0ofg1(toks1)) where
{
  //impltmp list0_foreach$work<token>(x) = println!(x)
  impltmp list0_foreach$work<token>(x) = (print_token(x); println!())
}

extern castfn
list1_t2vt1
{a:tflt}{n:int}
(list1(INV(a),n)):<> list1_vt(a, n)



val () = list0_foreach<token>(g0ofg1(toks1)) where
{
  //impltmp list0_foreach$work<token>(x) = println!(x)
  impltmp list0_foreach$work<token>(x) = (print2_token(x))//; println!())
}


val toks =  list1_t2vt1{token}(toks1)

val toks = list1_vt2t(lexing_preprocess_tokenlst(toks))
//
(* val toks2 = toks : list1(token) *)
(* val () = print(g0ofg1(toks)) *)
(* val _ = $showtype(toks2) *)

(*
val () = list0_foreach<token>(g0ofg1(toks)) where
{
  //impltmp list0_foreach$work<token>(x) = println!(x)
  impltmp list0_foreach$work<token>(x) = (show(x))//; println!())
}
*)


(*
val _(*ntok*) =
list_iforeach<token>(toks) where
{
//
implement(env)
list_iforeach$fwork<token><env>
  (i, x, env) =
  println!("TOKEN(", i, ") = ", x)
//
} // end of [where] // end of [val]
*)
//
in
//
let
  var err: int
  var buf: tokbuf
  val ((*void*)) = (err := 0)
  val ((*void*)) = tokbuf_initize_list(buf, toks)
in
  fptop_d0eclseq(stadyn, buf, err)
end
//
end // end of [parse_from_fileref_toplevel]
//
(* ****** ****** *)

(* end of [xats_parsing_basics.dats] *)
