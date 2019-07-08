(* ****** ****** *)

(*
#staload "libats/SATS/stdio.sats"
#staload _ = "libats/DATS/stdio.dats"
#staload "libats/SATS/print.sats"
#staload _ = "libats/DATS/print.dats"
*)
#include "share/HATS/temptory_staload_bucs320.hats"

#staload UN = "libats/SATS/unsafe.sats"

//
(* ****** ****** *)
//
#staload "./../SATS/basics.sats"

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
#staload
_(*TMP*) = "./../DATS/staexp0_show.dats"
//
(* ****** ****** *)
//
#macdef spc() = fprint$val<char>(the_stdout<>(), ' ')

impltmp show$before<>() = ()
impltmp show$after<>() = ()

impltmp show$beg<>() = ()
impltmp show$end<>() = ()

impltmp show$sep<>() = fprint$val<string>(the_stdout<>(), " ")
#macdef prout(x) = fprint$val<string>(the_stdout<>(), ,(x))
fun show_newline() = prout("\n")
fun show_spc() = prout(" ")
fun show_comma() = prout(",")
fun show_col() = prout(":")
fun show_bar() = prout("| ")

//
local

//
#macrodef
rec
auxlist
  (xs, y) =
(
//
if
iscons! (xs)
then `((show ,(car! xs)); (show_spc()); ,(auxlist (cdr! xs, y))) else y
// end of [if]
//
) (* end of [auxlist] *)
//
in (* in of [local] *)

#macdef
show_mac (x) =
,(
  if islist! (x) then auxlist (x, `()) else `(show(,(x)))
) (* end of [print_mac] *)

#macdef
showln_mac (x) =
,(
  if islist! (x)
    then auxlist (x, `(show_newline())) else `(show ,(x); show_newline())
  // end of [if]
) (* end of [println_mac] *)

end // end of [local]
//


impltmp
show$val<string>(x) = fprint$val<string>(the_stdout<>(), x)


impltmp
show$val<i0dnt>(x) = show_i0dnt(x)
impltmp
print$val<i0dnt> x = show_i0dnt x

impltmp
show$val<token> x = show_token x
impltmp
print$val<token> x = show_token x
//
impltmp
show$val<t0int> x = show_t0int x
impltmp
print$val<t0int> x = show_t0int x
//
(* ****** ****** *)
//
impltmp
show$val<s0exp> x = show_s0exp x
impltmp
print$val<s0exp> x = show_s0exp x
impltmp
show$val<s0qua> x = show_s0qua x
impltmp
print$val<s0qua> x = show_s0qua x
//
(* ****** ****** *)
//
impltmp
print$val<d0pat> x = show_d0pat x
impltmp
print$val<d0exp> x = show_d0exp x
impltmp
show$val<d0pat> x = show_d0pat x
impltmp
show$val<d0exp> x = show_d0exp x
//
impltmp
print$val<q0arg> x = show_q0arg x
impltmp
show$val<q0arg> x = show_q0arg x
//
impltmp
print$val<a0typ> x = show_a0typ x
impltmp
print$val<d0arg> x = show_d0arg x
impltmp
show$val<a0typ> x = show_a0typ x
impltmp
show$val<d0arg> x = show_d0arg x
//
impltmp
print$val<f0arg> x = show_f0arg x
impltmp
show$val<f0arg> x = show_f0arg x
//
impltmp
print$val<sq0arg> x = show_sq0arg x
impltmp
show$val<sq0arg> x = show_sq0arg x
//
impltmp
print$val<tq0arg> x = show_tq0arg x
impltmp
show$val<tq0arg> x = show_tq0arg x
//
impltmp
print$val<ti0arg> x = show_ti0arg x
impltmp
show$val<ti0arg> x = show_ti0arg x
//
(* ****** ****** *)
//
impltmp
print$val<d0ecl> x = show_d0ecl x
impltmp
show$val<d0ecl> x = show_d0ecl x
//
(* ****** ****** *)
//
impltmp
(a)//tmp
print$val<dl0abeled(a)> x = show_dl0abeled<a> x
impltmp
(a)//tmp
show$val<dl0abeled(a)> x = show_dl0abeled<a> x
//
(* ****** ****** *)

impltmp
print$val<d0clau> x = show_d0clau x
impltmp
show$val<d0clau> x = show_d0clau x

impltmp
print$val<d0gua> x = show_d0gua x
impltmp
show$val<d0gua> x = show_d0gua x

(* ****** ****** *)

impltmp
print$val<g0marg> x = print_g0marg x
impltmp
print$val<g0exp> x = print_g0exp x
impltmp
print$val<g0eid> x = print_i0dnt x
impltmp
show$val<g0marg> x = show_g0marg x
impltmp
show$val<g0exp> x = show_g0exp x
impltmp
show$val<g0eid> x = show_i0dnt x

(* ****** ****** *)

impltmp
print$val<eq0arg> x = print_eq0arg x
impltmp
print$val<eq0opt> x = print_eq0opt x
impltmp
show$val<eq0arg> x = show_eq0arg x
impltmp
show$val<eq0opt> x = show_eq0opt x

(* ****** ****** *)

implement
show_eq0arg(x0) =
(
  case+ x0.node() of
  | EQ0ARGnone(token) => print!("EQ0ARGnone()")
  | EQ0ARGsome(token(*'='*), s0exp) =>
    (
      show(token);
      (* show(s0exp) *)
    )
    (* print!("EQ0ARGsome(", token, "; ", s0exp, ")") *)
)

implement
show_eq0opt(x0) =
(
case+ x0.node() of
| EQ0ARGopt (token(*'='*), eq0argopt) =>
  (
    show_spc();
    show(token);
    show_spc();
    show$val<optn0(eq0arg)>(g0ofg1(eq0argopt))
  )
  (* print!("EQ0ARGopt(", token, "; ", eq0argopt, ")") *)
)


(* ****** ****** *)

// template arg
implement
show_q0arg(x0) =
(
//
case+
x0.node() of
(*
| Q0ARGnone(tok) =>
  fprint!(out, "Q0ARGnone(", tok, ")")
*)
| Q0ARGsome(sid, opt) =>
  (
    show(sid);
    (* show(opt) *)
    show$val<optn0(sort0)>(g0ofg1(opt)) where
    {
      impltmp show$before<>() = show_col()
    }
  )
  //print!("Q0ARGsome(", sid, "; ", opt, ")")
//
) (* end of [show_q0arg] *)

(* ****** ****** *)

implement
show_a0typ(x0) =
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
) (* end of [show_a0typ] *)

(* ****** ****** *)

local
//
fun
show_a0typlstopt
(opt: a0typlstopt): void =
(
case+ opt of
| optn1_none() => print!("None()")
| optn1_some(a0ts) => print!("Some(", a0ts, ")")
)
//
#symload print with show_a0typlstopt of 100
//
in (* in-of-local *)

implement
show_d0arg(x0) =
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
) (* end of [show_d0arg] *)

end // end of [local]

(* ****** ****** *)

implement
show_f0arg(x0) =
(
//
case+
x0.node() of
| F0ARGnone(tok) => ()
  (* print!("F0ARGnone(", tok, ")") *)
| F0ARGsome_dyn(d0p) => show(d0p)
  (* print!("F0ARGsome_dyn(", d0p, ")") *)
| F0ARGsome_sta(tbeg, s0qs, tend) =>
  (
    show(tbeg);
    show$val<list0(s0qua)>(g0ofg1(s0qs)) where
    {
      impltmp show$sep<>() = prout(";")
    };
    show(tend)
  )
  (* print!("F0ARGsome_sta(", tbeg, "; ", s0qs, "; ", tend, ")") *)
| F0ARGsome_met(tbeg, s0es, tend) =>
  print!("F0ARGsome_met(", tbeg, "; ", s0es, "; ", tend, ")")
//
) (* end of [show_f0arg] *)

(* ****** ****** *)

implement
show_sq0arg(x0) =
(
//
case+
x0.node() of
| SQ0ARGnone(tok) =>
  print!("SQ0ARGnone(", tok, ")")
(*
| SQ0ARGs0rtsome(q0as) =>
  (
    show$val<s0marg>(q0as)
  )
*)
| SQ0ARGsome(tbeg, q0as, tend) =>
  (
    show(tbeg);
    show$val<list0(q0arg)>(g0ofg1(q0as));
    show(tend)
  )
  (* print!("SQ0ARGsome(", tbeg, "; ", q0as, "; ", tend, ")") *)
//
) (* end of [show_sq0arg] *)

(* ****** ****** *)

implement
show_tq0arg(x0) =
(
//
case+
x0.node() of
| TQ0ARGnone(tok) =>
  print!("TQ0ARGnone(", tok, ")")
| TQ0ARGsome(tbeg, q0as, tend) =>
  (
    show(tbeg);
    show$val<list0(q0arg)>(g0ofg1(q0as));
    show(tend)
  )
  (* print!("TQ0ARGsome(", tbeg, "; ", q0as, "; ", tend, ")") *)
//
) (* end of [show_tq0arg] *)

(* ****** ****** *)

implement
show_ti0arg(x0) =
(
//
case+
x0.node() of
| TI0ARGnone(tok) => show(tok)
  (* print!("TI0ARGnone(", tok, ")") *)
| TI0ARGsome(tbeg, q0as, tend) =>
  (* print!("TI0ARGsome(", tbeg, "; ", q0as, "; ", tend, ")") *)
  (
    show(tbeg);
    show$val<list0(s0exp)>(g0ofg1(q0as)) where {
      impltmp show$sep<>() = prout(",")
    };
    show(tend)
  )
//
) (* end of [show_ti0arg] *)

(* ****** ****** *)

impltmp
{a}(*tmp*)
show_dl0abeled(x0) = let
//
val+DL0ABELED(l0, t0, x1) = x0
//
in
  print!("SL0ABELED(");
  print!(l0, ", ", t0, ", ");
  print$val<a>(x1); print!(")")
end // end of [show_dl0abeled]

(* ****** ****** *)

local

impltmp
print$val<d0pat> x = show_d0pat x

in (* in-of-local *)

implement
show_d0pat(x0) =
(
case+ x0.node() of
//
| D0Pid(id) => show(id)
  (* print!("D0Pid(", id, ")") *)
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
  (
    show$val<list0(d0pat)>(g0ofg1(d0ps)) where
    {
      impltmp show$sep<>() = ()
    }
  )
  (* print!("D0Papps(", d0ps, ")") *)
//
| D0Psqarg
  (tbeg, s0as, tend) =>
  print!("D0Psqarg("
  , tbeg, "; ", s0as, "; ", tend, ")")
//
| D0Pparen
  (tbeg, d0ps, tend) =>
  (
    show(tbeg);
    show$val<list0(d0pat)>(g0ofg1(d0ps)) where
    {
      impltmp show$sep<>() = prout(",");
    };
    show(tend)
  )
(*
  print!("D0Pparen("
  , tbeg, "; ", d0ps, "; ", tend, ")")
*)
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
| D0Panno // here is an annotation for template via '(a:tflt)'
  (d0p, ann) =>
  (
    show(d0p);
    show_col();
    show(ann)
  )
  (* print!("D0Panno(", d0p, "; ", ann, ")") *)
//
| D0Pqual
  (tok, d0p) =>
  print!("D0Pqual(", tok, "; ", d0p, ")")
//
| D0Pnone(tok) => //print!("D0Pnone(", tok, ")")
  show(tok)
//
) (* end of [show_d0pat] *)

end // end of [local]

(* ****** ****** *)

local

impltmp
print$val<d0pat> x = show_d0pat x

in (* in-of-local *)

implement
show_d0pat_RPAREN(x0) =
(
case+ x0 of
| d0pat_RPAREN_cons0(tok) => show(tok)
  (* print!("d0pat_RPAREN_cons0(", tok, ")") *)
| d0pat_RPAREN_cons1(tok1, d0ps, tok2) =>
  (
    show(tok1);
    show$val<list0(d0pat)>(g0ofg1(d0ps)) where
    {

    };
    show(tok2)
    (* show$val<optn0(s0exp)>(g0ofg1(tok2)) where *)
    (* { *)
    (* }; *)
  )
  (* print!("d0pat_RPAREN_cons1(", tok1, ", ", d0ps, ", ", tok2, ")") *)
) (* end of [show_d0pat_RPAREN] *)

end // end of [local]

(* ****** ****** *)

implement
show_d0gua(xs) =
(
  case+
  xs.node() of
  | D0GUAexp(d0exp) => show_d0exp(d0exp)
  | D0GUAmat(d0exp, token(*AS*), d0pat) =>
    (
      show_d0exp(d0exp);
      show_token(token);
      show_d0pat(d0pat)
    )
)



(*
datatype d0clau_node =
  | D0CLAUgpat of (dg0pat)
  | D0CLAUclau of (dg0pat, token(*EQGT*), d0exp)
  and
  dg0pat_node =
    | DG0PATpat of (d0pat)
    | DG0PATgua of (d0pat, token(*WHEN*), d0gualst)
*)

implement
show_d0clau(xs) =
(
  case+ xs.node() of
    | D0CLAUgpat(dg0pat) => show_dg0pat(dg0pat)
    | D0CLAUclau(dg0pat, token(*EQGT*), d0exp) =>
      (
        show_dg0pat(dg0pat);
        show_token(token);
        show_d0exp(d0exp)
      )
)

implement
show_dg0pat(xs) =
(
  case+ xs.node() of
    | DG0PATpat(d0pat) => show_d0pat(d0pat)
    | DG0PATgua(d0pat, token(*WHEN*), d0gualst) =>
      (
        show(d0pat);
        show_token(token);
        //show(d0gaulst)
        show$val<list0(d0gua)>(g0ofg1(d0gualst));
      )
)


(* ****** ****** *)

local

impltmp
print$val<d0pat> x = show_d0pat x

in (* in-of-local *)

implement
show_labd0pat_RBRACE(x0) =
(
case+ x0 of
| labd0pat_RBRACE_cons0(tok) =>
  print!("labd0pat_RBRACE_cons0(", tok, ")")
| labd0pat_RBRACE_cons1(tok1, ld0ps, tok2) =>
  print!("labd0pat_RBRACE_cons1(", tok1, ", ", ld0ps, ", ", tok2, ")")
) (* end of [show_labd0pat_RBRACE] *)

end // end of [local]

(* ****** ****** *)

local

impltmp
print$val<d0exp> x = show_d0exp x

in (* in-of-local *)

implement
show_d0exp(x0) =
(
case+ x0.node() of
//
| D0Eid(id) => show(id)
  (* print!("D0Eid(", id, ")") *)
//
| D0Eint(i0) => show_t0int(i0)
  (* print!("D0Eint(", i0, ")") *)
| D0Echr(c0) => show_t0chr(c0)
  (* print!("D0Echr(", c0, ")") *)
| D0Eflt(f0) => show(f0)
  (* print!("D0Eflt(", f0, ")") *)
| D0Estr(s0) => show(s0)
  (* print!("D0Estr(", s0, ")") *)
//
| D0Eapps(d0es) =>
  (
    show$val<list0(d0exp)>(g0ofg1(d0es)) where
    {
      impltmp show$sep<>() = ()
    }
  )
  (* print!("D0Eapps(", d0es, ")") *)
//
| D0Esqarg
  (tbeg, s0es, tend) =>
  (
    show(tbeg);
    show$val<list0(s0exp)>(g0ofg1(s0es));
    show(tend)
  )
(*
  print!
  ("D0Esqarg("
  , tbeg, "; ", s0es, "; ", tend, ")")
*)
| D0Etqarg
  (tbeg, s0es, tend) =>
  (
    show(tbeg);
    show$val<list0(s0exp)>(g0ofg1(s0es)) where
    {
      impltmp show$sep<>() = show_comma()//()
    };
    show(tend)
  )
  (* print!("D0Etqarg(" *)
  (* , tbeg, "; ", s0es, "; ", tend, ")") *)
//
| D0Eparen
  (tbeg, d0es, tend) =>
  (
    show(tbeg);
    show$val<list0(d0exp)>(g0ofg1(d0es)) where
    {
      impltmp show$sep<>() = show_comma();
    };
    show(tend)
  )

  (* print!("D0Eparen(" *)
  (* , tbeg, "; ", d0es, "; ", tend, ")") *)
//
| D0Etuple
  (tbeg, topt, d0es, tend) =>
  (
    show(tbeg);
    (* show(topt); *)
    show$val<optn0(token)>(g0ofg1(topt));
    show$val<list0(d0exp)>(g0ofg1(d0es));
    show(tend)
  )
(*
  print!("D0Etuple("
  , tbeg, "; ", topt, "; ", d0es, "; ", tend, ")")
*)
| D0Erecord
  (tbeg, topt, ld0es, tend) =>
  print!("D0Erecord("
  , tbeg, "; ", topt, "; ", ld0es, "; ", tend, ")")
//
| D0Eif0
  (tif0, d0e1, d0e2, d0e3, tend) =>
  (
    show(tif0);
    show_newline();
    show(d0e1);
    show_newline();
    show(d0e2);
    show_newline();
    show(d0e3);
    show$val<optn0(token)>(g0ofg1(tend))
  )
  (* ) where { val _ = $showtype(tend) } *)
(*
  print!("D0Eif0(", tif0, "; "
  , d0e1, "; ", d0e2, "; ", d0e3, "; ", tend, ")")
*)
//
| D0Ecase
  (tok0, d0e1, tof2, tbar, d0cs, tend) =>
  (
    show(tok0);
    show(d0e1);
    show_spc();
    show(tof2);
    //show_ident
    show_newline();
    show$val<optn0(token)>(g0ofg1(tbar)) where
    {
      impltmp show$after<>() = show_spc();
    };
    show$val<list0(d0clau)>(g0ofg1(d0cs)) where
    {
      impltmp show$sep<>() =
        (
          show_newline()
        ; show_bar()
        )
    };
    show$val<optn0(token)>(g0ofg1(tend));
  )
(*
  print!("D0Ecase(", tok0, "; "
  , d0e1, "; ", tof2, "; ", tbar, "; ", "...", "; ", tend, ")")
*)
//
| D0Elet
  (tok0, d0cs, tok1, d0es, tok2) =>
  (
    show(tok0);
    show_newline();
    show$val<list0(d0ecl)>(g0ofg1(d0cs)) where {
      impltmp show$sep<>() = prout("\n");
    };
    show_newline();
    show(tok1);
    show_newline();
    show$val<list0(d0exp)>(g0ofg1(d0es)) where {
    };
    show_newline();
    show(tok2);
    show_newline();
  )
(*
  print!("D0Elet(", tok0, "; "
  , d0cs, "; ", tok1, "; ", d0es, "; ", tok2, ")")
*)
//
| D0Ewhere(d0e1, d0cs) =>
  (
    show(d0e1);
    show_newline();
    show(d0cs)
  )
  (* print!("D0Ewhere(", d0e1, "; ", d0cs, ")") *)
//
| D0Edtsel
  (tok, lab, arg) =>
  (
    show(tok);
    show(lab);
    show$val<optn0(d0exp)>(g0ofg1(arg))
  )
  (* print!("D0Edtsel(", tok, "; ", lab, "; ", arg, ")") *)
//
| D0Elam
  (tok0, arg1, res2, farrw, fbody, tend) =>
  (
    show(tok0);
    (* show(arg1); *)
    show$val<list0(f0arg)>(g0ofg1(arg1)) where
    {

    };
    show(res2);
    show(farrw);
    show(fbody);
    (* show(tend) *)
    show$val<optn0(token)>(g0ofg1(tend)) where
    {

    };

  )
(*
  print!("D0Elam(", tok0, "; "
  , arg1, "; ", res2, "; ", farrw, "; ", fbody, "; ", tend, ")")
*)
//
| D0Eanno
  (d0e, ann) =>
  (
    show(d0e);
    show_col();
    show(ann)
  )
  (* print!("D0Eanno(", d0e, "; ", ann, ")") *)
//
| D0Equal
  (tok, d0e) =>
  (* print!("D0Equal(", tok, "; ", d0e, ")") *)
  (
    show(tok);
    show(d0e)
  )
//
| D0Enone(tok) => print!("\n") //print!("D0Enone(", tok, ")")
//
) (* end of [show_d0exp] *)

end // end of [local]

(* ****** ****** *)

local

impltmp
print$val<d0exp> x = show_d0exp x

in (* in-of-local *)

implement
show_d0exp_RPAREN(x0) =
(
case+ x0 of
| d0exp_RPAREN_cons0(tok) => show(tok)
  (* print!("d0exp_RPAREN_cons0(", tok, ")") *)
| d0exp_RPAREN_cons1(tok1, d0es, tok2) =>
  print!("d0exp_RPAREN_cons1(", tok1, ", ", d0es, ", ", tok2, ")")
| d0exp_RPAREN_cons2(tok1, d0es, tok2) =>
  (* print!("d0exp_RPAREN_cons2(", tok1, ", ", d0es, ", ", tok2, ")") *)
  (
    show(tok1);
    show$val<list0(d0exp)>(g0ofg1(d0es)) where
    {
      impltmp show$sep<>() = prout";"
    };
    show(tok2)
  )
) (* end of [show_d0exp_RPAREN] *)

end // end of [local]

(* ****** ****** *)

local

impltmp
print$val<d0exp> x = show_d0exp x

in (* in-of-local *)

implement
show_labd0exp_RBRACE(x0) =
(
case+ x0 of
| labd0exp_RBRACE_cons0(tok) =>
  print!("labd0exp_RBRACE_cons0(", tok, ")")
| labd0exp_RBRACE_cons1(tok1, ld0es, tok2) =>
  print!("labd0exp_RBRACE_cons1(", tok1, ", ", ld0es, ", ", tok2, ")")
) (* end of [show_labd0exp_RBRACE] *)

end // end of [local]

(* ****** ****** *)

implement
show_d0exp_THEN(x0) =
(
case+ x0 of
| d0exp_THEN(tok, d0e) =>
  (
    show(tok);
    show_spc();
    show(d0e)
  )
  (* print!("d0exp_THEN(", tok, "; ", d0e, ")") *)
) (* end of [show_d0exp_THEN] *)

(* ****** ****** *)

implement
show_d0exp_ELSE(x0) =
(
case+ x0 of
| d0exp_ELSEnone() => ()
  (* print!("d0exp_ELSEnone(", ")") *)
| d0exp_ELSEsome(tok, d0e) =>
  (
    show(tok);
    show_spc();
    show(d0e)
  )
  (* print!("d0exp_ELSEsome(", tok, "; ", d0e, ")") *)
) (* end of [show_d0exp_ELSE] *)

(* ****** ****** *)

implement
show_endwhere(x0) =
(
case+ x0 of
| endwhere_cons1(tok) =>
  (
    show_newline();
    show(tok);
  )
  (* print!("endwhere_cons1(", tok, ")") *)
| endwhere_cons2(tok1, opt2) =>
  (
    show_newline();
    show(tok1);
    show$val<optn0(token)>(g0ofg1(opt2)) where
    {
      impltmp show$before<>() = show_newline()
    };
  )

  (* print!("endwhere_cons2(", tok1, "; ", opt2, ")") *)
)
//
(* ****** ****** *)

implement
show_d0eclseq_WHERE(x0) =
(
case+ x0 of
| d0eclseq_WHERE
  (tok0, opt1, d0cs, opt2) =>
  (
    // here for newline or space
    (* spc(); *)
    //
    show(tok0);
    show$val<optn0(token)>(g0ofg1(opt1)) where
    {
      impltmp show$before<>() = show_newline()
    };
    show_newline();
    show$val<list0(d0ecl)>(g0ofg1(d0cs)) where
    {
      impltmp show$sep<>() = show_newline()
    };
    show(opt2)
  )
  (* print!("d0eclseq_WHERE(" *)
  (* , tok0, "; ", opt1, "; ", d0cs, "; ", opt2, ")") *)
) (* end of [show_d0eclseq_WHERE] *)
//
(* ****** ****** *)

implement
show_f0unarrow(x0) =
(
case+ x0 of
| F0UNARROWnone(tok) => show(tok)
  (* print!("F0UNARROWnone(", tok, ")") *)
| F0UNARROWdflt(tok) => show(tok)
  (* print!("F0UNARROWdflt(", tok, ")") *)
| F0UNARROWlist(tbeg, s0es, tend) =>
  print!("F0UNARROWlist(", tbeg, "; ", s0es, "; ", tend, ")")
) (* end of [show_f0unarrow] *)

(* ****** ****** *)

implement
show_declmodopt(x0) =
(
case+ x0 of
//
| DECLMODnone() => ()
  (* print!("DECLMODnone(", ")") *)
//
| DECLMODsing(tok, id0) =>
  print!("DECLMODsing(", tok, "; ", id0, ")")
| DECLMODlist(tok, tbeg, ids, tend) =>
  print!("DECLMODlist("
  , tok, "; ", tbeg, "; ", ids, "; ", tend, ")")
)

(* ****** ****** *)

implement
show_teqd0expopt(x0) =
(
case+ x0 of
| TEQD0EXPnone() => ()
  (* print!("TEQD0EXPnone(", ")") *)
| TEQD0EXPsome(tok, d0e) =>
  (
    show(tok);
    show_d0exp(d0e);
  )
  (* print!("TEQD0EXPsome(", tok, "; ", d0e, ")") *)
)

(* ****** ****** *)

implement
show_wths0expopt(x0) =
(
case+ x0 of
| WTHS0EXPnone() => ()
  (* print!("WTHS0EXPnone(", ")") *)
| WTHS0EXPsome(tok, d0e) =>
  print!("WTHS0EXPsome(", tok, "; ", d0e, ")")
)

(* ****** ****** *)

local

impltmp
print$val<d0ecl> x = show_d0ecl x
impltmp
print$val<v0aldecl> x = show_v0aldecl x
impltmp
print$val<v0ardecl> x = show_v0ardecl x
impltmp
print$val<f0undecl> x = show_f0undecl x
impltmp
print$val<d0cstdecl> x = show_d0cstdecl x
impltmp
show$val<d0ecl> x = show_d0ecl x
impltmp
show$val<v0aldecl> x = show_v0aldecl x
impltmp
show$val<v0ardecl> x = show_v0ardecl x
impltmp
show$val<f0undecl> x = show_f0undecl x
impltmp
show$val<d0cstdecl> x = show_d0cstdecl x

in (* in-of-local *)

implement
show_d0ecl(x0) =
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
  (
    show(tok);
    show_spc();
    show$val<list0(i0dnt)>(g0ofg1(ids));
  )
  (* print!("D0Cnonfix(", tok, "; ", ids, ")") *)
| D0Cfixity(tok, ids, opt) =>
  (* print!("D0Cfixity(", tok, "; ", ids, "; ", opt, ")") *)
  (
    show(tok);
    show_spc();
    show$val<list0(i0dnt)>(g0ofg1(ids));
    show(opt);
  )
//
| D0Cstatic(tok, d0c) =>
  print!("D0Cstatic(", tok, "; ", d0c, ")")
| D0Cextern(tok, d0c) =>
  (
    show(tok);
    spc();
    show(d0c) //where { val _ = $showtype d0c }
  )
  (* show_mac(tok, d0c) *)
  (* print!("D0Cextern(", tok, "; ", d0c, ")") *)
//
| D0Cdefine
  (tok, gid, gmas, gdef) =>
  (
    show(tok);
    show_spc();
    show(gid);
    show$val<list0(g0marg)>(g0ofg1(gmas)) where
    {
      impltmp show$before<>() = show_spc();
    };
    show(gdef)
  )
(*
  print!
  ("D0Cdefine(", gid, "; ", gmas, "; ", gdef, ")")
*)
//
| D0Cmacdef
  (tok, gid, gmas, mdef) =>
  print!
  ("D0Cmacdef("
  , gid, "; ", gmas, "; ", mdef, ")")

| D0Cinclude(tok, d0e) =>
  (
    show(tok);
    show_spc();
    show(d0e)
  )
  (* print!("D0Cinclude(", tok, "; ", d0e, ")") *)
//
| D0Cstaload(tok, d0e) =>
  (
    show(tok);
    show_spc();
    show(d0e);
  )
  (* print!("D0Cstaload(", tok, "; ", d0e, ")") *)
(*
| D0Cdynload(tok, d0e) =>
  print!("D0Cdynload(", tok, "; ", d0e, ")")
*)
//
| D0Cabssort(tok, tid) => show_mac(tok, tid)
  (* print!(tok, "; ", tid) *)
  //print!("D0Cabssort(", tok, "; ", tid, ")")
//
| D0Cstacst0
  (tok, sid, tmas, tok1, s0t2) =>
  print!("D0Cstacst0("
  , tok, "; ", sid, "; ", tmas, "; ", tok1, "; ", s0t2, ")")
//
| D0Csortdef
  (tok, tid, tok1, def2) => show_mac(tok, tid, tok1, def2)
(*
  (
    show(tok);
    print(" ");
    show(tid);
    print(" ");
    show(tok1);
    print(" ");
    show(def2)
  )
*)
(*
  {
    val () = show_token(tok);
    val () = print!("; ", tid, "; ", tok1, "; ", def2, ")")
  }
*)
  (* print!(tok, "; ", tid, "; ", tok1, "; ", def2, ")") *)
(*
  print!("D0Csortdef("
  , tok, "; ", tid, "; ", tok1, "; ", def2, ")")
*)
| D0Csexpdef
  ( tok, sid
  , arg, res, tok1, tdef) =>
  (
    show(tok);
    spc();
    show(sid);
    (* spc(); *)
    show$val<list0(s0marg)>(g0ofg1(arg)) where {
      impltmp show$sep<>() = ()
    };
    (* $showtype(res); *)
    show$val<optn0(sort0)>(g0ofg1(res)) where {
      impltmp show$sep<>() = prout(":")
    };
    spc();
    show(tok1);
    spc();
    show(tdef)
  )
  (* print!("D0Csexpdef(" *)
  (* , tok, "; ", sid, "; " *)
  (* , arg, "; ", res, "; ", tok1, "; ", tdef, ")") *)
//
| D0Cabstype
  (tok, sid, arg, res, tdef, eq0opt) =>
  (
    show(tok);
    spc();
    show(sid);
    spc();
    show$val<list0(t0marg)>(g0ofg1(arg));
    (* $showtype(res); *)
    show$val<optn0(sort0)>(g0ofg1(res));
    show(tdef);
    show(eq0opt)
  )
(*
  print!("D0Cabstype("
  , tok, "; ", sid, "; ", arg, "; ", res, "; ", tdef, ")")
*)
//
| D0Cabsimpl
  (tok, sqid, smas, res0, teq1, def2) =>
  (
    show(tok);
    show_spc();
    show(sqid);
    //show(smas);
    show$val<list0(s0marg)>(g0ofg1(smas));
    //show(res0);
    show$val<optn0(sort0)>(g0ofg1(res0));
    show_spc();
    show(teq1);
    show_spc();
    show(def2)
  )
  (*
  print!("D0Cabsimpl("
  , tok, "; ", sqid, "; "
  , smas, "; ", res0, "; ", teq1, "; ", def2, ")")
*)
//
| D0Cvaldecl
  (tok, mopt, d0cs) =>
  (
    show(tok);
    (* show_spc(); *)
    show(mopt);
    show$val<list0(v0aldecl)>(g0ofg1(d0cs)) where
    {
      //impltmp show$sep<>() = prout("\n")
    }
    (*
    where
    {
      impltmp show$sep<>() = ()
    };
    *)
  )
  (* print!("D0Cvaldecl(", tok, "; ", mopt, "; ", d0cs, ")") *)
//
| D0Cvardecl
  (tok, d0cs) =>
  (
    show(tok);
    show_spc();
    show$val<list0(v0ardecl)>(g0ofg1(d0cs))
  )
(*
  (
    print!("D0Cvardecl(", tok, "; ", d0cs, ")")
  ) (*D0Cvardecl*)
*)

//
| D0Cfundecl
  (tok, mopt, tqas, d0cs) =>
  (
    (* show_newline(); *)
    show(tok);
    (* spc(); *)
    (if iseqz(g0ofg1(tqas)) then show_spc());
    show(mopt);
    (* spc(); *)
    show$val<list0(tq0arg)>(g0ofg1(tqas)) where
    {
      impltmp show$sep<>() = ()
      impltmp show$end<>() = (if isneqz tqas then prout(" ") else ())
    };
    show$val<list0(f0undecl)>(g0ofg1(d0cs)) where
    {
      impltmp show$sep<>() =
      (
        show_newline();
        prout("and");
        show_newline()
      )
    }
  )
  (* print!("D0Cfundecl(", tok, "; ", mopt, "; ", tqas, "; ", d0cs, ")") *)
//
| D0Cimpdecl
  ( tok, mopt
  //, s0as
  , sqas, tqas
  , dqid, tias, f0as, res0, teq1, d0e2) =>
  (
    show(tok);
    show(mopt);
    (* show$val<s0marg>(s0as); *)
    show$val<list0(sq0arg)>(g0ofg1(sqas));
    show$val<list0(tq0arg)>(g0ofg1(tqas));
    spc();
    show(dqid);
    show$val<list0(ti0arg)>(g0ofg1(tias)) where
    {
      impltmp show$sep<>() = ()
    };
    show$val<list0(f0arg)>(g0ofg1(f0as)) where
    {
      impltmp show$sep<>() = show_comma();
    };

    show(res0);

    show_spc();
    show(teq1);
    show_newline();


    show(d0e2)

  )
  (* where *)
  (*   { *)
  (*     impltmp show$sep<>() = () *)
  (*   } *)
(*
  print!("D0Cimpdecl("
  , tok, "; ", mopt, "; ", sqas, "; ", tqas, "; "
  , dqid, "; ", tias, "; ", f0as, "; ", res0, "; ", teq1, "; ", d0e2, ")")
*)
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
  (
    show(tok);
    show_spc();
    show$val<list0(d0atype)>(g0ofg1(d0cs)) where {
      impltmp show$sep<>() = (prout("\n"); prout("and"); prout("\n"))
    };
    (
      case+ wopt of
      | WD0CSnone() => ()
      | _ => (prout("\n"); show(wopt))
    )
  )
  (* print!("D0Cdatatype(", tok, "; ", d0cs, "; ", wopt, ")") *)
//
| D0Cdynconst
  (tok, tqas, d0cs) =>
  print!("D0Cdynconst(", tok, "; ", tqas, "; ", d0cs, ")")
//
| D0Clocal
  (tok, d0cs0, tok1, d0cs1, tok2) =>
  (
    show(tok);
    show_newline();
    // here all can be indented
    show$val<list0(d0ecl)>(g0ofg1(d0cs0)) where {
      impltmp show$sep<>() = show_newline();
    };
    show_newline();
    show(tok1);
    show_newline();
    show$val<list0(d0ecl)>(g0ofg1(d0cs1)) where {
      impltmp show$sep<>() = show_newline();
    };
    show_newline();
    show(tok2)
  )
    (* print!("D0Clocal(" *)
  (* , tok, "; ", d0cs0, "; ", tok1, "; ", d0cs1, "; ", tok2, ")") *)
//
(*
| _(*rest-of-d1ecl*) =>
    print!("show_d1ecl: D0C...: not-yet-implemented")
*)
//
) (* end of [show_d0ecl] *)
where
{

(*
impltmp
{a}
print$val(x) = show$val<a>(x)
*)
(* fprint$val<a>(the_stdout<>(), x) *)

}
end // end of [local]

(* ****** ****** *)

impltmp
print$val<precmod> x = show_precmod x
impltmp
print$val<i0dnt> x = show_i0dnt x

implement
show_precopt(x0) =
(
case+ x0 of
| PRECOPTnil() => ()
  (* print!("PRECOPTnil()") *)
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
) (* end of [show_precopt] *)
//
implement
show_signint(x0) =
(
case+ x0 of
| SIGNINTint(tint) =>
  print!("SIGNINTint(", tint, ")")
| SIGNINTopr(topr, tint) =>
  print!("SIGNINTopr(", topr, "; ", tint, ")")
)
//
implement
show_precmod(x0) =
(
case+ x0 of
| PRECMODnone() => ()
  (* print!("PRECMODnone()") *)
| PRECMODsome(tbeg, sint, tend) =>
  let
    (* val _ = $showtype(tbeg) *)
  in
  print!("PRECMODsome(", tbeg, "; ", sint, "; ", tend, ")")
  end
)
//
(* ****** ****** *)

implement
show_abstdf0(x0) =
(
case+ x0 of
| ABSTDF0nil() => ()
  (* print("ABSTDF0nil()") *)
| ABSTDF0lteq(tok, s0e) =>
  print!("ABSTDF0lteq(", tok, "; ", s0e, ")")
| ABSTDF0eqeq(tok, s0e) =>
  print!("ABSTDF0eqeq(", tok, "; ", s0e, ")")
) (* end of [show_abstdf0] *)

(* ****** ****** *)


implement
show_g0expdef(x0) =
(
case+ x0 of
| G0EDEFnone() => ()
  (* print("G0EDEFnone()") *)
| G0EDEFsome(tokopt, g0exp(*def*)) =>
  (
    show_spc();
    show$val<optn0(token)>(g0ofg1(tokopt));
    show(g0exp)
  )
  (* print!("G0EDEFsome(", tokopt, "; ", g0exp, ")") *)
)

implement
show_d0macdef(x0) =
(
case+ x0 of
| D0MDEFnone() =>
  print("D0MDEFnone()")
| D0MDEFsome(tokopt, d0exp(*def*)) =>
  print!("D0MDEFsome(", tokopt, "; ", d0exp,")")
)


(* ****** ****** *)

implement
show_wd0eclseq(x0) =
(
case+ x0 of
| WD0CSnone() => ()
  (* print("WD0CSnone()") *)
| WD0CSsome(tbeg, topt, d0cs, tend) =>
  (
    show(tbeg);
    show_newline();
    (* show(topt); *)
    show$val<optn0(token)>(g0ofg1(topt));
    show$val<list0(d0ecl)>(g0ofg1(d0cs)) where
    {
      impltmp show$sep<>() = prout("\n");
    };
    show_newline();
    show(tend);
  )

(*
  print!("WD0CSsome("
  , tbeg, "; ", topt, "; ", d0cs, "; ", tend, ")")
*)
) (* end of [show_wd0eclseq] *)

(* ****** ****** *)

implement
show_v0aldecl(x0) = let
//
val+V0ALDECL(rcd) = x0
//
//val _ = $showtype(rcd.pat)
in
(
  (
    case+ (rcd.pat).node() of
    | D0Papps(xs) => ()
    | _ => show_spc()
  );
  show(rcd.pat);
  show_spc();
  show(rcd.teq);
  show_spc();
  show(rcd.def);
(*
  show$val<list0(f0arg)>(g0ofg1(rcd.arg));
  prout(" : ");
  show(rcd.res);
*)
  (* show(rcd.teq); *)
  (* show(rcd.def); *)
  (* show(rcd.wtp); *)
)
  (* print!("V0ALDECL@{" *)
  (* , ", pat=", rcd.pat, ", teq=", rcd.teq *)
  (* , ", def=", rcd.def, ", wtp=", rcd.wtp, "}") *)
end // end of [show_v0aldecl]

(* ****** ****** *)

implement
show_v0ardecl(x0) = let
//
val+V0ARDECL(rcd) = x0
//
in
(
  show(rcd.nam);
  show$val<optn0(i0dnt)>(g0ofg1(rcd.wth));
  show$val<optn0(s0exp)>(g0ofg1(rcd.res)) where
  {
    impltmp show$before<>() = prout(": ")
  };
  show(rcd.ini);
)
(*
  print!("V0ARDECL@{"
  , ", nam=", rcd.nam
  , ", wth=", rcd.wth
  , ", res=", rcd.res, ", ini=", rcd.ini, "}")
*)
end // end of [show_v0ardecl]

(* ****** ****** *)

implement
show_f0undecl(x0) = let
//
val+F0UNDECL(rcd) = x0
//
(* val _ = $showtype(rcd.arg) *)
in
(
  show(rcd.nam);
  show$val<list0(f0arg)>(g0ofg1(rcd.arg)) where
  {
    impltmp show$sep<>() = ()
    //impltmp show$sep<>() = show_col()
  };
  (* show_col(); *)
  show(rcd.res);
  (
    case+ token_get_node(rcd.teq) of
    | T_EQ() =>
    (
    show_spc();
    show(rcd.teq);
    show_newline();

    show(rcd.def);// where { val _ = $showtype rcd.def };
    show(rcd.wtp);
    )
    | _ => ()
  )

)(*
  print!("F0UNDECL@{"
  , ", nam=", rcd.nam
  , ", arg=", rcd.arg
  , ", res=", rcd.res, ", teq=", rcd.teq
  , ", def=", rcd.def, ", wtp=", rcd.wtp, "}")
*)
end // end of [show_f0undecl]

(* ****** ****** *)

implement
show_d0cstdecl(x0) = let
//
val+D0CSTDECL(rcd) = x0
//
in
  print!("D0CSTDECL@{"
  , ", nam=", rcd.nam, ", arg=", rcd.arg
  , ", res=", rcd.res, ", def=", rcd.def, "}")
end // end of [show_d0cstdecl]

(* ****** ****** *)

(* end of [xats_dynexp0_print.dats] *)
