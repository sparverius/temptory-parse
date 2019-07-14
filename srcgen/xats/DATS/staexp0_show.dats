

#include "share/HATS/temptory_staload_bucs320.hats"

(* ****** ****** *)

#staload UN = "libats/SATS/unsafe.sats"

(* ****** ****** *)
//
#staload "./../SATS/basics.sats"
#staload _ = "./../DATS/basics.dats"
#staload "./../SATS/label0.sats"
#staload _ = "./label0.dats"
#staload "./../SATS/lexing.sats"
#staload "./../SATS/staexp0.sats"
#staload _ = "./staexp0.dats"
(* #staload _ = "./staexp0_print.dats" *)


#staload _ = "./../DATS/conf.dats"


//
(* ****** ****** *)

#macdef prout(x) = fprint$val<string>(the_stdout<>(), ,(x))


impltmp show$before<>() = ()
impltmp show$after<>() = ()

impltmp show$beg<>() = ()
impltmp show$end<>() = ()

fun show_newline() = prout("\n")
fun show_spc() = prout(" ")
fun show_col() = colon$sep<>() //prout(":")
fun show_comma() = (prout(","); comma$sep<>())
fun show_bar() = (prout("|"); bar$sep<>())
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
then `(show ,(car! xs); ,(auxlist (cdr! xs, y))) else y
// end of [if]
//
) (* end of [auxlist] *)
//
in (* in of [local] *)

#macdef
show_mac (x) =
,(
  if islist! (x) then auxlist (x, `()) else `(show ,(x))
) (* end of [print_mac] *)

#macdef
println_mac (x) =
,(
  if islist! (x)
    then auxlist (x, `(show_newline())) else `(show ,(x); show_newline())
  // end of [if]
) (* end of [println_mac] *)

end // end of [local]
//



#macdef spc() = fprint$val<char>(the_stdout<>(), ' ')


impltmp show$sep<>() = fprint$val<string>(the_stdout<>(), " ")

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

impltmp
(a:tflt)
print$val<list1(a)>(xs) = list1_print<a>(xs)

impltmp
(a:vtflt)
print$val<list1_vt(a)> = list1_vt_print<a>



(* ****** ****** *)
//

fun show_int(x: int) = fprint$val<int>(the_stdout<>(), x)
fun show_string(x: string) = fprint$val<string>(the_stdout<>(), x)
fun show_char(x: char) = fprint$val<char>(the_stdout<>(), x)

#symload show with show_int
#symload show with show_string
#symload show with show_char

impltmp
(a:vtflt)
print$val<list1_vt(a)> = list1_vt_print<a>

extern fun{a:vtflt} list1_vt_show : (!list1_vt a) -> void
extern fun{a:tflt} list1_show : (list1(a)) -> void

extern fun{a:vtflt} list0_vt_show : (!list0_vt a) -> void
extern fun{a:tflt} list0_show : (list0(a)) -> void

extern fun{a:tflt} optn0_show : (optn0(a)) -> void
extern fun{a:tflt} optn1_show : (optn1(a)) -> void

impltmp {a} optn1_show(t0) =
(
  case+ t0 of
    | optn1_none() => ()
    | optn1_some(x0) => (show$before<>(); show$val<a>(x0); show$after<>())
)

impltmp {a} optn0_show(t0) =
(
  case+ t0 of
    | optn0_none() => ()
    | optn0_some(x0) => (show$before<>(); show$val<a>(x0); show$after<>())
)

impltmp(a:tflt) show$val<optn0(a)>(x) = optn0_show<a>(x)
impltmp(a:tflt) show$val<optn1(a)>(x) = optn1_show<a>(x)


impltmp(a:vtflt) list1_vt_show<a>(xs) =
(
show$beg<>();
loop(0, xs);
show$end<>();
) where
{
  fun loop(i0: int, xs: !list1_vt(a)): void =
  (
  case+ xs of
  | list1_vt_nil() => ()
  | list1_vt_cons(x0, xs) =>
    (
    if
    (i0 > 0)
    then show$sep<>();
    show$val<a>(x0); loop(i0+1, xs)
    )
  )
}

impltmp(a:tflt) list1_show<a>(xs) =
(
show$beg<>();
loop(0, xs);
show$end<>();
) where
{
  fun loop(i0: int, xs: !list1(a)): void =
  (
  case+ xs of
  | list1_nil() => ()
  | list1_cons(x0, xs) =>
    (
    show$before<>();
    (if(i0 > 0) then show$sep<>());
    show$val<a>(x0);
    show$after<>();
    loop(i0+1, xs)
    )
  )
}

impltmp(a:vtflt) list0_vt_show<a>(xs) =
(
show$beg<>();
loop(0, xs);
show$end<>();
) where
{
  fun loop(i0: int, xs: !list0_vt(a)): void =
  (
  case+ xs of
  | list0_vt_nil() => ()
  | list0_vt_cons(x0, xs) =>
    (
    (if(i0 > 0) then show$sep<>());
    show$before<>();
    show$val<a>(x0);
    show$after<>();
    loop(i0+1, xs)

(*
    if
    (i0 > 0)
    then show$sep<>();
    show$val<a>(x0); loop(i0+1, xs)
*)
    )
  )
}

impltmp(a:tflt) list0_show<a>(xs) =
(
show$beg<>();
loop(0, xs);
show$end<>();
) where
{
  fun loop(i0: int, xs: !list0(a)): void =
  (
  case+ xs of
  | list0_nil() => ()
  | list0_cons(x0, xs) =>
    (
    (if(i0 > 0) then show$sep<>());
    show$before<>();
    show$val<a>(x0);
    show$after<>();
    loop(i0+1, xs)
    )
(*
    (
    if
    (i0 > 0)
    then show$sep<>();
    show$val<a>(x0); loop(i0+1, xs)
    )
*)
  )
}

#symload show with list1_vt_show
#symload show with list1_show

#symload show with list0_vt_show
#symload show with list0_show


impltmp
(a:tflt)
print$val<list1(a)>(xs) = list1_show<a>(xs)
impltmp
(a:vtflt)
print$val<list1_vt(a)> = list1_vt_show<a>

impltmp
(a:tflt)
show$val<list1(a)>(xs) = list1_show<a>(xs)
impltmp
(a:vtflt)
show$val<list1_vt(a)> = list1_vt_show<a>


impltmp
(a:tflt)
print$val<list0(a)>(xs) = list0_show<a>(xs)
impltmp
(a:vtflt)
print$val<list0_vt(a)> = list0_vt_show<a>

impltmp
(a:tflt)
show$val<list0(a)>(xs) = list0_show<a>(xs)
impltmp
(a:vtflt)
show$val<list0_vt(a)> = list0_vt_show<a>


impltmp
print$val<token> x = show_token x
impltmp
show$val<token> x = show_token x
//
impltmp
print$val<t0int> x = show_t0int x
impltmp
print$val<t0chr> x = show_t0chr x
impltmp
print$val<t0flt> x = show_t0flt x
impltmp
print$val<t0str> x = show_t0str x
impltmp
show$val<t0int> x = show_t0int x
impltmp
show$val<t0chr> x = show_t0chr x
impltmp
show$val<t0flt> x = show_t0flt x
impltmp
show$val<t0str> x = show_t0str x
//
impltmp
print$val<i0dnt> x = show_i0dnt x
impltmp
show$val<i0dnt> x = show_i0dnt x
//
impltmp
print$val<l0abl> x = show_l0abl x
impltmp
show$val<l0abl> x = show_l0abl x
//
impltmp
print$val<s0ymb> x = show_s0ymb x
impltmp
show$val<s0ymb> x = show_s0ymb x
//
(* ****** ****** *)

impltmp
print$val<sort0> x = show_sort0 x
impltmp
show$val<sort0> x = show_sort0 x

(* ****** ****** *)

impltmp
print$val<s0rtcon> x = show_s0rtcon x
impltmp
print$val<d0tsort> x = show_d0tsort x
impltmp
print$val<s0rtdef> x = show_s0rtdef x
impltmp
show$val<s0rtcon> x = show_s0rtcon x
impltmp
show$val<d0tsort> x = show_d0tsort x
impltmp
show$val<s0rtdef> x = show_s0rtdef x

(* ****** ****** *)
//
impltmp
print$val<s0arg> x = show_s0arg x
impltmp
print$val<s0marg> x = show_s0marg x
impltmp
show$val<s0arg> x = show_s0arg x
impltmp
show$val<s0marg> x = show_s0marg x
//
impltmp
print$val<t0arg> x = show_t0arg x
impltmp
print$val<t0marg> x = show_t0marg x
impltmp
show$val<t0arg> x = show_t0arg x
impltmp
show$val<t0marg> x = show_t0marg x
//
(* ****** ****** *)

impltmp
print$val<s0qua> x = show_s0qua x
impltmp
print$val<s0uni> x = show_s0uni x
impltmp
show$val<s0qua> x = show_s0qua x
impltmp
show$val<s0uni> x = show_s0uni x

impltmp
print$val<s0exp> x = show_s0exp x
impltmp
show$val<s0exp> x = show_s0exp x

(* ****** ****** *)

impltmp
print$val<d0atype> x = show_d0atype x
impltmp
print$val<d0atcon> x = show_d0atcon x
impltmp
show$val<d0atype> x = show_d0atype x
impltmp
show$val<d0atcon> x = show_d0atcon x

impltmp
print$val<d0atype> x = show_d0atype x

(* ****** ****** *)

impltmp
(a)//tmp
print$val<sl0abled(a)> x = show_sl0abled<a> x
impltmp
(a)//tmp
show$val<sl0abled(a)> x = show_sl0abled<a> x

(* ****** ****** *)


impltmp
print$val<g0marg> x = print_g0marg x
impltmp
print$val<g0exp> x = print_g0exp x
impltmp
print$val<g0eid> x = print_i0dnt x

(* ****** ****** *)

impltmp
print$val<s0quasopt> x = print_s0quasopt x


implement
show_s0quasopt(x0) =
(
  case+ x0.node() of
  | S0QUASnone() => print!("S0QUASnone()")
  | S0QUASsome(s0qualst) =>
    print!("S0QUASsome(", s0qualst, ")")
)

(* ****** ****** *)

impltmp
show$val<t0int> x = show_t0int x


implement
show_t0int(x0) =
(
case+ x0.node() of
| T0INTnone(tok) => show(tok)
  (* print!("T0INTnone(", tok, ")") *)
| T0INTsome(tok) => show(tok)

  (* print!("T0INTsome(", tok, ")") *)
)

(* ****** ****** *)

implement
show_t0chr(x0) =
(
case+ x0.node() of
| T0CHRnone(tok) =>
  print!("T0CHRnone(", tok, ")")
| T0CHRsome(tok) => show(tok)
  (* print!("T0CHRsome(", tok, ")") *)
)

(* ****** ****** *)

implement
show_t0flt(x0) =
(
case+ x0.node() of
| T0FLTnone(tok) =>
  print!("T0FLTnone(", tok, ")")
| T0FLTsome(tok) => show(tok)
  (* print!("T0FLTsome(", tok, ")") *)
)

(* ****** ****** *)


implement
show_t0str(x0) =
(
case+ x0.node() of
| T0STRnone(tok) =>
  print!("T0STRnone(", tok, ")")
| T0STRsome(tok) =>
  (
  show(tok)
  )
  (* print!("T0STRsome(", tok, ")") *)
)

(* ****** ****** *)

implement
show_i0dnt(x0) =
(
case+ x0.node() of
| I0DNTnone(tok) => ()//show(tok)
  (* print!("I0DNTnone(", tok, ")") *)
| I0DNTsome(tok) => show(tok)
  (* print!("I0DNTsome(", tok, ")") *)
)

(* ****** ****** *)
//
implement
show_l0abl(l0) =
(
case+
l0.node() of
| L0ABsome(lab) => show(lab)
  (* print!("L0ABsome(", lab, ")") *)
| L0ABnone(tok) =>
  print!("L0ABnone(", tok, ")")
)
//
(* ****** ****** *)
//
implement
show_s0ymb(x0) =
(
case+
x0.node() of
//
| S0YMBi0dnt(id0) => show(id0)
  (* print!("S0YMBi0dnt(", id0, ")") *)
//
| S0YMBdtlab(dot1, lab2) =>
  print!("S0YMBdtlab(", dot1, "; ", lab2, ")")
| S0YMBbrack(tok1, tok2) =>
  print!("S0YMBbrack(", tok1, "; ", tok2, ")")
//
) (* end of [show_s0ymb] *)
//
(* ****** ****** *)

implement
show_sq0eid(x0) =
(
case+ x0 of
| SQ0EIDnone(sid) => show(sid)
  (* print!("SQ0EIDnone(", sid, ")") *)
| SQ0EIDsome(tok, sid) =>
  print!("SQ0EIDsome(", tok, "; ", sid, ")")
)

(* ****** ****** *)

implement
show_dq0eid(x0) =
(
case+ x0 of
| DQ0EIDnone(sid) => show(sid)
  (* print!("DQ0EIDnone(", sid, ")") *)
| DQ0EIDsome(tok, sid) =>
  print!("DQ0EIDsome(", tok, "; ", sid, ")")
)

(* ****** ****** *)

(* ****** ****** *)

local

impltmp
print$val<g0exp> x = print_g0exp x
impltmp
show$val<g0exp> x = show_g0exp x

in (* in-of-local *)

implement
show_g0exp(x0) =
(
case+ x0.node() of
//
| G0Eid(tid) => show(tid)
  (* print!("G0Eid(", tid, ")") *)
//
| G0Eint(int) => show_t0int(int)
  (* print!("G0Eint(", int, ")") *)
//
| G0Eapps(s0ts) =>
  (
    show$val<list0(g0exp)>(g0ofg1(s0ts));
  )
  (* print!("G0Eapps(", s0ts, ")") *)
//
| G0Elist(t0, g0es, t1) =>
  (
    show(t0);
    show$val<list0(g0exp)>(g0ofg1(g0es));
    show(t1)
  )
(*
  print!
  ("G0Elist(", t0, "; ", g0es, "; ", t1, ")")
*)
//
| G0Enone(tok) => show(tok)
  (* print!("G0Enone(", tok, ")" ) *)
  // end of [G0Enone]
//
) (* end of [fprint_g0exp] *)

end // end of [local]

(* ****** ****** *)

(*
implement
print_g0marg(x0) =
fprint_g0marg(stdout_ref, x0)
implement
prerr_g0marg(x0) =
fprint_g0marg(stderr_ref, x0)
*)

local

impltmp
print$val<g0eid> x = print_i0dnt x

impltmp
show$val<g0eid> x = show_i0dnt x

in (* in-of-local *)

implement
show_g0marg(x0) =
(
case+
x0.node() of
| G0MARGnone(tok) => ()
(*
  print!
  ("G0MARGnone(", tok, ")")
*)
| G0MARGsarg(tbeg, g0as, tend) =>
  (
    show(tbeg);
    show$val<list0(i0dnt)>(g0ofg1(g0as));
    show(tend)
  )
(*
  print!
  ("G0MARGsarg(", tbeg, "; ", g0as, "; ", tend, ")")
*)
| G0MARGdarg(tbeg, g0as, tend) =>
  (
    show(tbeg);
    show$val<list0(i0dnt)>(g0ofg1(g0as));
    show(tend)
  )

(*
  print!
  ("G0MARGdarg(", tbeg, "; ", g0as, "; ", tend, ")")
*)
) (* fprint_g0marg *)

(*
implement
print_g0marglst(xs) =
(
  list1_print<g0marg>(xs)
)
*)

end // end of [local]

(* ****** ****** *)

local

impltmp
print$val<sort0> x = show_sort0 x

impltmp
show$val<int> x = fprint$val<int>(the_stdout<>(), x)

in (* in-of-local *)

implement
show_sort0(x0) =
(
case+ x0.node() of
//
| S0Tid(tid) => show(tid)
  (* print!("S0Tid(", tid, ")") *)
//
| S0Tint(int) => show(int)
  (* print!("S0Tint(", int, ")") *)
//
| S0Tapps(s0ts) => show$val<list0(sort0)>(g0ofg1(s0ts)) where
{
  impltmp show$sep<>() = ()//fprint$val<string>(the_stdout<>(), " ")
}
  (* print!("S0Tapps(", s0ts, ")") *)
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
) (* end of [show_sort0] *)

end // end of [local]

(* ****** ****** *)

implement
show_s0rtcon(x0) =
(
case+ x0.node() of
| S0RTCON(sid, opt) =>
  (
    show(sid);
    show$val<optn0(sort0)>(g0ofg1(opt)) where {
    };

  )
  (* print!("S0RTCON(", sid, ", ", opt, ")") *)
) (* end of [show_s0rtcon] *)

(* ****** ****** *)

implement
show_d0tsort(x0) =
(
case+ x0.node() of
| D0TSORT(tid, tok, s0cs) =>
  (
    show(tid);
    show(tok);
    show_newline();
    show$val<list0(s0rtcon)>(g0ofg1(s0cs)) where {
      impltmp show$before<>() = prout("| ")
      impltmp show$sep<>() = (show_newline(); )
    };
  )
  (* print!("D0TSORT(", tid, "; ", tok, "; ", s0cs, ")") *)
) (* end of [show_d0tsort] *)

(* ****** ****** *)

implement
show_s0rtdef(x0) =
(
case+ x0.node() of
| S0RTDEFsort(s0t) => show(s0t)
  (* print!("S0RTDEFsort(", s0t, ")") *)
| S0RTDEFsbst(tbeg, s0a0, tbar, s0es, tend) =>
  (
    show(tbeg);
    show(s0a0);
    show(tbar);
    show$val<list0(s0exp)>(g0ofg1(s0es)) where {
    };
    show(tend)
  )
(*
  print!
  ("S0RTDEFsbst("
  , tbeg, "; ", s0a0, "; ", tbar, "; ", s0es, "; ", tend, ")")
*)
) (* end of [show_s0rtdef] *)

(* ****** ****** *)

implement
show_s0arg(x0) =
(
case+
x0.node() of
| S0ARGnone(tok) =>
  print!("S0ARGnone(", tok, ")")
| S0ARGsome(sid, opt) =>
  (
    show(sid);
    show$val<optn0(sort0)>(g0ofg1(opt)) where {
      impltmp show$before<>() = show_col()
    }
    (* show(opt) *)
  )
  (* print!("S0ARGsome(", sid, ", ", opt, ")") *)
) (* show_s0arg *)

implement
show_s0marg(x0) =
(
case+
x0.node() of
| S0MARGnone(tok) => ()
  (* print!("S0MARGnone(", tok, ")") *)
| S0MARGsing(tid) => show(tid)
  (* print!("S0MARGsing(", tid, ")") *)
| S0MARGlist(tbeg, s0as, tend) =>
  (
    show(tbeg);
    show$val<list0(s0arg)>(g0ofg1(s0as)) where {
      impltmp show$sep<>() = show_comma()//prout(",")
    };
    show(tend)
  )
  //print!("S0MARGlist(", tbeg, "; ", s0as, "; ", tend, ")")
) (* show_s0marg *)

(* ****** ****** *)

implement
show_t0arg(x0) =
(
case+
x0.node() of
| T0ARGsome(s0t, opt) =>
  (

    (* show(opt) *)
    (* show_newline(); *)
    show$val<optn0(token)>(g0ofg1(opt)) where {
      impltmp show$sep<>() = () //prout(":") //()
      impltmp show$after<>() = show_col()//prout(":")
    };

    show(s0t);
  )
  (* print!("T0ARGsome(", s0t, ", ", opt, ")") *)
) (* show_t0arg *)

implement
show_t0marg(x0) =
(
case+
x0.node() of
| T0MARGnone(tok) =>
  print!("T0MARGnone(", tok, ")")
| T0MARGlist(tbeg, t0as, tend) =>
  (
    show(tbeg);
    show$val<list0(t0arg)>(g0ofg1(t0as)) where {
      impltmp show$sep<>() = show_comma() //prout(",")//()
    };
    show(tend)
  )
  (* print!("T0MARGlist(", tbeg, ", ", t0as, ", ", tend, ")") *)
) (* show_t0marg *)

(* ****** ****** *)

implement
show_s0qua(x0) =
(
case+ x0.node() of
| S0QUAprop(s0e) => show(s0e)
  (* print!("S0QUAprop(", s0e, ")") *)
| S0QUAvars(ids, opt) =>
  (
    show$val<list0(i0dnt)>(g0ofg1(ids)) where
    {
      impltmp show$sep<>() = show_comma()//prout(",");
    };
    //print(":");
    show_col();
    show$val<optn0(sort0)>(g0ofg1(opt))
    (* show(opt); *)
    (* show(opt) *)
  )
  (* print!("S0QUAvars(", ids, "; ", opt, ")") *)
)

(* ****** ****** *)

implement
show_s0uni(x0) =
(
case+ x0.node() of
| S0UNInone(tok) =>
  print!("S0UNInone(", tok, ")")
| S0UNIsome(tbeg, s0qs, tend) =>
  (
    show(tbeg);
    show$val<list0(s0qua)>(g0ofg1(s0qs));
    show(tend);
    show_spc();
  )
  (* print!("S0UNIsome(", tbeg, "; ", s0qs, "; ", tend, ")") *)
)

(* ****** ****** *)

impltmp
{a}(*tmp*)
show_sl0abled(x0) = let
//
val+SL0ABLED(l0, t0, x1) = x0
//
in
  print!("SL0ABLED(");
  print!(l0, ", ", t0, ", ");
  show$val<a>(x1); print!(")")
end // end of [show_sl0abled]

(* ****** ****** *)

local

(*
impltmp
print$val<s0exp> x = show_s0exp x
*)

in (* in-of-local *)

implement
show_s0exp(x0) =
(
case+ x0.node() of
//
| S0Eid(sid) => show(sid)
  (* print!("S0Eid(", sid, ")") *)
//
| S0Eop1(opid) => show(opid)
  (* print!("S0Eop1(", opid, ")") *)
| S0Eop2(tbeg, opid, tend) =>
  (
    show(tbeg);
    show(opid);
    show(tend);
  )
  (* print!("S0Eop2(", tbeg, "; ", opid, "; ", tend, ")") *)
//
| S0Eint(i0) => show_t0int(i0)
  (* print!("S0Eint(", i0, ")") *)
| S0Echr(c0) => show(c0)
  (* print!("S0Echr(", c0, ")") *)
| S0Eflt(f0) => show(f0)
  (* print!("S0Eflt(", f0, ")") *)
| S0Estr(s0) => show(s0)
  (* print!("S0Estr(", s0, ")") *)
//
| S0Eapps(s0es) => show$val<list0(s0exp)>(g0ofg1(s0es)) where
  {
    impltmp show$sep<>() = ()//prout(" ")//()
  }
  (* print!("S0Eapps(", s0es, ")") *)
//
| S0Eimp(tbeg, s0es, tend) =>
  (
    show(tbeg);
    show$val<list0(s0exp)>(g0ofg1(s0es)) where
    {
      impltmp show$sep<>() = ()
    };
    show(tend)

  )
  (* print!("S0Eimp(", tbeg, "; ", s0es, "; ", tend, ")") *)
//
| S0Eparen
  (tbeg, s0es, tend) =>
    (
    show(tbeg);
    (* show_spc(); *)
    show$val<list0(s0exp)>(g0ofg1(s0es)) where
    {
      impltmp show$sep<>() = show_comma()//prout(",")
    };
    show(tend)
    (* show(opt); *)
  )
  (* print!("S0Eparen(", tbeg, "; ", s0es, "; ", tend, ")") *)
//
| S0Eforall(tbeg, s0qs, tend) =>
  (
    show(tbeg);
    show$val<list0(s0qua)>(g0ofg1(s0qs)) where {
      impltmp show$sep<>() = show_bar() //prout("|")
    };
    show(tend)
  )
(*
let
    val _ = $showtype(tbeg)
    val _ = $showtype(s0qs)
*)
(*
  in
    print!("S0Eforall(", tbeg, "; ", s0qs, "; ", tend, ")")
  end
*)
| S0Eexists(tbeg, s0qs, tend) =>
  (
    show(tbeg);
    show$val<list0(s0qua)>(g0ofg1(s0qs)) where
    {
      //impltmp show$sep<>() = prout(",")
    };
    show(tend)
  )
  (* print!("S0Eexists(", tbeg, "; ", s0qs, "; ", tend, ")") *)
//
| S0Etuple
  (tbeg, topt, s0es, tend) =>
  (
    show(tbeg);
    (* show_spc(); *)
    show$val<optn0(token)>(g0ofg1(topt));
    show$val<list0(s0exp)>(g0ofg1(s0es)) where
    {
      impltmp show$sep<>() = show_comma()//prout(",")
    };
    show(tend)
    (* show(opt); *)
  )
(*
  print!("S0Etuple("
  , tbeg, "; ", topt, "; ", s0es, "; ", tend, ")")
*)
| S0Erecord
  (tbeg, topt, s0es, tend) =>
    (
    show(tbeg);
    (* show_spc(); *)
    show$val<optn0(token)>(g0ofg1(topt));
    show$val<list0(sl0abled(s0exp))>(g0ofg1(s0es)) where
    {
      impltmp show$sep<>() = show_comma()//prout(",")
    };
    show(tend)
  )
(*
  print!("S0Erecord("
  , tbeg, "; ", topt, "; ", s0es, "; ", tend, ")")
*)
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
  (
    show(tok);
    show(s0e)
  )
  (* print!("S0Equal(", tok, "; ", s0e, ")") *)
//
| S0Enone(token) => ()
  //print!("S0Enone(", token, ")")
//
) (* end of [show_s0exp] *)

end // end of [local]

(* ****** ****** *)

local

impltmp
print$val<s0exp> x = show_s0exp x

in (* in-of-local *)

implement
show_s0exp_RPAREN(x0) =
(
case+ x0 of
| s0exp_RPAREN_cons0(tok) => show(tok)
  (* print!("s0exp_RPAREN_cons0(", tok, ")") *)
| s0exp_RPAREN_cons1(tok1, s0es, tok2) =>
  (
    show(tok1);
    show$val<list0(s0exp)>(g0ofg1(s0es)) where
    {
      impltmp show$sep<>() = ()//prout(",")
    };
    show(tok2)
  )
(*
  print!("s0exp_RPAREN_cons1(", tok1, ", ", s0es, ", ", tok2, ")")
*)
) (* end of [show_s0exp_RPAREN] *)

end // end of [local]

(* ****** ****** *)

local

impltmp
print$val<s0exp> x = show_s0exp x

in (* in-of-local *)

implement
show_labs0exp_RBRACE(x0) =
(
case+ x0 of
| labs0exp_RBRACE_cons0(tok) =>
  print!("labs0exp_RBRACE_cons0(", tok, ")")
| labs0exp_RBRACE_cons1(tok1, ls0es, tok2) =>
  print!("labs0exp_RBRACE_cons1(", tok1, ", ", ls0es, ", ", tok2, ")")
) (* end of [show_labs0exp_RBRACE] *)

end // end of [local]

(* ****** ****** *)

(*
implement
show_s0eff
  (x0) =
(
case+ x0 of
| S0EFFnone(tok) =>
  print!("S0EFFnone(", tok, ")")
| S0EFFsome
  (tbeg, s0es, tend) =>
  print!("S0EFFsome("
  , tbeg, "; ", s0es, "; ", tend, ")")
) (* end of [show_s0eff] *)
*)

(* ****** ****** *)

implement
show_s0eff(x0) =
(
case+ x0 of
| S0EFFnone(token(*:*)) => // HX: default
  (
    show(token)
  )
  (* print!("S0EFFnone(", "token=", token, ")") *)
| S0EFFsome(tok0(*:<*), s0explst, tok1) => // HX: annotated
  (
    show(tok0);
    show$val<list0(s0exp)>(g0ofg1(s0explst)) where {
      //impltmp show$sep<>() = show_spc();
    };
    show(tok1)
  )
(*
  print!("S0EFFsome("
  , "tok0=", tok0
  , ", s0explst=", s0explst
  , ", tok1=", tok1
  , ")")
*)
)


implement
show_effs0expopt(x0) =
(
case+ x0 of
| EFFS0EXPnone() => ()
  (* print!("EFFS0EXPnone(", ")") *)
//| EFFS0EXPsome(s0e) => (show_col(); show(s0e))
| EFFS0EXPsome(s0f, s0e) =>
  (
    show(s0f);
    show_spc(); // ?
    show(s0e);
  )
(*
  print!("EFFS0EXPsome(", s0f, "; ", s0e, ")")
*)
(*
where
{
//  val _ = $showtype(s0e)
}
*)
  (* print!("EFFS0EXPsome(", s0e, ")") *)
(*
| EFFS0EXPsome(s0f, s0e) =>
  print!("EFFS0EXPsome(", s0f, "; ", s0e, ")")
*)
) (* end of [show_effs0expopt] *)

(* ****** ****** *)

implement
show_d0atype(x0) =
(
case+ x0.node() of
| D0ATYPE(tid, arg, res, teq, d0cs) =>
  (
    (* show_newline(); *)
    show(tid);
    show$val<list0(t0marg)>(g0ofg1(arg)) where {
      impltmp show$sep<>() = show_newline();
    };
    (* show(arg); *)
    show$val<optn0(sort0)>(g0ofg1(res)) where {
      impltmp show$sep<>() = show_newline();
    };
    (* show(res); *)
    show_spc();
    show(teq);
    show_newline();
    show$val<list0(d0atcon)>(g0ofg1(d0cs)) where {
        impltmp show$before<>() = prout("| ")
        impltmp show$sep<>() = (show_newline(); )
    };
  )
(*
  print!("D0ATYPE("
  , tid, "; "
  , arg, "; ", res, "; ", teq, "; ", d0cs, ")")
*)
) (* end of [show_d0atype] *)

(* ****** ****** *)

implement
show_d0atcon(x0) =
(
case+ x0.node() of
| D0ATCON(s0us, dcon, s0is, argopt) =>
  (
    show$val<list0(s0uni)>(g0ofg1(s0us)) where {
      impltmp show$sep<>() = ()
    };
    show(dcon);
    show$val<list0(s0exp)>(g0ofg1(s0is)) where {
      impltmp show$sep<>() = ()
    };
    show$val<optn0(s0exp)>(g0ofg1(argopt)) where {
      impltmp show$before<>() = prout(" of ")
      impltmp show$sep<>() = ()
    };


  )
(*
  print!("D0ATCON("
  , s0us, "; ", dcon, "; ", s0is, "; ", argopt, ")")
*)
) (* end of [show_d0atcon] *)

(* ****** ****** *)

(* end of [xats_staexp0_print.dats] *)
