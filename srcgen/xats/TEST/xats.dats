(* ****** ****** *)
(*
** HX-2018-06-30
** Testing. Kind of.
*)
(* ****** ****** *)
//
(*
#include
"/atspre_staload.hats"
#include
"share/atspre_staload_libats_ML.hats"
*)
#include "share/HATS/temptory_staload_bucs320.hats"

//
(* ****** ****** *)

local
//
#include
"./../../util/DATS/cblist.dats"
#include
"./../../util/DATS/Posix/cblist.dats"
//
in
  // nothing
end // end of [local]

(* ****** ****** *)

local
//
(* ****** ****** *)
//
#include"./../DATS/basics.dats"
//
#include"./../DATS/symbol.dats"
//
#include"./../DATS/label0.dats"
#include"./../DATS/location.dats"
#include"./../DATS/filepath.dats"
//
(* ****** ****** *)
//
#include"./../DATS/lexbuf.dats"
//
#include"./../DATS/lexing_token.dats"
#include"./../DATS/lexing_kword.dats"
//
#include"./../DATS/lexing_util0.dats"
//
(* ****** ****** *)
//
#include"./../DATS/staexp0.dats"
#include"./../DATS/staexp0_print.dats"
//
(* ****** ****** *)
//
#include"./../DATS/dynexp0.dats"
#include"./../DATS/dynexp0_print.dats"
//
(* ****** ****** *)
//
#include"./../DATS/parsing_tokbuf.dats"
//
#include"./../DATS/parsing_basics.dats"
//
#include"./../DATS/parsing_staexp.dats"
//
#include"./../DATS/parsing_dynexp.dats"
//
(* ****** ****** *)
//
in
  // nothing
end // end of [local]

(* ****** ****** *)

implement
main0() =
{
//
val () =
println!
("Hello from [test_xats_main]!")
//
} (* end of [main0] *)

(* ****** ****** *)
//
#staload "./../SATS/lexing.sats"
//
#staload "./../SATS/staexp0.sats"
#staload "./../SATS/dynexp0.sats"
//
#staload "./../SATS/parsing.sats"
//
(* ****** ****** *)

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

(* ****** ****** *)

impltmp
print$val<sort0> x = print_sort0 x
impltmp
print$val<s0exp> x = print_s0exp x
impltmp
print$val<d0exp> x = print_d0exp x
impltmp
print$val<d0ecl> x = print_d0ecl x

(* ****** ****** *)
(*
//
val-
toks =
(
string_tokenize
("\
//
//
")
)
//
val
toks =
lexing_preprocess_tokenlst(toks)
//
val
toks =
list_vt2t(toks)
(*
val ((*void*)) =
list0_foreach<token>
( g0ofg1(toks)
, lam(tok) => fprint_token(stdout_ref, tok))
*)
//
val s0e0 = let
//
var err: int
var buf: tokbuf
val ((*void*)) = (err := 0)
val ((*void*)) =
tokbuf_initize_list(buf, toks)
//
in
  p_s0exp(buf, err)
end // end of [val]
//
val () = println! ("s0e0 = ", s0e0)
//
*)
(* ****** ****** *)

implement
main0(argc, argv) =
{
//
val toks = parse_from_stdin_toplevel(1)
(*
val toks = list1_vt2t(toks)
*)
//
val ((*void*)) =
list0_foreach<d0ecl>(g0ofg1(toks)) where
{
  impltmp
  list0_foreach$work<d0ecl> tok = println!(tok)
}

}
////

if
(argc >= 2)
then //xatsopt_main0(argc, argv)
() where
// end of [if]
{
val () = println!(argv[1])

val opt =
FILEref_open_opt
(*
("./test_syntax.text", "r"(* file_mode_r *))
*)
(argv[1], "r"(* file_mode_r *))
val-~optn0_vt_some(inp) = opt
val
toks =
fileref_tokenize(inp)
val
toks =
lexing_preprocess_tokenlst(toks)
//
val toks = list1_vt2t(toks)
//
val ((*void*)) =
list0_foreach<token>(g0ofg1(toks)) where
{
  impltmp
  list0_foreach$work<token> tok = println!(tok)
}
//
val d0cs = let
//
var err: int
var buf: tokbuf
val ((*void*)) = (err := 0)
val ((*void*)) =
  tokbuf_initize_list(buf, toks)
//
in
  (* p_d0eclseq_top(buf, err) *)
  ptop_d0eclseq_dyn(buf, err)
end // end of [val]

val () = println! ("d0cs = ", d0cs)

} else println! ("Hello from ATS3(ATS/Xanadu)!")

) (* end of [main] *)

(* ****** ****** *)

(* end of [test_xats_main.dats] *)
