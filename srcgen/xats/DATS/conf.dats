#include "share/HATS/temptory_staload_bucs320.hats"

#staload "./../SATS/basics.sats"

#define newline "\n"
#define space " "

#define pr print

(* #macdef newline() = print newline *)

// fundecl

(* impltmp tq0arg$sep<>() = pr newline *)
(* impltmp tq0arg$beg<>() = pr newline *)
(* impltmp tq0arg$end<>() = pr newline *)
(* impltmp template$before_each<>() = () *)
(* impltmp template$after_each<>() = () *)

impltmp template$none<>() = pr newline

impltmp template$arg$sep<>() = ()

impltmp template$sep<>() = pr newline
impltmp template$beg<>() = pr newline
impltmp template$end<>() = pr newline
impltmp template$before_each<>() = ()
impltmp template$after_each<>() = ()

impltmp template$none<>() = pr newline

impltmp template$arg$sep<>() = ()

impltmp ti0arg$sep<>() = ()

impltmp tq0arg$sep<>() = pr newline
impltmp tq0arg$beg<>() = ()
impltmp tq0arg$end<>() = pr newline

impltmp tq0arg$none<>() = ()

impltmp sq0arg$beg<>() = pr newline
impltmp sq0arg$end<>() = pr newline
impltmp sq0arg$none<>() = pr newline

impltmp sq0arg$sep<>() = ()

impltmp q0arg$colon$beg<>() = () //pr space
impltmp q0arg$colon$end<>() = () //pr space

impltmp staexp$sep<>() = () //pr space

(*
extern fun{} decl$aft(): void
impltmp decl$aft<>() =
  (
    show$println();
    prout("and");
    show_newline()
  )
*)

impltmp impdecl$eq$beg<>() = pr space
impltmp impdecl$eq$end<>() = pr newline

impltmp impdecl$beg<>() = pr newline
impltmp impdecl$end<>() = () //newline //newline


impltmp def$id$beg<>() = space //newline
impltmp def$id$end<>() = space //newline



impltmp eq$beg<>() = space //newline
impltmp eq$end<>() = space //newline

impltmp case$sep<>() = pr space

impltmp colon$sep<>() = pr space

impltmp bar$sep<>() = pr space

impltmp comma$sep<>() = pr space

(* impltmp case$beg<>() = pr newline *)
(* impltmp case$end<>() = pr newline //newline *)
(* impltmp case$endall<>() = pr newline //newline *)

impltmp paren$beg<>() = pr newline
impltmp paren$end<>() = pr newline


impltmp eqgt$beg<>() = pr space
impltmp eqgt$end<>() = pr space

// ->
impltmp arrow$beg<>() = pr space
impltmp arrow$end<>() = pr space

impltmp smcln$beg<>() = ()
impltmp smcln$end<>() = pr space


impltmp let$beg<>() = ()
impltmp let$end<>() = pr newline

impltmp in$beg<>() = pr newline
impltmp in$end<>() = pr newline

impltmp end$beg<>() = pr newline
impltmp end$end<>() = () //pr space

impltmp if$beg<>() = ()
impltmp if$end<>() = pr space

impltmp then$beg<>() = pr space
impltmp then$end<>() = ()

impltmp else$beg<>() = pr space
impltmp else$end<>() = ()


impltmp ldelay$beg<>() = ()
impltmp ldelay$end<>() = pr newline

impltmp delay$beg<>() = ()
impltmp delay$end<>() = pr newline
