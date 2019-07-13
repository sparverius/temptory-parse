#include "share/HATS/temptory_staload_bucs320.hats"

#staload "./../SATS/basics.sats"

// fundecl

impltmp template$sep<>() = print!("\n")
impltmp template$beg<>() = print!("\n")
impltmp template$end<>() = print!("\n")
impltmp template$before_each<>() = ()
impltmp template$after_each<>() = ()

(*
extern fun{} decl$aft(): void
impltmp decl$aft<>() =
  (
    show$println();
    prout("and");
    show_newline()
  )
*)
