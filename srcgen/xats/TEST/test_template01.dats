(*
impltmp
{a:tflt}
print$val(xs) = list1_print<a>(xs)
*)


(*
impltmp
{a:tflt}
(* (a:tflt) *)
<a:tflt>
print$val<list1(a)>(xs) : void = list1_print<a>(xs)
*)

//impltmp {a}<b> print$val(funargs) : void = ()
impltmp (a:tflt) print$val<con(a)>(funargs) : void = ()
impltmp {a:tflt} print$val<con(a)>(funargs) : void = ()
impltmp <a:tflt> print$val<con(a)>(funargs) : void = ()

(*
impltmp
(* {a:tflt} *)
(* (a:tflt) *)
(* <a:tflt> *)
print$val<list1(a)>(xs) = list1_print<a>(xs)
*)
