val () =
(
gseq_foreach<gseq_i(pf),xs,i(x0)>(gseq_i(pf) | xs)
)
where

  prval pf =
  gseq_list0{int}()
  typedef x0 = int
  typedef xs = list0(x0)
  propdef pf = gseq_list0(x0)
  typedef i(x0:tflt) = (int, x0)
  impltmp
  gseq_foreach$work<gseq_i(pf),xs,i(x0)>(_ | ix) = println!(ix.0, " -> ", ix.1)

endwhere

////


(*
extern fun foo() : void

implement foo() = ()

(*
datatype foo =
  | nothing
*)

////
*)

(*
local
in
end
*)

(*
fun foo(): void = ()
*)

implement
gseq_rforall<gseq><cs,c0>(cs) = (string0_rforall<>(cs)) where
{
impltmp
string0_rforall$test<>(c0) = gseq_rforall$test<c0>(c0)
string0_rforall$test<>(c0) =
  gseq_rforall$test<gseq><cs,c0>(c0)
} (* end of [gseq_rforall] *)
