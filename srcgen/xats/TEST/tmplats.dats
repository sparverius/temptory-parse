#include "share/HATS/temptory_staload_bucs320.hats"

sortdef tycon = tflt+ -> tflt

extern fun {f:tycon} {a,b:tflt} fmap2 (xs: f a): f(b)
extern fun {a,b:tflt} fmap2$fopr : a -> b

(* instance listn *)
(*
extern praxi lengte0 {a:tflt} {n:int} (xs: list1(a, n)): [n>=0] unit_p
*)

impltmp (a,b:tflt) fmap2<list0><a,b> (xs) = list0_map<a><b>(xs)
  where
  {
  impltmp list0_map$fopr<a><b>(x) = fmap2$fopr<a,b>(x)
  }


implement main0 () = let
    #define :: cons
    val xs = g0ofg1('[1,2,3,4]) : list0(int)//[n: int] list1(int, n)
    (* prval _ = lengte0(xs) *)
    impltmp fmap2$fopr<int,int>(x) = x * 3
    val xs = fmap2<list0><int,int>(xs)
(*
    val mz = fmap2<list><int,int>(xs, lam x => let val _ = println! x in x end)
*)
in
end


////

fun
{pf:prop}
{xs:tflt;x0:tflt}
gseq_forall(xs): bool
fun
{pf:prop}
{xs:tflt
;x0:tflt}
gseq_forall$test(x0): bool


////


fun
{pf:prop}
{xs:tflt;x0:tflt}
gseq_forall(xs): bool
fun
{pf:prop}
{xs:tflt;x0:tflt}
gseq_forall$test(x0): bool


////


val () =
(gseq_foreach<gseq_i(pf),xs,i(x0)>(gseq_i(pf) | xs))
where
  prval pf = gseq_list0{int}()
  typedef xs = list0(int)
  propdef pf = gseq_list0(int)
  typedef i(x0:tflt) =
  impltmp
  gseq_foreach$work<gseq_i(pf),xs,(int, int)>
    (_ | ix) = println!(ix.0, " -> ", ix.1)


(* endwhere *)

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
