#include "share/HATS/temptory_staload_bucs320.hats"

sortdef t = tflt
sortdef f = tflt+ -> tflt

extern fun {f:f}{a,b:t} map : f(a) -> f(b)
extern fun {f:f}{a,b:t} map$fopr : a -> b

(* ****** ****** *)

impltmp(a,b:t) map<list0><a,b>(xs) = list0_map<a><b>(xs)
impltmp(f:f,a:t,b:t) list0_map$fopr<a><b>(x) = map$fopr<list0><a,b>(x)

(* ****** ****** *)

implement main0() =
{
  val map0 = g0ofg1('[72,69,76,76,79]) : list0(int)

  val map1 = map<list0><int,char>(map0) where
  {
  impltmp map$fopr<list0><int,char>(x) = char0_chr(x) + 32
  }

  val () = println!(map0)
  (* val () = println!(map1) *)
}
