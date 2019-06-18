fun
fact(x: int) =
(
gseq_mul<gseq><list(int),int>
(
list0_vt2t
(
gseq_map_list<gseq><int,int><int>(x)
)
)
) where

  impltmp
  gseq_map$fopr<gseq><int,int><int>(x) = x + 1

  end

////

(* ****** ****** *)

val () = println!("fact(10) = ", fact(10))
