fun
board_print0
(xs: board): void =
(
let
val () =
board_print1(xs) in board_free(xs)
end
)
and
board_print1
(xs: !board): void =
(
glseq_rforeach1<gseq><board,int>
  (xs)
) where
{
impltmp
glseq_rforeach1$work<gseq><board,int>(x0) =
(
  loop(0)
) where
{
fun
loop(i0: int): void =
if
i0 < N
then
(
loop(i0+1)
) where
{
  val () =
  if
  i0 = x0
  then print "Q " else print". "
}
else println!()
}
} (* end of [board_print *)
