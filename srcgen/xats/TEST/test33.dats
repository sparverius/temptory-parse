impltmp
{px}
{py}
{xs,x0}
{ys,y0}
gseq_x2forall(xs, ys) =
(
  loop2(xs, ys)
) where
{
val xs =
gseq_listize<px><xs,x0>(xs)
val ys =
gseq_listize<py><ys,y0>(ys)
//
vtypedef xx = list0_vt(x0)
vtypedef yy = list0_vt(y0)
//
fun
loop1(x0: x0, yy: !yy): bool =
(
case+ yy of
| list0_vt_nil() => true
| list0_vt_cons(y0, yy) =>
  (
  if
  test
  then loop1(x0, yy) else false
  ) where
  {
    val
    test =
    gseq_x2forall$test<px><py><xs,x0><ys,y0>(x0, y0)
  } (* end of [list0_vt_cons] *)
)
fun
loop2(xx: xx, yy: yy): bool =
(
case+ xx of
| ~list0_vt_nil() =>
   let val () = list0_vt_free<y0>(yy) in true end
| ~list0_vt_cons(x0, xx) =>
   if
   loop1(x0, yy)
   then
   loop2(xx, yy)
   else
   (
   list0_vt_free<x0>(xx); list0_vt_free<y0>(yy); false
   ) (* list0_vt_cons *)
)
} (* end of [gseq_x2forall] *)
