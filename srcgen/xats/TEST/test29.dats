impltmp
{pf}
{xs,x0}
gseq_iseqz(xs) =
(
gseq_forall<pf><xs,x0>(xs)
) //// where
{
impltmp
gseq_forall$test<pf><xs,x0>(x0) = ff
}
impltmp
{pf}
{xs,x0}
gseq_isneqz(xs) =
(
 if eqz then ff else tt
) where
{
  val eqz = gseq_iseqz<pf><xs,x0>(xs)
} (* end of [gseq_isneqz] *)
