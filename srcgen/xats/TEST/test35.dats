impltmp
{pf}
{xs,x0}
gseq_size(xs) =
(
gseq_foldleft<pf><xs,x0><r0>
  (xs, i2sz(0))
) where
{
//
typedef r0 = size
//
impltmp
gseq_foldleft$fopr<pf><xs,x0><r0>(r0, x0) = succ(r0)
} (* end of [gseq_size] *)
