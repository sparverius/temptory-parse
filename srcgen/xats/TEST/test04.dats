fun
fibo(n0: int): int =((gseq_foldleft<gseq><SINT2,x0><r0>(SINT2, (0, 1))).0) where
{
//
typedef x0 = sint
typedef r0 = (sint, sint)
//
impltmp
SINT2_range$beg<>() = 0
impltmp
SINT2_range$end<>() = n0
//
impltmp
gseq_foldleft$fopr<gseq><SINT2,x0><r0>(r0, i0) = (r0.1, r0.0+r0.1)
//
} (* end of [fact] *)
