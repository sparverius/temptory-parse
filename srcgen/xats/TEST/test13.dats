fun helper(xs:myseq(a)):list0(list0(a)) =
if
myseq_iseqz(xs)
then sing(nil())
else (list0_concat(list0_vt2t(gseq_map_list<gseq><myseq(a),tup(a,myseq(a))><list0(list0(a))>(xs)))
where
{
impltmp gseq_map$fopr<gseq><myseq(a),tup(a,myseq(a))><list0(list0(a))>(x0) =
list0_mcons(x0.0,helper(x0.1))
})
