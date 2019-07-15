(*
#staload UN = "./../SATS/unsafe.sats"

#staload UN =
{

}
////
*)

static fun foo : () -> void

impltmp
gcompare$val<string_vt>(cs1, cs2) = ($effmask_all(strcmp(cs1, cs2))) where
{
  val cs1 = $UN.string0_vt2t(cs1)
  and cs2 = $UN.string0_vt2t(cs2)
}
impltmp (a:tflt) gcompare$val<list0(a)>(xs, ys) =
  ($effmask_all(list0_compare<a>(xs, ys)))



////
impltmp {a} gequal$ref(x1, x2) = gequal$val<a>(x1, x2)
////

impltmp
<a0,a1,a2:tflt>
gcompare$val<tup(a0,a1,a2)>
  (xs, ys) =
(
  tuple3_compare<a0,a1,a2>(xs, ys)
)
