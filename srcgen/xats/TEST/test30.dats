(*
extern
praxi
_assert_
{vw:view}(vw):<prf> void
*)


extern
castfn
_castfn_
{vt:vtbox}(vt):<fun>(ptr)
////

impltmp
{pf}
{xs,x0}{y0}
gseq_rmap_list(xs) = let
//
vtypedef p0 = ptr
//
impltmp
gseq_foldright$fopr<pf><xs,x0><p0>
  (x0, p0) =
  (addr@(r1)) where
{
//
extern
praxi
_assert_
{vw:view}(vw):<prf> void
extern
castfn
_castfn_
{vt:vtbox}(vt):<fun>(ptr)
//
val y0 =
gseq_rmap$fopr<pf><xs,x0><y0>(x0)
val ys =
list0_vt_cons(y0, _(*r1*))
val+
list0_vt_cons(y0, r1) = ys
//
val ys = _castfn_(ys)
val () =
$UN.ptr0_set<ptr>(p0, ys)
//
prval () = _assert_(view@y0)
prval () = _assert_(view@r1)
//
} (* end of [gseq_foldright$fopr] *)
//
var r0: ptr?
val p0 = addr@(r0)
val p1 =
gseq_foldright<pf><xs,x0><p0>(xs, p0)
//
in
  $UN.ptr0_set
  (p1, list0_vt_nil()); $UN.castvwtp0{list0_vt(y0)}(r0)
end (* end of [gseq_rmap_list] *)
