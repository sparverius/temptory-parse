datasort
status =
| init | conn | bind | listen
//
absview socket_v (int, status)

datavtype
// vtflt+: covariant
list0_vtflt_vtbox
  (a:vtflt+) =
| list0_vt_nil(a) of ()
| list0_vt_cons(a) of
  (a, list0_vtflt_vtbox(INV(a)))
// end of [datavtype]
//
sexpdef list0_vt = list0_vtflt_vtbox
