castfn
dataget
{a:vtflt}(x: !INV(a) >> a): (a?!)


fun
{a:tflt}
lazy_force
(lazyval: lazy(INV(a))):<!laz> (a)
//
fun
{a:vtflt}
lazy_vt_force
(lazyval: lazy_vt(INV(a))):<!all> (a)
