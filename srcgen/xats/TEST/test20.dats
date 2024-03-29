fun
tempopt_version(): string = "ext#%"
//
(* ****** ****** *)

datatype TYPE(a:vtflt) = TYPE(a) of ()

(* ****** ****** *)
//
// HX-2012: In $ATSHOME/ccomp/runtime:
// atsbool_true/atsbool_false are mapped to 1/0
// this mapping is fixed and should never be changed!
//
#define true true_bool // shorthand
#define false false_bool // shorthand
//
val true_bool : bool(true)  = "mac#atsbool_true" // = 1
val false_bool : bool(false) = "mac#atsbool_false" // = 0
//

(* ****** ****** *)
//
// HX: [false] implies all
//
prfun
false_elim
{X:prop | false} ((*void*)): X
//
(* ****** ****** *)

prfun
prop_verify
{b:bool | b} ():<prf> void
prfun
prop_verify_add
{b:bool | b} ():<prf> [b] void



(* ****** ****** *)
//
val {
a:vtflt
} sizeof : usize(sizeof(a))
//
praxi
lemma_sizeof
{a:vtflt}
((*void*)):[sizeof(a) >= 0] void
//
(* ****** ****** *)


praxi
topize
{a:tflt}(x: !INV(a) >> a?): void

(* ****** ****** *)

castfn
dataget
{a:vtflt}(x: !INV(a) >> a): (a?!)


(* ****** ****** *)
(*
//
// HX: returning the pf to GC
//
praxi
mfree_gc_v_elim
{l:addr}(pf: mfree_gc_v(l)):<prf> void
// end of [mfree_gc_v_elim]
//
praxi
mfree_gcngc_v_nullify
  {l:addr} (
  pf1: mfree_gc_v(l), pf1: mfree_ngc_v(l)
) : void // end of [mfree_gcngc_nullify_v]
//
*)
(* ****** ****** *)
(*
//
fun
cloptr_free
  {a:tflt}
  (cloptr(a)):<!wrt> void = "mac#%"
//
#symload free with cloptr_free of 10
//
*)
(* ****** ****** *)
//
fun
{a:tflt}
lazy_force
(lazyval: lazy(INV(a))):<!laz> (a)
//
fun
{a:vtflt}
lazy_vt_force
(lazyval: lazy_vt(INV(a))):<!all> (a)
//
(*
//
// HX-2016-08:
// this is assumed internally!
//
#symload ! with lazy_force of 10
#symload ! with lazy_vt_force of 10
*)
//
(* ****** ****** *)
//
// HX-2013:
// implemented in [pats_ccomp_instrset]
//
fun
lazy_vt_free
{a:vtflt}
(lazy_vt(a)):<!wrt> void = "mac#%"
//
#symload ~ with lazy_vt_free of 10
#symload free with lazy_vt_free of 10
//
(* ****** ****** *)
//
// HX-2014:
// implemented in [pats_ccomp_instrset]
//
fun
lazy2cloref
  {a:tflt}
(
  lazy(a)
) : ((*void*)) -<cloref1> (a) = "mac#%"
//
(* ****** ****** *)
//
// HX: the_null_ptr = (void*)0
//
val
the_null_ptr
  : ptr(null) = "mac#the_atsptr_null"
//
(* ****** ****** *)
//
praxi
lemma_addr_param
  {l:addr}((*void*)): [l >= null] void
//
(* ****** ****** *)

praxi
lemma_string_param
  {n:int} (x: string(n)): [n >= 0] void
// end of [lemma_string_param]
praxi
lemma_stropt_param
  {n:int} (x: stropt(n)): [n >= ~1] void
// end of [lemma_stropt_param]

(* ****** ****** *)
//
// HX-2012-06:
// indication of the failure of
exception AssertExn of () // an assertion
//
(* ****** ****** *)
//
// HX-2012-06:
// indication of something expected
exception NotFoundExn of () // to be found but not
//
(* ****** ****** *)
//
exception
GenerallyExn of (string) // for unspecified causes
(*
exception
GenerallyExn2 of (string, ptr(*data*)) // for unspecified causes
*)
//
(* ****** ****** *)
//
// HX-2012-07:
// indication of a function argument being
exception IllegalArgExn of (string) // out of its domain
//
(* ****** ****** *)
//
datatype unit = unit of ()
dataprop unit_p = unit_p of ()
dataview unit_v = unit_v of ()
datavtype unit_vt = unit_vt of ()
//
prfun unit_v_elim (unit_v): void
prfun unit_vt_elim (unit_vt): void
//
(* ****** ****** *)
(*
//
abstbox
boxed_tflt_tbox(a:tflt+) = unit
absvtbox
boxed_vtflt_vtbox(a:vtflt+) = unit
//
vtypedef
boxed(a:vtflt) = boxed_vtflt_vtbox(a)
vtypedef
boxed_vt(a:vtflt) = boxed_vtflt_vtbox(a)
//
typedef boxed(a:tflt) = boxed_tflt_tbox(a)
typedef boxed_t(a:tflt) = boxed_tflt_tbox(a)
//
fun{a:tbox} box: (INV(a)) -> boxed_t(a)
fun{a:tbox} unbox: boxed_t(INV(a)) -> (a)
fun{a:vtbox} box_vt: (INV(a)) -> boxed_vt(a)
fun{a:vtbox} unbox_vt: boxed_vt(INV(a)) -> (a)
//
*)
(* ****** ****** *)
//
sexpdef
array(a:vtflt, n:int) = @[a][n]
//
viewdef
array_v
(a:vtflt, l:addr, n:int) = @[a][n] @ l
//
abstbox
arrayref_vtflt_int_tbox
  (a:vtflt(*elt*), n:int(*size*)) = ptr
sexpdef arrayref = arrayref_vtflt_int_tbox
//
absvtbox
arrayptr_vtflt_addr_int_vtbox
(a:vtflt+,l:addr,n:int(*size*)) = ptr(l)
sexpdef
arrayptr = arrayptr_vtflt_addr_int_vtbox
vtypedef
arrayptr
(a:vtflt,n:int) = [l:addr] arrayptr(a,l,n)
//
abstbox
arrszref_vtflt_tbox(a: vtflt) = ptr
typedef
arrszref(a:vtflt) = arrszref_vtflt_tbox(a)
//
absvtbox
arrszptr_vtflt_vtbox(a: vtflt) = ptr
vtypedef
arrszptr(a:vtflt) = arrszptr_vtflt_vtbox(a)
//
(* ****** ****** *)
//
datatype
// tflt+: covariant
list0_tflt_tbox
  (a:tflt+) =
| list0_nil(a) of ()
| list0_cons(a) of
  (a, list0_tflt_tbox(INV(a)))
// end of [datatype]
//
sexpdef list0 = list0_tflt_tbox


////

sortdef tbox0 = tbox
sortdef tbox1 = vtbox
sortdef tflt0 = tflt
sortdef tflt1 = vtflt


(* ****** ****** *)
//
// HX:
// some built-in
// static boolean constants
//
(*
stacst
true_bool : bool = "ext#"
stacst
false_bool : bool = "ext#"
//
sexpdef
tt = true_bool and ff = false_bool
sexpdef
true = true_bool and false = false_bool
*)
//

(* ****** ****** *)
//
// HX: boolean negation
//
(*
stacst
neg_bool: bool -> bool = "ext#"
*)
//

sexpdef ~ = neg_bool // overloaded
sexpdef not = neg_bool // overloaded
//
(* ****** ****** *)
//
// HX: boolean disjunction
//
stacst
add_bool_bool: (bool, bool) -> bool = "ext#"
//
// HX: boolean conjunction
//
stacst
mul_bool_bool: (bool, bool) -> bool = "ext#"
//
sexpdef + = add_bool_bool and || = add_bool_bool
sexpdef * = mul_bool_bool and && = mul_bool_bool
//
(* ****** ****** *)
//
stacst
lt_bool_bool: (bool, bool) -> bool = "ext#"
stacst
lte_bool_bool: (bool, bool) -> bool = "ext#"
//
stacst
gt_bool_bool: (bool, bool) -> bool = "ext#"
stacst
gte_bool_bool: (bool, bool) -> bool = "ext#"
//
sexpdef < = lt_bool_bool and <= = lte_bool_bool
sexpdef > = gt_bool_bool and >= = gte_bool_bool
//
stacst
eq_bool_bool: (bool, bool) -> bool = "ext#"
stacst
neq_bool_bool: (bool, bool) -> bool = "ext#"
//
sexpdef == = eq_bool_bool
sexpdef != = neq_bool_bool
sexpdef <> = neq_bool_bool (* for backward compatibility *)
//
(* ****** ****** *)
//
// HX: arithmetic operations
//
stacst
neg_int: (int) -> int = "ext#"
//
sexpdef ~ = neg_int // overloaded
//
stacst
add_int_int: (int, int) -> int = "ext#"
stacst
sub_int_int: (int, int) -> int = "ext#"
stacst
mul_int_int: (int, int) -> int = "ext#"
stacst
div_int_int: (int, int) -> int = "ext#"
//
sexpdef + = add_int_int and - = sub_int_int
sexpdef * = mul_int_int and / = div_int_int
//
// HX: ndiv: divisor is positive
// HX: idiv: alias for div_int_int
//
stacst
ndiv_int_int: (int, int) -> int = "ext#"
stacst
idiv_int_int: (int, int) -> int = "ext#"
//
sexpdef ndiv = ndiv_int_int // divided by nat
sexpdef idiv = idiv_int_int // divided by int
//
sexpdef
nmod_int_int
(
  x:int, y:int
) = x - y * (x \ndiv_int_int y)
//
sexpdef mod = nmod_int_int
sexpdef nmod = nmod_int_int
sexpdef % (*adopted from C*) = nmod_int_int
//
(* ****** ****** *)
//
// HX: comparison operations
//
stacst
lt_int_int: (int, int) -> bool = "ext#"
stacst
lte_int_int: (int, int) -> bool = "ext#"
//
stacst
gt_int_int: (int, int) -> bool = "ext#"
stacst
gte_int_int: (int, int) -> bool = "ext#"
//
sexpdef < = lt_int_int and <= = lte_int_int
sexpdef > = gt_int_int and >= = gte_int_int
//
stacst
eq_int_int: (int, int) -> bool = "ext#"
stacst
neq_int_int: (int, int) -> bool = "ext#"
//
sexpdef == = eq_int_int
sexpdef != = neq_int_int
sexpdef <> = neq_int_int (* for backward compatibility *)
//
(* ****** ****** *)
//
// HX: various integer ops
//
stacst
abs_int: (int) -> int = "ext#"
//
sexpdef
absrel_int_int
  (x: int, v: int): bool =
  (x >= 0 && x == v) || (x <= 0 && ~x == v)
//
sexpdef abs = abs_int
sexpdef absrel = absrel_int_int
//
stacst
sgn_int: (int) -> int = "ext#"
//
sexpdef
sgnrel_int_int
  (x: int, v: int): bool =
  (x > 0 && v==1) || (x==0 && v==0) || (x < 0 && v==(~1))
//
sexpdef sgn = sgn_int
sexpdef sgnrel = sgnrel_int_int
//
stacst
max_int_int: (int, int) -> int = "ext#"
stacst
min_int_int: (int, int) -> int = "ext#"
//
sexpdef
maxrel_int_int_int
  (x: int, y: int, v: int): bool =
  (x >= y && x == v) || (x <= y && y == v)
//
sexpdef
minrel_int_int_int
  (x: int, y: int, v: int): bool =
  (x >= y && y == v) || (x <= y && x == v)
//
sexpdef max = max_int_int
sexpdef min = min_int_int
sexpdef maxrel = maxrel_int_int_int
sexpdef minrel = minrel_int_int_int
//
sexpdef
nsub (x:int, y:int) = max (x-y, 0)
//
sexpdef
ndivrel_int_int_int // HX: y > 0
  (x: int, y: int, q: int): bool =
  (q * y <= x) && (x < q * y + y)
//
sexpdef ndivrel = ndivrel_int_int_int
//
sexpdef
idivrel_int_int_int
  (x: int, y: int, q: int) = ( // HX: y != 0
  x >= 0 && y > 0 && ndivrel_int_int_int ( x,  y,  q)
) || (
  x >= 0 && y < 0 && ndivrel_int_int_int ( x, ~y, ~q)
) || (
  x <  0 && y > 0 && ndivrel_int_int_int (~x,  y, ~q)
) || (
  x <  0 && y < 0 && ndivrel_int_int_int (~x, ~y,  q)
) (* end of [idivrel_int_int_int] *)
//
sexpdef idivrel = idivrel_int_int_int
//
sexpdef
divmodrel_int_int_int_int
  (x: int, y: int, q: int, r: int) : bool =
  (0 <= r && r < y && x == q*y + r)
//
sexpdef divmodrel = divmodrel_int_int_int_int
//
(* ****** ****** *)
//
stacst
ifint_bool_int_int
  : (bool, int, int) -> int = "ext#"
//
sexpdef
ifintrel_bool_int_int_int
(
  b:bool, x:int, y:int, r:int
) : bool = (b && r==x) || (~b && r==y)
//
sexpdef ifint = ifint_bool_int_int
sexpdef ifintrel = ifintrel_bool_int_int_int
//
(* ****** ****** *)

sexpdef
bool2int(b: bool): int = ifint(b, 1, 0)
sexpdef int2bool (i: int): bool = (i != 0)
sexpdef b2i = bool2int and i2b = int2bool

(* ****** ****** *)
//
(*
** HX: pointer <-> integer
*)
stacst int_of_addr: addr -> int = "ext#"
stacst addr_of_int: int -> addr = "ext#"
sexpdef a2i = int_of_addr and i2a = addr_of_int
//
(* ****** ****** *)
//
sexpdef pow2_7 = 128
sexpdef pow2_8 = 256
sexpdef i2u_int8 (i:int) = ifint (i >= 0, i, i+pow2_8)
sexpdef i2u8 = i2u_int8
sexpdef u2i_int8 (u:int) = ifint (u < pow2_7, u, u-pow2_8)
sexpdef u2i8 = u2i_int8
//
sexpdef pow2_15 = 32768
sexpdef pow2_16 = 65536
sexpdef i2u_int16 (i:int) = ifint (i >= 0, i, i+pow2_16)
sexpdef i2u16 = i2u_int16
sexpdef u2i_int16 (u:int) = ifint (u < pow2_15, u, u-pow2_16)
sexpdef u2i16 = u2i_int16
//
(* ****** ****** *)

sexpdef pow2_32 = 0x100000000
sexpdef pow2_64 = 0x10000000000000000

(* ****** ****** *)
//
stacst
null_addr: addr = "ext#"
sexpdef null: addr = null_addr
//
stacst
add_addr_int: (addr, int) -> addr = "ext#"
stacst
sub_addr_int: (addr, int) -> addr = "ext#"
stacst
sub_addr_addr: (addr, addr) -> int = "ext#"
//
sexpdef + = add_addr_int
sexpdef - = sub_addr_int and - = sub_addr_addr
//
(* ****** ****** *)
//
stacst
lt_addr_addr: (addr, addr) -> bool = "ext#"
stacst
lte_addr_addr: (addr, addr) -> bool = "ext#"
//
stacst
gt_addr_addr: (addr, addr) -> bool = "ext#"
stacst
gte_addr_addr: (addr, addr) -> bool = "ext#"
//
sexpdef < = lt_addr_addr and <= = lte_addr_addr
sexpdef > = gt_addr_addr and >= = gte_addr_addr
//
stacst
eq_addr_addr: (addr, addr) -> bool = "ext#"
stacst
neq_addr_addr: (addr, addr) -> bool = "ext#"
//
sexpdef == = eq_addr_addr
sexpdef != = neq_addr_addr
sexpdef <> = neq_addr_addr (* for backward compatibility *)
//
(* ****** ****** *)
//
// HX-2017-11-07:
//
abstflt types_nil
abstflt types_cons(vtflt+, tflt+)
(*
stacst types_nil : types
stacst types_cons : (vtflt+, types+) -> types
*)
//
(* ****** ****** *)
//
// HX: this is a special constant!
//
stacst
sizeof_t0ype_int: tflt -> int = "ext#"
//
sexpdef
sizeof(a : vtflt): int = sizeof_t0ype_int(a?)
//
(* ****** ****** *)

sortdef nat = { n:int | n >= 0 }
sortdef nat1 = { n:nat | n < 1 } // for 0
sortdef nat2 = { n:nat | n < 2 } // for 0, 1
sortdef nat3 = { n:nat | n < 3 } // for 0, 1, 2
sortdef nat4 = { n:nat | n < 4 } // for 0, 1, 2, 3

sortdef pos = { i:int | i >  0 } // positive ints
sortdef neg = { i:int | i <  0 } // negative ints
sortdef npos = { i:int | i <= 0 } // non-positive ints
sortdef nneg = { i:int | i >= 0 } // non-negative ints

(* ****** ****** *)

sortdef sgn = { i:int | ~1 <= i; i <= 1 }

(* ****** ****** *)

sortdef igz = { i:int | i > 0 }
sortdef igez = { i:int | i >= 0 }
sortdef ilez = { i:int | i <= 0 }

sortdef agz = { l:addr | l > null }
sortdef agez = { l:addr | l >= null }
sortdef alez = { l:addr | l <= null }

(* ****** ****** *)

#define CHAR_MAX 127
#define CHAR_MIN ~128
#define UCHAR_MAX 0xFF

(* ****** ****** *)
//
stacst effnil : eff // nothing
stacst effall : eff // everything
//
stacst effntm : eff // nonterm
stacst effexn : eff // exception
stacst effref : eff // reference
stacst effwrt : eff // writeover
//
stacst add_eff_eff : (eff, eff) -> eff
sexpdef + = add_eff_eff // union of effsets
stacst sub_eff_eff : (eff, eff) -> eff
sexpdef - = add_eff_eff // difference of effsets
//
(* ****** ****** *)
//
// HX: some overloaded symbols
//
symintr ~ not
(*
symintr && || // macros
*)
symintr lnot lor lxor land
symintr + - * / % mod ndiv nmod
symintr < <= > >= = == != <> compare
symintr isltz isltez isgtz isgtez iseqz isneqz
symintr neg abs max min
symintr succ pred half double
symintr square sqrt cube cbrt pow
//
symintr ! [] // deref subscript
symintr << >> // for L/R-shifting
//
symintr inc dec
symintr ++ -- // inc and dec
symintr get set exch
symintr getinc setinc exchinc
symintr decget decset decexch
symintr !++ --! // getinc and decget
symintr =++ --= // setinc and decset
//
symintr assert
//
symintr encode decode
//
symintr uncons unsome
//
symintr ptrcast (* taking the address of a boxed val *)
symintr g0ofg1 g1ofg0 (* casting: indexed <-> un-indexed *)
//
symintr copy free length
//
symintr print prerr fprint gprint
symintr println prerrln fprintln gprintln
//
(*
//
symintr forall
symintr iforall
//
symintr foreach
symintr foreach2
symintr iforeach
symintr rforeach
//
*)
//
symintr ofstring ofstrptr
symintr tostring tostrptr
//
(* ****** ****** *)
//
// HX-2014-02:
// for dot-notation overloading
//
symintr .size
symintr .len .length
symintr .get .set .exch
symintr .nrow .ncol
symintr .head .tail
symintr .next .prev
symintr .init .last
symintr .eval // HX: convention: using "!"
//
(* ****** ****** *)
//
absview // S2Eat
at_vt0ype_addr_view(a:vtflt+, l:addr)
//
viewdef @ // HX: @ is infix
  (a:vtflt, l:addr) = at_vt0ype_addr_view(a, l)
//
(* ****** ****** *)
//
abstflt clo_t0ype_t0ype(a:tflt) = a
absvtflt clo_vt0ype_vt0ype(a:vtflt) = a
//
(* ****** ****** *)
//
(*
vtypedef READ(a:vtflt) = a // HX: used as a comment
vtypedef WRITE(a:vtflt) = a // HX: used as a comment (rarely)
*)
//
(*
vtypedef SHARED (a:vtflt) = a // HX: used as a comment
vtypedef NSHARED (a:vtflt) = a // HX: used as a comment (rarely)
*)
//
(* ****** ****** *)
//
absprop
invar_prop_prop(a:prop)
absview
invar_view_view(a:view)
//
abstflt // S2Einvar
invar_t0ype_t0ype(a:tflt) = a
absvtflt // S2Einvar
invar_vt0ype_vt0ype(a:vtflt) = a
//
// HX: this order is significant
//
viewdef
INV(a:view) = invar_view_view(a)
propdef
INV(a:prop) = invar_prop_prop(a)
//
vtypedef
INV
(a:vtflt) = invar_vt0ype_vt0ype(a)
//
typedef
INV(a:tflt) = invar_t0ype_t0ype(a)
//
(* ****** ****** *)
//
absview
vcopyenv_view_view(v:view)
absvtflt
vcopyenv_vt0ype_vt0ype(vt:vtflt) = vt
//
sexpdef vcopyenv_v = vcopyenv_view_view
sexpdef vcopyenv_vt = vcopyenv_vt0ype_vt0ype
//
(* ****** ****** *)
