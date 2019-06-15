#include "./share.sats"

#staload
LAB = "./label0.sats"
#staload
LOC = "./location.sats"
//
  typedef label = $LAB.label
  typedef loc_t = $LOC.location
//
#staload LEX = "./lexing.sats"
#staload SYM = "./symbol.sats"
//
  typedef token = $LEX.token
  typedef tokenlst = $LEX.tokenlst
  typedef tokenopt = $LEX.tokenopt
//
  typedef symbol = $SYM.symbol
  typedef symbolist = $SYM.symbolist
  typedef symbolopt = $SYM.symbolopt
//
(* ****** ****** *)
(*
//
typedef tkint = token // int
typedef tkchr = token // char
typedef tkflt = token // float
typedef tkstr = token // string
//
typedef tkintopt = Option(tkint)
typedef tkchropt = Option(tkchr)
typedef tkfltopt = Option(tkflt)
typedef tkstropt = Option(tkstr)
//
*)
(* ****** ****** *)
//
abstbox t0int_tbox = ptr
abstbox t0chr_tbox = ptr
abstbox t0flt_tbox = ptr
abstbox t0str_tbox = ptr
//
abstbox i0dnt_tbox = ptr
//
(* ****** ****** *)
//
abstbox l0abl_tbox = ptr
//
(* ****** ****** *)
//
typedef t0int = t0int_tbox
typedef t0chr = t0chr_tbox
typedef t0flt = t0flt_tbox
typedef t0str = t0str_tbox
//
typedef i0dnt = i0dnt_tbox
//
(* ****** ****** *)
//
typedef s0tid = i0dnt_tbox
typedef s0eid = i0dnt_tbox
//
typedef d0pid = i0dnt_tbox
typedef d0eid = i0dnt_tbox
//
typedef l0abl = l0abl_tbox
//
(* ****** ****** *)
//
datatype
t0int_node =
  | T0INTnone of token
  | T0INTsome of token
datatype
t0chr_node =
  | T0CHRnone of token
  | T0CHRsome of token
datatype
t0flt_node =
  | T0FLTnone of token
  | T0FLTsome of token
datatype
t0str_node =
  | T0STRnone of token
  | T0STRsome of token
//
(* ****** ****** *)
//
datatype
i0dnt_node =
  | I0DNTnone of token
  | I0DNTsome of token
//
(* ****** ****** *)
(*
typedef t0int = $rec
{
  t0int_loc= loc_t, t0int_node= symbol
} (* end of [t0int] *)
typedef t0chr = $rec
{
  t0chr_loc= loc_t, t0chr_node= symbol
} (* end of [t0chr] *)
typedef t0flt = $rec
{
  t0flt_loc= loc_t, t0flt_node= symbol
} (* end of [t0flt] *)
typedef t0str = $rec
{
  t0str_loc= loc_t, t0str_node= symbol
} (* end of [t0str] *)
//
typedef i0dnt = $rec
{
  i0dnt_loc= loc_t, i0dnt_node= symbol
} (* end of [i0dnt] *)
*)
(* ****** ****** *)
//
fun
t0int_get_loc: (t0int) -> loc_t
fun
t0int_get_node: (t0int) -> t0int_node
//
#symload .loc with t0int_get_loc
#symload .node with t0int_get_node
//
fun t0int_none : token -> t0int
fun t0int_some : token -> t0int
//
fun print_t0int : print_type(t0int)
(*
fun prerr_t0int : prerr_type(t0int)
fun fprint_t0int : fprint_type(t0int)
*)
//
#symload print with print_t0int
(*
#symload prerr with prerr_t0int
#symload fprint with fprint_t0int
*)
//
(* ****** ****** *)
//
fun
t0chr_get_loc: (t0chr) -> loc_t
fun
t0chr_get_node: (t0chr) -> t0chr_node
//
#symload .loc with t0chr_get_loc
#symload .node with t0chr_get_node
//
fun t0chr_none : token -> t0chr
fun t0chr_some : token -> t0chr
//
fun print_t0chr : print_type(t0chr)
(*
fun prerr_t0chr : prerr_type(t0chr)
fun fprint_t0chr : fprint_type(t0chr)
*)
//
#symload print with print_t0chr
(*
#symload prerr with prerr_t0chr
#symload fprint with fprint_t0chr
*)
//
(* ****** ****** *)
//
fun
t0flt_get_loc: (t0flt) -> loc_t
fun
t0flt_get_node: (t0flt) -> t0flt_node
//
#symload .loc with t0flt_get_loc
#symload .node with t0flt_get_node
//
fun t0flt_none : token -> t0flt
fun t0flt_some : token -> t0flt
//
fun print_t0flt : print_type(t0flt)
(*
fun prerr_t0flt : prerr_type(t0flt)
fun fprint_t0flt : fprint_type(t0flt)
*)
//
#symload print with print_t0flt
(*
#symload prerr with prerr_t0flt
#symload fprint with fprint_t0flt
*)
//
(* ****** ****** *)
//
fun
t0str_get_loc: (t0str) -> loc_t
fun
t0str_get_node: (t0str) -> t0str_node
//
#symload .loc with t0str_get_loc
#symload .node with t0str_get_node
//
fun t0str_none : token -> t0str
fun t0str_some : token -> t0str
//
fun print_t0str : print_type(t0str)
(*
fun prerr_t0str : prerr_type(t0str)
fun fprint_t0str : fprint_type(t0str)
*)
//
#symload print with print_t0str
(*
#symload prerr with prerr_t0str
#symload fprint with fprint_t0str
*)
//
(* ****** ****** *)
//
typedef i0dnt = i0dnt_tbox
typedef i0dntlst = List0(i0dnt)
typedef i0dntopt = Option(i0dnt)
//
fun
i0dnt_get_loc
  : (i0dnt) -> loc_t
fun
i0dnt_get_node
  : (i0dnt) -> i0dnt_node
//
#symload .loc with i0dnt_get_loc
#symload .node with i0dnt_get_node
//
fun i0dnt_none : token -> i0dnt
fun i0dnt_some : token -> i0dnt
//
(* ****** ****** *)
//
fun print_i0dnt : print_type(i0dnt)
(*
fun prerr_i0dnt : prerr_type(i0dnt)
fun fprint_i0dnt : fprint_type(i0dnt)
*)
//
#symload print with print_i0dnt
(*
#symload prerr with prerr_i0dnt
#symload fprint with fprint_i0dnt
*)
//
(* ****** ****** *)
//
datatype
l0abl_node =
  | L0ABsome of label // valid
  | L0ABnone of (token) // invalid
//
fun
l0abl_get_loc(l0abl): loc_t
fun
l0abl_get_node(l0abl): l0abl_node
//
#symload .loc with l0abl_get_loc
#symload .node with l0abl_get_node
//
fun print_l0abl : print_type(l0abl)
(*
fun prerr_l0abl : prerr_type(l0abl)
fun fprint_l0abl : fprint_type(l0abl)
*)
//
#symload print with print_l0abl
(*
#symload prerr with prerr_l0abl
#symload fprint with fprint_l0abl
*)
//
fun
l0abl_make_int1(tok: token): l0abl
fun
l0abl_make_name(tok: token): l0abl
fun
l0abl_make_none(tok: token): l0abl
//
fun
l0abl_make_node
(loc: loc_t, node: l0abl_node): l0abl
//
(* ****** ****** *)
//
(*
s0ymb ::=
| i0dnt
| DOT l0ab | LBRACK RBRACK
*)
//
abstbox
s0ymb_tbox = ptr
typedef
s0ymb = s0ymb_tbox
//
datatype
s0ymb_node =
| S0YMBi0dnt of (i0dnt)
| S0YMBdtlab of (token, l0abl)
| S0YMBbrack of (token, token)
//
fun
s0ymb_get_loc: (s0ymb) -> loc_t
fun
s0ymb_get_node: (s0ymb) -> s0ymb_node
//
#symload .loc with s0ymb_get_loc
#symload .node with s0ymb_get_node
//
fun print_s0ymb : print_type(s0ymb)
(*
fun prerr_s0ymb : prerr_type(s0ymb)
fun fprint_s0ymb : fprint_type(s0ymb)
*)
//
#symload print with print_s0ymb
(*
#symload prerr with prerr_s0ymb
#symload fprint with fprint_s0ymb
*)
//
fun
s0ymb_make_node
(loc: loc_t, node: s0ymb_node): s0ymb
//
(* ****** ****** *)
//
datatype
sl0abled
  (a:tbox) =
  SL0ABLED of (l0abl, token, a)
//
fun
{a:tbox}
print_sl0abled(x0: sl0abled(a)): void
//
(* ****** ****** *)
//
datatype
sq0eid =
| SQ0EIDnone of (s0eid)
| SQ0EIDsome of (token, s0eid)
datatype
dq0eid =
| DQ0EIDnone of (d0eid)
| DQ0EIDsome of (token, d0eid)
//
fun sq0eid_get_loc(sq0eid): loc_t
fun dq0eid_get_loc(dq0eid): loc_t
//
#symload .loc with sq0eid_get_loc
#symload .loc with dq0eid_get_loc
//
fun
print_sq0eid: print_type(sq0eid)
fun
print_dq0eid: print_type(dq0eid)
//
#symload print with print_sq0eid
#symload print with print_dq0eid
//
(* ****** ****** *)
//
abstbox sort0_tbox = ptr
abstbox s0exp_tbox = ptr
//
abstbox s0arg_tbox = ptr
abstbox s0marg_tbox = ptr
//
abstbox t0arg_tbox = ptr
abstbox t0marg_tbox = ptr
//
(* ****** ****** *)
//
typedef sort0 = sort0_tbox
typedef sort0lst = List0(sort0)
typedef sort0opt = Option(sort0)
//
typedef s0exp = s0exp_tbox
typedef s0explst = List0(s0exp)
typedef s0expopt = Option(s0exp)
//
typedef s0arg = s0arg_tbox
typedef s0marg = s0marg_tbox
typedef s0arglst = List0(s0arg)
typedef s0marglst = List0(s0marg)
//
typedef t0arg = t0arg_tbox
typedef t0marg = t0marg_tbox
typedef t0arglst = List0(t0arg)
typedef t0marglst = List0(t0marg)
//
(* ****** ****** *)
//
datatype
sort0_node =
//
| S0Tid of (s0tid)
//
| S0Tint of (t0int)
//
| S0Tapps of
  (sort0lst) // HX: unsupported
//
| S0Tlist of
  (token, sort0lst, token) (*temporary*)
//
| S0Tqual of (token, sort0) // HX: qualified
(*
| S0Ttype of int(*kind*)
  (* prop/view/type/t0ype/viewtype/viewt0ype *)
*)
| S0Tnone of (token) // HX: (usually) indication of error
// end of [sort0_node]

(* ****** ****** *)
//
fun
sort0_get_loc(sort0): loc_t
fun
sort0_get_node(sort0): sort0_node
//
#symload .loc with sort0_get_loc
#symload .node with sort0_get_node
//
fun print_sort0 : print_type(sort0)
(*
fun prerr_sort0 : prerr_type(sort0)
fun fprint_sort0 : fprint_type(sort0)
*)
//
#symload print with print_sort0
(*
#symload prerr with prerr_sort0
#symload fprint with fprint_sort0
*)
//
fun
sort0_make_node
(loc: loc_t, node: sort0_node): sort0
//
(* ****** ****** *)
//
abstbox
s0rtcon_tbox = ptr
typedef
s0rtcon = s0rtcon_tbox
typedef
s0rtconlst = List0(s0rtcon)
//
datatype
s0rtcon_node =
| S0RTCON of (s0eid, sort0opt)
//
fun
s0rtcon_get_loc(s0rtcon): loc_t
fun
s0rtcon_get_node(s0rtcon): s0rtcon_node
//
#symload .loc with s0rtcon_get_loc
#symload .node with s0rtcon_get_node
//
fun print_s0rtcon : print_type(s0rtcon)
(*
fun prerr_s0rtcon : prerr_type(s0rtcon)
fun fprint_s0rtcon : fprint_type(s0rtcon)
*)
//
#symload print with print_s0rtcon
(*
#symload prerr with prerr_s0rtcon
#symload fprint with fprint_s0rtcon
*)
//
fun
s0rtcon_make_node
(loc: loc_t, node: s0rtcon_node): s0rtcon
//
(* ****** ****** *)
//
abstbox
d0tsort_tbox = ptr
typedef
d0tsort = d0tsort_tbox
typedef
d0tsortlst = List0(d0tsort)
//
datatype
d0tsort_node =
| D0TSORT of
  (s0tid, token(*EQ*), s0rtconlst)
  // D0TSORT
//
fun
d0tsort_get_loc
  (x0: d0tsort): loc_t
fun
d0tsort_get_node
  (x0: d0tsort): d0tsort_node
//
#symload .loc with d0tsort_get_loc
#symload .node with d0tsort_get_node
//
fun
print_d0tsort: print_type(d0tsort)
(*
fun
prerr_d0tsort: prerr_type(d0tsort)
fun
fprint_d0tsort: fprint_type(d0tsort)
*)
//
#symload print with print_d0tsort
(*
#symload prerr with prerr_d0tsort
#symload fprint with fprint_d0tsort
*)
//
fun
d0tsort_make_node
(loc: loc_t, node: d0tsort_node): d0tsort
//
(* ****** ****** *)
//
abstbox
s0rtdef_tbox = ptr
typedef
s0rtdef = s0rtdef_tbox
//
datatype
s0rtdef_node =
| S0RTDEFsort of sort0
| S0RTDEFsbst of
  (token, s0arg, token, s0explst, token)
//
fun
s0rtdef_get_loc(s0rtdef): loc_t
fun
s0rtdef_get_node(s0rtdef): s0rtdef_node
//
#symload .loc with s0rtdef_get_loc
#symload .node with s0rtdef_get_node
//
fun print_s0rtdef : print_type(s0rtdef)
(*
fun prerr_s0rtdef : prerr_type(s0rtdef)
fun fprint_s0rtdef : fprint_type(s0rtdef)
*)
//
#symload print with print_s0rtdef
(*
#symload prerr with prerr_s0rtdef
#symload fprint with fprint_s0rtdef
*)
//
fun
s0rtdef_make_node
(loc: loc_t, node: s0rtdef_node): s0rtdef
//
(* ****** ****** *)
//
datatype
s0arg_node =
  | S0ARGnone of token
  | S0ARGsome of (s0eid, sort0opt)
//
datatype
s0marg_node =
  | S0MARGnone of token
  | S0MARGsing of (s0eid)
  | S0MARGlist of (token, s0arglst, token)
//
(* ****** ****** *)
//
fun
s0arg_get_loc(s0arg): loc_t
fun
s0arg_get_node(s0arg): s0arg_node
//
#symload .loc with s0arg_get_loc
#symload .node with s0arg_get_node
//
fun print_s0arg : print_type(s0arg)
(*
fun prerr_s0arg : prerr_type(s0arg)
fun fprint_s0arg : fprint_type(s0arg)
*)
//
#symload print with print_s0arg
(*
#symload prerr with prerr_s0arg
#symload fprint with fprint_s0arg
*)
//
fun
s0arg_make_node
(loc: loc_t, node: s0arg_node): s0arg
//
(* ****** ****** *)
//
fun
s0marg_get_loc(s0marg): loc_t
fun
s0marg_get_node(s0marg): s0marg_node
//
#symload .loc with s0marg_get_loc
#symload .node with s0marg_get_node
//
fun print_s0marg : print_type(s0marg)
(*
fun prerr_s0marg : prerr_type(s0marg)
fun fprint_s0marg : fprint_type(s0marg)
*)
//
#symload print with print_s0marg
(*
#symload prerr with prerr_s0marg
#symload fprint with fprint_s0marg
*)
//
fun
s0marg_make_node
(loc: loc_t, node: s0marg_node): s0marg
//
(* ****** ****** *)
//
datatype
t0arg_node =
(*
  | T0ARGnone of token
*)
  | T0ARGsome of (sort0, tokenopt)
//
datatype
t0marg_node =
| T0MARGnone of token(*error*)
| T0MARGlist of
  (token(*'('*), t0arglst, token(*')'*))
//
(* ****** ****** *)
//
fun
t0arg_get_loc(t0arg): loc_t
fun
t0arg_get_node(t0arg): t0arg_node
//
#symload .loc with t0arg_get_loc
#symload .node with t0arg_get_node
//
fun print_t0arg : print_type(t0arg)
(*
fun prerr_t0arg : prerr_type(t0arg)
fun fprint_t0arg : fprint_type(t0arg)
*)
//
#symload print with print_t0arg
(*
#symload prerr with prerr_t0arg
#symload fprint with fprint_t0arg
*)
//
fun
t0arg_make_node
(loc: loc_t, node: t0arg_node): t0arg
//
(* ****** ****** *)
//
fun
t0marg_get_loc(t0marg): loc_t
fun
t0marg_get_node(t0marg): t0marg_node
//
#symload .loc with t0marg_get_loc
#symload .node with t0marg_get_node
//
fun print_t0marg : print_type(t0marg)
(*
fun prerr_t0marg : prerr_type(t0marg)
fun fprint_t0marg : fprint_type(t0marg)
*)
//
#symload print with print_t0marg
(*
#symload prerr with prerr_t0marg
#symload fprint with fprint_t0marg
*)
//
fun
t0marg_make_node
(loc: loc_t, node: t0marg_node): t0marg
//
(* ****** ****** *)
//
abstbox s0qua_tbox = ptr
typedef s0qua = s0qua_tbox
typedef s0qualst = List0(s0qua)
//
datatype
s0qua_node =
| S0QUAprop of (s0exp)
| S0QUAvars of (i0dntlst, sort0opt)
//
fun
s0qua_get_loc(s0qua): loc_t
fun
s0qua_get_node(s0qua): s0qua_node
//
#symload .loc with s0qua_get_loc
#symload .node with s0qua_get_node
//
fun print_s0qua : print_type(s0qua)
(*
fun prerr_s0qua : prerr_type(s0qua)
fun fprint_s0qua : fprint_type(s0qua)
*)
//
#symload print with print_s0qua
(*
#symload prerr with prerr_s0qua
#symload fprint with fprint_s0qua
*)
//
fun
s0qua_make_node
(loc: loc_t, node: s0qua_node): s0qua
//
(* ****** ****** *)
//
abstbox s0uni_tbox = ptr
typedef s0uni = s0uni_tbox
typedef s0unilst = List0(s0uni)
//
datatype
s0uni_node =
| S0UNInone of (token)
| S0UNIsome of (token, s0qualst, token)
//
fun
s0uni_get_loc(s0uni): loc_t
fun
s0uni_get_node(s0uni): s0uni_node
//
#symload .loc with s0uni_get_loc
#symload .node with s0uni_get_node
//
fun print_s0uni : print_type(s0uni)
(*
fun prerr_s0uni : prerr_type(s0uni)
fun fprint_s0uni : fprint_type(s0uni)
*)
//
#symload print with print_s0uni
(*
#symload prerr with prerr_s0uni
#symload fprint with fprint_s0uni
*)
//
fun
s0uni_make_node
(loc: loc_t, node: s0uni_node): s0uni
//
(* ****** ****** *)
//
typedef labs0exp = sl0abled(s0exp)
typedef labs0explst = List0(labs0exp)
//
(* ****** ****** *)

datatype
s0exp_node =
//
| S0Eid of (s0eid)
//
| S0Eop1 of (token) // op_symid
| S0Eop2 of (token, s0eid, token) // op(...)
//
| S0Eint of (t0int)
| S0Echr of (t0chr)
| S0Eflt of (t0flt)
| S0Estr of (t0str)
//
| S0Eapps of s0explst
//
| S0Eimp of
    (token, s0explst, token)
  // end of [S0Eimp]
//
| S0Eparen of
    (token, s0explst, s0exp_RPAREN)
  // end of [S0Eparen]
//
| S0Etuple of
    (token, tokenopt, s0explst, s0exp_RPAREN)
| S0Erecord of
    (token, tokenopt, labs0explst, labs0exp_RBRACE)
//
| S0Eforall of
  (token, s0qualst, token) // universal
| S0Eexists of
  (token, s0qualst, token) // existential
//
| S0Elam of
  ( token, s0marglst
  , sort0opt, token, s0exp, tokenopt)
//
| S0Eanno of
    (s0exp, sort0) // sort annotation
  // end of [S0Eanno]
//
| S0Equal of
    (token, s0exp) // qualified staexp
  // end of [S0Equal]
//
| S0Enone of (token) // HX-2018-07-08: indicating error
// end of [s0exp_node]
//
and
s0exp_RPAREN =
| s0exp_RPAREN_cons0 of token
| s0exp_RPAREN_cons1 of (token, s0explst, token)
//
and
labs0exp_RBRACE =
| labs0exp_RBRACE_cons0 of token
| labs0exp_RBRACE_cons1 of (token, labs0explst, token)
//
(* ****** ****** *)
//
fun
s0exp_get_loc(s0exp): loc_t
fun
s0exp_get_node(s0exp): s0exp_node
//
#symload .loc with s0exp_get_loc
#symload .node with s0exp_get_node
//
fun print_s0exp : print_type(s0exp)
(*
fun prerr_s0exp : prerr_type(s0exp)
fun fprint_s0exp : fprint_type(s0exp)
*)
//
#symload print with print_s0exp
(*
#symload prerr with prerr_s0exp
#symload fprint with fprint_s0exp
*)
//
fun
s0exp_anno_opt
(s0exp, sort0opt): s0exp
fun
s0exp_make_node
(loc: loc_t, node: s0exp_node): s0exp
//
(* ****** ****** *)
//
fun
s0exp_RPAREN_loc(s0exp_RPAREN): loc_t
//
fun
print_s0exp_RPAREN: print_type(s0exp_RPAREN)
(*
fun
prerr_s0exp_RPAREN: print_type(s0exp_RPAREN)
fun
fprint_s0exp_RPAREN: fprint_type(s0exp_RPAREN)
*)
//
#symload print with print_s0exp_RPAREN
(*
#symload prerr with prerr_s0exp_RPAREN
#symload fprint with fprint_s0exp_RPAREN
*)
//
(* ****** ****** *)
//
fun
labs0exp_RBRACE_loc(labs0exp_RBRACE): loc_t
//
fun
print_labs0exp_RBRACE: print_type(labs0exp_RBRACE)
(*
fun
prerr_labs0exp_RBRACE: prerr_type(labs0exp_RBRACE)
fun
fprint_labs0exp_RBRACE: fprint_type(labs0exp_RBRACE)
*)
//
#symload print with print_labs0exp_RBRACE
(*
#symload prerr with prerr_labs0exp_RBRACE
#symload fprint with fprint_labs0exp_RBRACE
*)
//
(* ****** ****** *)
//
//
// HX-2019-02-18:
// There is no longer plan
// to support effect-tracking!!!
//
(*
//
datatype
s0eff =
| S0EFFnone of
  (token(*:*)) // HX: default
| S0EFFsome of
  (token(*:<*), s0explst, token) // HX: annotated
//
fun
print_s0eff: print_type(s0eff)
fun
prerr_s0eff: prerr_type(s0eff)
fun
fprint_s0eff: fprint_type(s0eff)
//
#symload print with print_s0eff
#symload prerr with prerr_s0eff
#symload fprint with fprint_s0eff
//
*)
//
datatype
effs0expopt =
| EFFS0EXPnone of ()
| EFFS0EXPsome of (s0exp)
(*
| EFFS0EXPsome of (s0eff, s0exp)
*)
//
fun
print_effs0expopt:
print_type(effs0expopt)
(*
fun
prerr_effs0expopt:
prerr_type(effs0expopt)
fun
fprint_effs0expopt: fprint_type(effs0expopt)
*)
//
#symload print with print_effs0expopt
(*
#symload prerr with prerr_effs0expopt
#symload fprint with fprint_effs0expopt
*)
//
(* ****** ****** *)
//
abstbox
d0atcon_tbox = ptr
typedef
d0atcon = d0atcon_tbox
typedef
d0atconlst = List0(d0atcon)
//
datatype
d0atcon_node =
| D0ATCON of
  ( s0unilst
  , d0eid(*nm*), s0explst, s0expopt)
//
fun
d0atcon_get_loc(d0atcon): loc_t
fun
d0atcon_get_node(d0atcon): d0atcon_node
//
#symload .loc with d0atcon_get_loc
#symload .node with d0atcon_get_node
//
fun print_d0atcon : print_type(d0atcon)
(*
fun prerr_d0atcon : prerr_type(d0atcon)
fun fprint_d0atcon : fprint_type(d0atcon)
*)
//
#symload print with print_d0atcon
(*
#symload prerr with prerr_d0atcon
#symload fprint with fprint_d0atcon
*)
//
fun
d0atcon_make_node
(loc: loc_t, node: d0atcon_node): d0atcon
//
(* ****** ****** *)
//
abstbox
d0atype_tbox = ptr
typedef
d0atype = d0atype_tbox
typedef
d0atypelst = List0(d0atype)
//
datatype
d0atype_node =
| D0ATYPE of
  ( d0eid
  , t0marglst
  , sort0opt(*res*), token, d0atconlst)
  // D0ATYPE
//
fun
d0atype_get_loc
  (x0: d0atype): loc_t
fun
d0atype_get_node
  (x0: d0atype): d0atype_node
//
#symload .loc with d0atype_get_loc
#symload .node with d0atype_get_node
//
fun print_d0atype : print_type(d0atype)
(*
fun prerr_d0atype : prerr_type(d0atype)
fun fprint_d0atype : fprint_type(d0atype)
*)
//
#symload print with print_d0atype
(*
#symload prerr with prerr_d0atype
#symload fprint with fprint_d0atype
*)
//
fun
d0atype_make_node
(loc: loc_t, node: d0atype_node): d0atype
//
(* ****** ****** *)

(* end of [xats_staexp0.sats] *)


////

(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS/Postiats - Unleashing the Potential of Types!
** Copyright (C) 2018 Hongwei Xi, ATS Trustful Software, Inc.
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of  the GNU GENERAL PUBLIC LICENSE (GPL) as published by the
** Free Software Foundation; either version 3, or (at  your  option)  any
** later version.
**
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
**
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)
//
// Author: Hongwei Xi
// Start Time: April, 2018
// Authoremail: gmhwxiATgmailDOTcom
//
(* ****** ****** *)
//
#include "./share.sats"
(* ****** ****** *)

//
#staload
LAB = "./label0.sats"
#staload
LOC = "./location.sats"
//
  typedef label = $LAB.label
  typedef loc_t = $LOC.location
//
#staload LEX = "./lexing.sats"
#staload SYM = "./symbol.sats"
//
  typedef token = $LEX.token
  typedef tokenlst = $LEX.tokenlst
  typedef tokenopt = $LEX.tokenopt
//
  typedef symbol = $SYM.symbol
  typedef symbolist = $SYM.symbolist
  typedef symbolopt = $SYM.symbolopt
//
(* ****** ****** *)
(*
//
typedef tkint = token // int
typedef tkchr = token // char
typedef tkflt = token // float
typedef tkstr = token // string
//
typedef tkintopt = Option(tkint)
typedef tkchropt = Option(tkchr)
typedef tkfltopt = Option(tkflt)
typedef tkstropt = Option(tkstr)
//
*)
(* ****** ****** *)
//
abstbox t0int_tbox = ptr
abstbox t0chr_tbox = ptr
abstbox t0flt_tbox = ptr
abstbox t0str_tbox = ptr
//
abstbox i0dnt_tbox = ptr
//
(* ****** ****** *)
//
abstbox l0abl_tbox = ptr
//
(* ****** ****** *)
//
typedef t0int = t0int_tbox
typedef t0chr = t0chr_tbox
typedef t0flt = t0flt_tbox
typedef t0str = t0str_tbox
//
typedef i0dnt = i0dnt_tbox
//
(* ****** ****** *)
//
typedef s0tid = i0dnt_tbox
typedef s0eid = i0dnt_tbox
typedef d0eid = i0dnt_tbox
//
typedef l0abl = l0abl_tbox
//
(* ****** ****** *)
//
datatype
t0int_node =
  | T0INTnone of token
  | T0INTsome of token
datatype
t0chr_node =
  | T0CHRnone of token
  | T0CHRsome of token
datatype
t0flt_node =
  | T0FLTnone of token
  | T0FLTsome of token
datatype
t0str_node =
  | T0STRnone of token
  | T0STRsome of token
//
(* ****** ****** *)
//
datatype
i0dnt_node =
  | I0DNTnone of token
  | I0DNTsome of token
//
(* ****** ****** *)
(*
typedef t0int = $rec
{
  t0int_loc= loc_t, t0int_node= symbol
} (* end of [t0int] *)
typedef t0chr = $rec
{
  t0chr_loc= loc_t, t0chr_node= symbol
} (* end of [t0chr] *)
typedef t0flt = $rec
{
  t0flt_loc= loc_t, t0flt_node= symbol
} (* end of [t0flt] *)
typedef t0str = $rec
{
  t0str_loc= loc_t, t0str_node= symbol
} (* end of [t0str] *)
//
typedef i0dnt = $rec
{
  i0dnt_loc= loc_t, i0dnt_node= symbol
} (* end of [i0dnt] *)
*)
(* ****** ****** *)
//
fun
t0int_get_loc: (t0int) -> loc_t
fun
t0int_get_node: (t0int) -> t0int_node
//
#symload .loc with t0int_get_loc
#symload .node with t0int_get_node
//
fun t0int_none : token -> t0int
fun t0int_some : token -> t0int
//
fun print_t0int : print_type(t0int)
fun prerr_t0int : prerr_type(t0int)
fun fprint_t0int : fprint_type(t0int)
//
#symload print with print_t0int
#symload prerr with prerr_t0int
#symload fprint with fprint_t0int
//
(* ****** ****** *)
//
fun
t0chr_get_loc: (t0chr) -> loc_t
fun
t0chr_get_node: (t0chr) -> t0chr_node
//
#symload .loc with t0chr_get_loc
#symload .node with t0chr_get_node
//
fun t0chr_none : token -> t0chr
fun t0chr_some : token -> t0chr
//
fun print_t0chr : print_type(t0chr)
fun prerr_t0chr : prerr_type(t0chr)
fun fprint_t0chr : fprint_type(t0chr)
//
#symload print with print_t0chr
#symload prerr with prerr_t0chr
#symload fprint with fprint_t0chr
//
(* ****** ****** *)
//
fun
t0flt_get_loc: (t0flt) -> loc_t
fun
t0flt_get_node: (t0flt) -> t0flt_node
//
#symload .loc with t0flt_get_loc
#symload .node with t0flt_get_node
//
fun t0flt_none : token -> t0flt
fun t0flt_some : token -> t0flt
//
fun print_t0flt : print_type(t0flt)
fun prerr_t0flt : prerr_type(t0flt)
fun fprint_t0flt : fprint_type(t0flt)
//
#symload print with print_t0flt
#symload prerr with prerr_t0flt
#symload fprint with fprint_t0flt
//
(* ****** ****** *)
//
fun
t0str_get_loc: (t0str) -> loc_t
fun
t0str_get_node: (t0str) -> t0str_node
//
#symload .loc with t0str_get_loc
#symload .node with t0str_get_node
//
fun t0str_none : token -> t0str
fun t0str_some : token -> t0str
//
fun print_t0str : print_type(t0str)
fun prerr_t0str : prerr_type(t0str)
fun fprint_t0str : fprint_type(t0str)
//
#symload print with print_t0str
#symload prerr with prerr_t0str
#symload fprint with fprint_t0str
//
(* ****** ****** *)
//
typedef i0dnt = i0dnt_tbox
typedef i0dntlst = List0(i0dnt)
typedef i0dntopt = Option(i0dnt)
//
fun
i0dnt_get_loc
  : (i0dnt) -> loc_t
fun
i0dnt_get_node
  : (i0dnt) -> i0dnt_node
//
#symload .loc with i0dnt_get_loc
#symload .node with i0dnt_get_node
//
fun i0dnt_none : token -> i0dnt
fun i0dnt_some : token -> i0dnt
//
(* ****** ****** *)
//
fun print_i0dnt : print_type(i0dnt)
fun prerr_i0dnt : prerr_type(i0dnt)
fun fprint_i0dnt : fprint_type(i0dnt)
//
#symload print with print_i0dnt
#symload prerr with prerr_i0dnt
#symload fprint with fprint_i0dnt
//
(* ****** ****** *)
//
datatype
l0abl_node =
  | L0ABsome of label // valid
  | L0ABnone of (token) // invalid
//
fun
l0abl_get_loc(l0abl): loc_t
fun
l0abl_get_node(l0abl): l0abl_node
//
#symload .loc with l0abl_get_loc
#symload .node with l0abl_get_node
//
fun print_l0abl : print_type(l0abl)
fun prerr_l0abl : prerr_type(l0abl)
fun fprint_l0abl : fprint_type(l0abl)
//
#symload print with print_l0abl
#symload prerr with prerr_l0abl
#symload fprint with fprint_l0abl
//
fun
l0abl_make_int1(tok: token): l0abl
fun
l0abl_make_name(tok: token): l0abl
fun
l0abl_make_none(tok: token): l0abl
//
fun
l0abl_make_node
(loc: loc_t, node: l0abl_node): l0abl
//
(* ****** ****** *)
//
datatype
sl0abeled
  (a:tflt) =
  SL0ABELED of (l0abl, token, a)
//
fun
{a:tflt}
print_sl0abeled
  (x0: sl0abeled(a)): void
//
(* ****** ****** *)
//
abstbox sort0_tbox = ptr
abstbox s0exp_tbox = ptr
//
(* ****** ****** *)
//
typedef sort0 = sort0_tbox
typedef sort0lst = List0(sort0)
typedef sort0opt = Option(sort0)
//
typedef s0exp = s0exp_tbox
typedef s0explst = List0(s0exp)
typedef s0expopt = Option(s0exp)
//
(* ****** ****** *)
//
datatype
sort0_node =
//
| S0Tid of (s0tid)
//
| S0Tapps of (sort0lst) // HX: unsupported
//
| S0Tlist of (token, sort0lst, token) (* for temporary use *)
//
| S0Tqual of (token, sort0) // HX: qualified
(*
| S0Ttype of int (* prop/view/type/t0ype/viewtype/viewt0ype *)
*)
| S0Tnone of (token)
// end of [sort0_node]

(* ****** ****** *)
//
fun
sort0_get_loc(sort0): loc_t
fun
sort0_get_node(sort0): sort0_node
//
#symload .loc with sort0_get_loc
#symload .node with sort0_get_node
//
fun print_sort0 : print_type(sort0)
(*
fun prerr_sort0 : prerr_type(sort0)
fun fprint_sort0 : fprint_type(sort0)
*)
//
#symload print with print_sort0
(*
#symload prerr with prerr_sort0
#symload fprint with fprint_sort0
*)
//
fun
sort0_make_node
(loc: loc_t, node: sort0_node): sort0
//
(* ****** ****** *)
//
abstbox
s0rtcon_tbox = ptr
typedef
s0rtcon = s0rtcon_tbox
typedef
s0rtconlst = List0(s0rtcon)
//
datatype
s0rtcon_node =
| S0RTCON of (s0eid, sort0opt)
//
fun
s0rtcon_get_loc(s0rtcon): loc_t
fun
s0rtcon_get_node(s0rtcon): s0rtcon_node
//
#symload .loc with s0rtcon_get_loc
#symload .node with s0rtcon_get_node
//
fun print_s0rtcon : print_type(s0rtcon)
(*
fun prerr_s0rtcon : prerr_type(s0rtcon)
fun fprint_s0rtcon : fprint_type(s0rtcon)
*)
//
#symload print with print_s0rtcon
(*
#symload prerr with prerr_s0rtcon
#symload fprint with fprint_s0rtcon
*)
//
fun
s0rtcon_make_node
(loc: loc_t, node: s0rtcon_node): s0rtcon
//
(* ****** ****** *)
//
abstbox
d0tsort_tbox = ptr
typedef
d0tsort = d0tsort_tbox
typedef
d0tsortlst = List0(d0tsort)
//
datatype
d0tsort_node =
| D0TSORT of
    (s0tid, token, s0rtconlst)
  // D0TSORT
//
fun
d0tsort_get_loc
  (x0: d0tsort): loc_t
fun
d0tsort_get_node
  (x0: d0tsort): d0tsort_node
//
#symload .loc with d0tsort_get_loc
#symload .node with d0tsort_get_node
//
fun
print_d0tsort: print_type(d0tsort)
(*
fun
prerr_d0tsort: prerr_type(d0tsort)
fun
fprint_d0tsort: fprint_type(d0tsort)
*)
//
#symload print with print_d0tsort
(*
#symload prerr with prerr_d0tsort
#symload fprint with fprint_d0tsort
*)
//
fun
d0tsort_make_node
(loc: loc_t, node: d0tsort_node): d0tsort
//
(* ****** ****** *)
//
abstbox s0arg_tbox = ptr
typedef s0arg = s0arg_tbox
typedef s0arglst = List0(s0arg)
//
datatype
s0arg_node =
  | S0ARGnone of token
  | S0ARGsome of (s0eid, sort0opt)
//
fun
s0arg_get_loc(s0arg): loc_t
fun
s0arg_get_node(s0arg): s0arg_node
//
#symload .loc with s0arg_get_loc
#symload .node with s0arg_get_node
//
fun print_s0arg : print_type(s0arg)
(*
fun prerr_s0arg : prerr_type(s0arg)
fun fprint_s0arg : fprint_type(s0arg)
*)
//
#symload print with print_s0arg
(*
#symload prerr with prerr_s0arg
#symload fprint with fprint_s0arg
*)
//
fun
s0arg_make_node
(loc: loc_t, node: s0arg_node): s0arg
//
(* ****** ****** *)
//
abstbox
s0rtdef_tbox = ptr
typedef
s0rtdef = s0rtdef_tbox
//
datatype
s0rtdef_node =
| S0RTDEFsort of sort0
| S0RTDEFsubset of
  (token, s0arg, token, s0explst, token)
//
fun
s0rtdef_get_loc(s0rtdef): loc_t
fun
s0rtdef_get_node(s0rtdef): s0rtdef_node
//
#symload .loc with s0rtdef_get_loc
#symload .node with s0rtdef_get_node
//
fun print_s0rtdef : print_type(s0rtdef)
(*
fun prerr_s0rtdef : prerr_type(s0rtdef)
fun fprint_s0rtdef : fprint_type(s0rtdef)
*)
//
#symload print with print_s0rtdef
(*
#symload prerr with prerr_s0rtdef
#symload fprint with fprint_s0rtdef
*)
//
fun
s0rtdef_make_node
(loc: loc_t, node: s0rtdef_node): s0rtdef
//
(* ****** ****** *)
//
abstbox s0marg_tbox = ptr
typedef s0marg = s0marg_tbox
typedef s0marglst = List0(s0marg)
//
datatype
s0marg_node =
  | S0MARGnone of token
  | S0MARGsing of (s0eid)
  | S0MARGlist of (token, s0arglst, token)
//
fun
s0marg_get_loc(s0marg): loc_t
fun
s0marg_get_node(s0marg): s0marg_node
//
#symload .loc with s0marg_get_loc
#symload .node with s0marg_get_node
//
fun print_s0marg : print_type(s0marg)
(*
fun prerr_s0marg : prerr_type(s0marg)
fun fprint_s0marg : fprint_type(s0marg)
*)
//
#symload print with print_s0marg
(*
#symload prerr with prerr_s0marg
#symload fprint with fprint_s0marg
*)
//
fun
s0marg_make_node
(loc: loc_t, node: s0marg_node): s0marg
//
(* ****** ****** *)
//
abstbox t0arg_tbox = ptr
typedef t0arg = t0arg_tbox
typedef t0arglst = List0(t0arg)
//
datatype
t0arg_node =
(*
  | T0ARGnone of token
*)
  | T0ARGsome of (sort0, tokenopt)
//
fun
t0arg_get_loc(t0arg): loc_t
fun
t0arg_get_node(t0arg): t0arg_node
//
#symload .loc with t0arg_get_loc
#symload .node with t0arg_get_node
//
fun print_t0arg : print_type(t0arg)
(*
fun prerr_t0arg : prerr_type(t0arg)
fun fprint_t0arg : fprint_type(t0arg)
*)
//
#symload print with print_t0arg
(*
#symload prerr with prerr_t0arg
#symload fprint with fprint_t0arg
*)
//
fun
t0arg_make_node
(loc: loc_t, node: t0arg_node): t0arg
//
(* ****** ****** *)
//
abstbox t0marg_tbox = ptr
typedef t0marg = t0marg_tbox
typedef t0marglst = List0(t0marg)
//
datatype
t0marg_node =
  | T0MARGnone of token
  | T0MARGlist of (token, t0arglst, token)
//
fun
t0marg_get_loc(t0marg): loc_t
fun
t0marg_get_node(t0marg): t0marg_node
//
#symload .loc with t0marg_get_loc
#symload .node with t0marg_get_node
//
fun print_t0marg : print_type(t0marg)
(*
fun prerr_t0marg : prerr_type(t0marg)
fun fprint_t0marg : fprint_type(t0marg)
*)
//
#symload print with print_t0marg
(*
#symload prerr with prerr_t0marg
#symload fprint with fprint_t0marg
*)
//
fun
t0marg_make_node
(loc: loc_t, node: t0marg_node): t0marg
//
(* ****** ****** *)
//
abstbox s0qua_tbox = ptr
typedef s0qua = s0qua_tbox
typedef s0qualst = List0(s0qua)
//
datatype
s0qua_node =
| S0QUAprop of s0exp
| S0QUAvars of (i0dntlst, token, sort0)
//
fun
s0qua_get_loc(s0qua): loc_t
fun
s0qua_get_node(s0qua): s0qua_node
//
#symload .loc with s0qua_get_loc
#symload .node with s0qua_get_node
//
fun print_s0qua : print_type(s0qua)
(*
fun prerr_s0qua : prerr_type(s0qua)
fun fprint_s0qua : fprint_type(s0qua)
*)
//
#symload print with print_s0qua
(*
#symload prerr with prerr_s0qua
#symload fprint with fprint_s0qua
*)
//
fun
s0qua_make_node
(loc: loc_t, node: s0qua_node): s0qua
//
(* ****** ****** *)
//
abstbox s0uni_tbox = ptr
typedef s0uni = s0uni_tbox
typedef s0unilst = List0(s0uni)
//
datatype
s0uni_node =
| S0UNInone of (token)
| S0UNIsome of (token, s0qualst, token)
//
fun
s0uni_get_loc(s0uni): loc_t
fun
s0uni_get_node(s0uni): s0uni_node
//
#symload .loc with s0uni_get_loc
#symload .node with s0uni_get_node
//
fun print_s0uni : print_type(s0uni)
(*
fun prerr_s0uni : prerr_type(s0uni)
fun fprint_s0uni : fprint_type(s0uni)
*)
//
#symload print with print_s0uni
(*
#symload prerr with prerr_s0uni
#symload fprint with fprint_s0uni
*)
//
fun
s0uni_make_node
(loc: loc_t, node: s0uni_node): s0uni
//
(* ****** ****** *)
//
typedef labs0exp = sl0abeled(s0exp)
typedef labs0explst = List0(labs0exp)
//
(* ****** ****** *)

datatype
s0exp_node =
//
| S0Eid of (s0eid)
//
| S0Eop1 of (token) // op_symid
| S0Eop2 of (token, s0eid, token) // op(...)
//
| S0Eint of (t0int)
| S0Echr of (t0chr)
| S0Eflt of (t0flt)
| S0Estr of (t0str)
//
| S0Eapps of s0explst
//
| S0Eparen of
    (token, s0explst, s0exp_RPAREN)
//
| S0Etuple of
    (token, tokenopt, s0explst, s0exp_RPAREN)
| S0Erecord of
    (token, tokenopt, labs0explst, labs0exp_RBRACE)
//
| S0Eforall of
  (token, s0qualst, token) // universal
| S0Eexists of
  (token, s0qualst, token) // existential
//
| S0Elam of
  (token, s0marglst, sort0opt, token, s0exp)
//
| S0Eanno of (s0exp, sort0) // sort annotation
//
| S0Equal of (token, s0exp) // qualified staexp
//
| S0Enone of (token) // HX-2018-07-08: indicating error
// end of [s0exp_node]
//
and
s0exp_RPAREN =
| s0exp_RPAREN_cons0 of token
| s0exp_RPAREN_cons1 of (token, s0explst, token)
//
and
labs0exp_RBRACE =
| labs0exp_RBRACE_cons0 of token
| labs0exp_RBRACE_cons1 of (token, labs0explst, token)
//
(* ****** ****** *)
//
fun
s0exp_get_loc(s0exp): loc_t
fun
s0exp_get_node(s0exp): s0exp_node
//
#symload .loc with s0exp_get_loc
#symload .node with s0exp_get_node
//
fun print_s0exp : print_type(s0exp)
(*
fun prerr_s0exp : prerr_type(s0exp)
fun fprint_s0exp : fprint_type(s0exp)
*)
//
#symload print with print_s0exp
(*
#symload prerr with prerr_s0exp
#symload fprint with fprint_s0exp
*)
//
fun
s0exp_make_node
(loc: loc_t, node: s0exp_node): s0exp
//
(* ****** ****** *)
//
fun
s0exp_RPAREN_loc(s0exp_RPAREN): loc_t
//
fun
print_s0exp_RPAREN: print_type(s0exp_RPAREN)
(*
fun
prerr_s0exp_RPAREN: print_type(s0exp_RPAREN)
fun
fprint_s0exp_RPAREN: fprint_type(s0exp_RPAREN)
*)
//
#symload print with print_s0exp_RPAREN
(*
#symload prerr with prerr_s0exp_RPAREN
#symload fprint with fprint_s0exp_RPAREN
*)
//
(* ****** ****** *)
//
fun
labs0exp_RBRACE_loc(labs0exp_RBRACE): loc_t
//
fun
print_labs0exp_RBRACE: print_type(labs0exp_RBRACE)
(*
fun
prerr_labs0exp_RBRACE: prerr_type(labs0exp_RBRACE)
fun
fprint_labs0exp_RBRACE: fprint_type(labs0exp_RBRACE)
*)
//
#symload print with print_labs0exp_RBRACE
(*
#symload prerr with prerr_labs0exp_RBRACE
#symload fprint with fprint_labs0exp_RBRACE
*)
//
(* ****** ****** *)
//
abstbox
d0atcon_tbox = ptr
typedef
d0atcon = d0atcon_tbox
typedef
d0atconlst = List0(d0atcon)
//
datatype
d0atcon_node =
| D0ATCON of
  (s0unilst, d0eid, s0exp, s0expopt)
//
fun
d0atcon_get_loc(d0atcon): loc_t
fun
d0atcon_get_node(d0atcon): d0atcon_node
//
#symload .loc with d0atcon_get_loc
#symload .node with d0atcon_get_node
//
fun print_d0atcon : print_type(d0atcon)
(*
fun prerr_d0atcon : prerr_type(d0atcon)
fun fprint_d0atcon : fprint_type(d0atcon)
*)
//
#symload print with print_d0atcon
(*
#symload prerr with prerr_d0atcon
#symload fprint with fprint_d0atcon
*)
//
fun
d0atcon_make_node
(loc: loc_t, node: d0atcon_node): d0atcon
//
(* ****** ****** *)
//
abstbox
d0atype_tbox = ptr
typedef
d0atype = d0atype_tbox
typedef
d0atypelst = List0(d0atype)
//
datatype
d0atype_node =
| D0ATYPE of
    (d0eid, t0marglst, token, d0atconlst)
  // D0ATYPE
//
fun
d0atype_get_loc
  (x0: d0atype): loc_t
fun
d0atype_get_node
  (x0: d0atype): d0atype_node
//
#symload .loc with d0atype_get_loc
#symload .node with d0atype_get_node
//
fun print_d0atype : print_type(d0atype)
(*
fun prerr_d0atype : prerr_type(d0atype)
fun fprint_d0atype : fprint_type(d0atype)
*)
//
#symload print with print_d0atype
(*
#symload prerr with prerr_d0atype
#symload fprint with fprint_d0atype
*)
//
fun
d0atype_make_node
(loc: loc_t, node: d0atype_node): d0atype
//
(* ****** ****** *)

(* end of [xats_staexp0.sats] *)
