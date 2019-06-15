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
// Start Time: May, 2018
// Authoremail: gmhwxiATgmailDOTcom
//
(* ****** ****** *)

#staload
UN = "libats/SATS/unsafe.sats"

#staload "libats/SATS/print.sats"
#staload _ = "libats/DATS/print.dats"

#staload "libats/SATS/gint.sats"
#staload _ = "libats/DATS/gint.dats"

#staload "libats/SATS/char.sats"
#staload _ = "libats/DATS/char.dats"

#staload "libats/SATS/list.sats"
#staload _ = "libats/DATS/list.dats"

#staload "libats/SATS/list_vt.sats"
#staload _ = "libats/DATS/list_vt.dats"

#staload "libats/SATS/array.sats"
#staload _ = "libats/DATS/array.dats"

(* ****** ****** *)

#staload "./../SATS/basics.sats"
#staload "./../SATS/lexing.sats"
#staload "./../SATS/location.sats"

(* ****** ****** *)

local

absimpl
token_tbox = $rec{
token_loc= loc_t, token_node= tnode
} (* token_tbox *)

in (* in-of-local *)

implement
//{}//tmp
token_make_node
  (loc, node) = $rec
{
  token_loc= loc, token_node= node
} (* end of [token_make] *)

implement
//{}//tmp
token_get_loc(tok) = tok.token_loc
implement
//{}//tmp
token_get_node(tok) = tok.token_node

end // end of [local]

(* ****** ****** *)
//
(*
implement
print_tnode
  (tok) = print_tnode(stdout_ref, tok)
implement
prerr_tnode
  (tok) =
  fprint_tnode(stderr_ref, tok)
*)
//
(* ****** ****** *)
//
implement
print_tnode(tnd) =
(
case+ tnd of
//
| T_EOF() => print("EOF")
| T_ERR() => print("ERR")
//
| T_EOL() => print("EOL")
//
| T_BLANK(x) =>
  print!("BLANK(", x, ")")
//
| T_CLNLT(x) =>
  print!("CLNLT(", x, ")")
| T_DOTLT(x) =>
  print!("DOTLT(", x, ")")
//
| T_IDENT_alp(x) =>
  print!("IDENT_alp(", x, ")")
| T_IDENT_sym(x) =>
  print!("IDENT_sym(", x, ")")
//
| T_IDENT_srp(x) =>
  print!("IDENT_srp(", x, ")")
| T_IDENT_dlr(x) =>
  print!("IDENT_dlr(", x, ")")
//
| T_IDENT_qual(x) =>
  print!("IDENT_qual(", x, ")")
//
| T_INT1(rep) =>
  print!("INT(", rep, ")")
| T_INT2(base, rep) =>
  print!("INT(", base, ", ", rep, ")")
| T_INT3(base, rep, _(*sfx*)) =>
  print!("INT(", base, ", ", rep, ")")
//
| T_FLOAT1(rep) =>
  print!("FLOAT(", rep, ")")
| T_FLOAT2(base, rep) =>
  print!("FLOAT(", base, ", ", rep, ")")
| T_FLOAT3(base, rep, _(*sfx*)) =>
  print!("FLOAT(", base, ", ", rep, ")")
//
(*
| T_CHAR(chr) =>
  let
    val chr = int2char0(chr)
  in
    print!("CHAR(", chr, ")")
  end
*)
| T_CHAR_nil(rep) =>
  print!("CHAR_nil(", rep, ")")
| T_CHAR_char(rep) =>
  print!("CHAR_char(", rep, ")")
| T_CHAR_slash(rep) =>
  print!("CHAR_slash(", rep, ")")
//
| T_STRING_closed(str) =>
  print!("STRING_closed(", str, ")")
| T_STRING_unclsd(str) =>
  print!("STRING_unclsd(", str, ")")
//
(*
| T_CDATA(cdata, asz) => print!("CDATA(...)")
*)
//
| T_SPECHAR(c) =>
  print!("SPECHAR(", char0_chr(* int2char0 *)(c), ")")
//
| T_COMMENT_line
    (init, content) =>
    print!("T_COMMENT_line(", init, "; ", "...)")
| T_COMMENT_rest
    (init, content) =>
    print!("T_COMMENT_rest(", init, "; ", "...)")
| T_COMMENT_cblock
    (level, content) =>
    print!("T_COMMENT_cblock(", level, "; ", "...)")
| T_COMMENT_mlblock
    (level, content) =>
    print!("T_COMMENT_mlblock(", level, "; ", "...)")
//
| T_AT() => print("AT")
//
| T_BAR() => print("BAR")
| T_CLN() => print("CLN")
| T_DOT() => print("DOT")
//
| T_EQ() => print("EQ")
//
| T_LT() => print("LT")
| T_GT() => print("GT")
//
| T_DLR() => print("DLR")
| T_SRP() => print("SRP")
//
| T_EQLT() => print("EQLT")
| T_EQGT() => print("EQGT")
//
| T_LTGT() => print("LTGT")
| T_GTLT() => print("GTLT")
//
| T_MSLT() => print("MSLT")
(*
| T_MSGT() => print("MSGT")
| T_MSLTGT() => print("MSLTGT")
*)
//
(*
| T_DOTLT() => print("DOTLT")
*)
| T_GTDOT() => print("GTDOT")
//
| T_COMMA() => print("COMMA")
| T_SMCLN() => print("SMCLN")
//
| T_BSLASH() => print("BSLASH")
//
| T_LPAREN() => print("LPAREN")
| T_RPAREN() => print("RPAREN")
| T_LBRACE() => print("LBRACE")
| T_RBRACE() => print("RBRACE")
//
| T_LBRACK() => print("LBRACK")
| T_RBRACK() => print("RBRACK")
//
| T_EXISTS(knd) =>
  print!("EXISTS(", knd, ")")
//
| T_TUPLE(knd) =>
  print!("TUPLE(", knd, ")")
| T_RECORD(knd) =>
  print!("RECORD(", knd, ")")
(*
| T_STRUCT() => print("STRUCT")
*)
//
| T_AS() => print("AS")
//
| T_OF() => print("OF")
//
| T_OP() => print("OP")
//
| T_OP_par() =>
  print("OP_par()")
| T_OP_sym(id) =>
  print!("OP_sym(", id, ")")
//
| T_IN() => print("IN")
//
| T_AND() => print("AND")
| T_END() => print("END")
//
| T_IF() => print("IF")
| T_SIF() => print("SIF")
| T_THEN() => print("THEN")
| T_ELSE() => print("ELSE")
//
| T_WHEN() => print("WHEN")
| T_WITH() => print("WITH")
//
| T_CASE(k0) =>
  print!("CASE(", k0, ")")
//
| T_SCASE() => print("SCASE()")
//
| T_ENDIF() => print("ENDIF")
| T_ENDSIF() => print("ENDSIF")
| T_ENDCASE() => print("ENDCASE")
| T_ENDSCASE() => print("ENDSCASE")
//
| T_LAM(knd) =>
  print!("LAM(", knd, ")")
| T_FIX(knd) =>
  print!("FIX(", knd, ")")
//
| T_LET() => print("LET")
| T_WHERE() => print("WHERE")
| T_LOCAL() => print("LOCAL")
//
| T_ENDLAM() => print("ENDLAM")
| T_ENDLET() => print("ENDLET")
| T_ENDWHERE() => print("ENDWHERE")
| T_ENDLOCAL() => print("ENDLOCAL")
//
| T_VAL(vlk) =>
  print!("VAL(", vlk, ")")
| T_VAR() => print!("VAR")
//
| T_FUN(fnk) =>
  print!("FUN(", fnk, ")")
//
| T_IMPLMNT(knd) =>
  print!("IMPLMNT(", knd, ")")
//
| T_ABSSORT() =>
  print!("ABSSORT(", ")")
//
| T_SORTDEF() =>
  print!("SORTDEF(", ")")
//
| T_SEXPDEF(srt) =>
  print!("SEXPDEF(", srt, ")")
//
| T_ABSTYPE(srt) =>
  print!("ABSTYPE(", srt, ")")
//
| T_ABSIMPL() =>
  print!("ABSIMPL")
| T_ABSOPEN() =>
  print!("ABSOPEN")
//
| T_DATASORT() =>
  print!("DATASORT")
| T_DATATYPE(srt) =>
  print!("DATATYPE(", srt, ")")
//
| T_WITHTYPE(srt) =>
  print!("WITHTYPE(", srt, ")")
//
| T_SRP_NONFIX() =>
  print!("#NONFIX")
| T_SRP_FIXITY(knd) =>
  print!("#FIXIXTY(", knd, ")")
//
| T_SRP_STACST() => print("#STACST")
//
| T_SRP_STATIC() => print("#STATIC")
| T_SRP_EXTERN() => print("#EXTERN")
//
| T_SRP_DEFINE() => print("#DEFINE")
| T_SRP_MACDEF() => print("#MACDEF")
//
| T_SRP_INCLUDE() => print("#INCLUDE")
//
| T_SRP_STALOAD() => print("#STALOAD")
| T_SRP_DYNLOAD() => print("#DYNLOAD")
//
| T_SRP_SYMLOAD() => print("#SYMLOAD")
//
) (* end of [fprint_tnode] *)
//
(* ****** ****** *)
//
(*
implement
print_token
  (tok) =
  fprint_token(stdout_ref, tok)
implement
prerr_token
  (tok) =
  fprint_token(stderr_ref, tok)
*)
//
implement
print_token(tok) =
(
  print(tok.node())
(*
  print!(tok.loc(), ": ", tok.node())
*)
)
//
(* ****** ****** *)
//
(*
implement
print2_tnode
  (tok) =
  fprint2_tnode(stdout_ref, tok)
implement
prerr2_tnode
  (tok) =
  fprint2_tnode(stderr_ref, tok)
*)
//
(* ****** ****** *)
//
implement
print2_tnode(tnd) =
(
case+ tnd of
//
| T_EOF() => ()
| T_ERR() =>
  print("*ERROR*")
//
| T_EOL() => print("\n")
//
| T_BLANK(x) => print(x)
//
| T_CLNLT(x) => print(x)
| T_DOTLT(x) => print(x)
//
| T_IDENT_alp(x) => print(x)
| T_IDENT_sym(x) => print(x)
//
| T_IDENT_srp(x) => print(x)
| T_IDENT_dlr(x) => print(x)
//
| T_IDENT_qual(x) => print(x)
//
| T_INT1(rep) => print(rep)
| T_INT2(base, rep) => print(rep)
| T_INT3(base, rep, _(*sfx*)) => print(rep)
//
| T_FLOAT1(rep) => print(rep)
| T_FLOAT2(base, rep) => print(rep)
| T_FLOAT3(base, rep, _(*sfx*)) => print(rep)
//
(*
| T_CHAR(chr) =>
  let
    val chr = int2char0(chr)
  in
    print!("CHAR(", chr, ")")
  end
*)
| T_CHAR_nil(rep) => print(rep)
| T_CHAR_char(rep) => print(rep)
| T_CHAR_slash(rep) => print(rep)
//
| T_STRING_closed(str) => print(str)
| T_STRING_unclsd(str) => print(str)
//
(*
| T_CDATA(cdata, asz) => print!("CDATA(...)")
*)
//
| T_SPECHAR(c) =>
  print(c) where{val c=char0_chr(* int2char0 *)(c)}
//
| T_COMMENT_line
    (init, content) =>
    print!(init, content)
| T_COMMENT_rest
    (init, content) =>
    print!(init, content)
| T_COMMENT_cblock
    (level, content) => print(content)
| T_COMMENT_mlblock
    (level, content) => print(content)
//
| T_AT() => print("@")
//
| T_BAR() => print("|")
| T_CLN() => print(":")
| T_DOT() => print(".")
//
| T_EQ() => print("=")
//
| T_LT() => print("<")
| T_GT() => print(">")
//
| T_DLR() => print("$")
| T_SRP() => print("#")
//
| T_EQLT() => print("=<")
| T_EQGT() => print("=>")
//
| T_LTGT() => print("<>")
| T_GTLT() => print("><")
//
| T_MSLT() => print("-<")
(*
| T_MSGT() => print("->")
| T_MSLTGT() => print("-<>")
*)
//
| T_GTDOT() => print(">.")
//
| T_COMMA() => print(",")
| T_SMCLN() => print(";")
//
| T_BSLASH() => print("\\")
//
| T_LPAREN() => print("(")
| T_RPAREN() => print(")")
| T_LBRACE() => print("{")
| T_RBRACE() => print("}")
//
| T_LBRACK() => print("[")
| T_RBRACK() => print("]")
//
| T_EXISTS(knd) =>
  print!("exists(", knd, ")")
//
| T_TUPLE(knd) =>
  print!("tuple(", knd, ")")
| T_RECORD(knd) =>
  print!("record(", knd, ")")
//
(*
| T_STRUCT(knd) =>
  print!("struct(", knd, ")")
*)
//
| T_AS() => print("as")
//
| T_OF() => print("of")
//
| T_OP() => print("op")
//
| T_OP_par() => print("op(")
| T_OP_sym(id) => print!("op", id)
//
| T_IN() => print("in")
//
| T_AND() => print("and")
| T_END() => print("end")
//
| T_IF() => print("if")
| T_SIF() => print("sif")
| T_THEN() => print("then")
| T_ELSE() => print("else")
//
| T_WHEN() => print("when")
| T_WITH() => print("with")
//
| T_CASE(k0) =>
  print!("case(", k0, ")")
//
| T_SCASE() => print("scase")
//
| T_ENDIF() => print("endif")
| T_ENDSIF() => print("endsif")
| T_ENDCASE() => print("endcase")
| T_ENDSCASE() => print("endscase")
//
| T_LAM(knd) =>
  print!("lam(", knd, ")")
| T_FIX(knd) =>
  print!("fix(", knd, ")")
//
| T_LET() => print("let")
| T_WHERE() => print("where")
| T_LOCAL() => print("local")
//
| T_ENDLAM() => print("endlam")
| T_ENDLET() => print("endlet")
| T_ENDWHERE() => print("endwhere")
| T_ENDLOCAL() => print("endlocal")
//
| T_VAL(vlk) =>
  print!("VAL(", vlk, ")")
| T_VAR() => print!("var")
//
| T_FUN(fnk) =>
  print!("FUN(", fnk, ")")
//
| T_IMPLMNT(knd) =>
  print!("implmnt(", knd, ")")
//
| T_ABSSORT() =>
  print!("abssort(", ")")
//
| T_SORTDEF() =>
  print!("sortdef(", ")")
//
| T_SEXPDEF(knd) =>
  print!("sexpdef(", knd, ")")
//
| T_ABSTYPE(knd) =>
  print!("abstype(", knd, ")")
//
| T_ABSIMPL() =>
  print!("absimpl")
| T_ABSOPEN() =>
  print!("absopen")
//
| T_DATASORT() =>
  print!("datasort")
| T_DATATYPE(knd) =>
  print!("datatype(", knd, ")")
//
| T_WITHTYPE(knd) =>
  print!("withtype(", knd, ")")
//
| T_SRP_NONFIX() =>
  print!("#nonfix")
| T_SRP_FIXITY(knd) =>
  print!("#fixity(", knd, ")")
//
| T_SRP_STACST() => print!("#stacst")
//
| T_SRP_STATIC() => print!("#static")
| T_SRP_EXTERN() => print!("#extern")
//
| T_SRP_DEFINE() => print("#define")
| T_SRP_MACDEF() => print("#macdef")
//
| T_SRP_INCLUDE() => print("#include")
//
| T_SRP_STALOAD() => print("#staload")
| T_SRP_DYNLOAD() => print("#dynload")
//
| T_SRP_SYMLOAD() => print("#symload")
//
) (* end of [fprint2_tnode] *)
//
(* ****** ****** *)

%{
ATSdynexn_dec(temptory_056___ArraySubscriptExn) ;
%}


local
//
#macdef c2i(x) = $UN.cast{size}(char0_ord(,(x)))
//
val
theAsz =
(* i2sz *)$UN.cast{size}(128)
val
theMap =
(* arrayref_make_elt<tnode> *)
arrszref_make_elt<tnode>(theAsz, T_EOF())
//
(*
val () = theMap[c2i('=')] := T_EQ()
*)
//
val () = theMap[c2i(',')] := T_COMMA()
val () = theMap[c2i(';')] := T_SMCLN()
//
val () = theMap[c2i('\(')] := T_LPAREN()
val () = theMap[c2i('\)')] := T_RPAREN()
//
val () = theMap[c2i('\{')] := T_LBRACE()
val () = theMap[c2i('\}')] := T_RBRACE()
//
val () = theMap[c2i('\[')] := T_LBRACK()
val () = theMap[c2i('\]')] := T_RBRACK()
//
val () = theMap[c2i('\\')] := T_BSLASH()
(*
#define c2i char2int1
//
val
theAsz =
i2sz(128)
val
theMap =
arrayref_make_elt<tnode>
  (theAsz, T_EOF())
//
(*
val () = theMap[c2i('=')] := T_EQ()
*)
//
val () = theMap[c2i(',')] := T_COMMA()
val () = theMap[c2i(';')] := T_SMCLN()
//
val () = theMap[c2i('\\')] := T_BSLASH()
//
val () = theMap[c2i('\(')] := T_LPAREN()
val () = theMap[c2i('\)')] := T_RPAREN()
//
val () = theMap[c2i('\{')] := T_LBRACE()
val () = theMap[c2i('\}')] := T_RBRACE()
//
val () = theMap[c2i('\[')] := T_LBRACK()
val () = theMap[c2i('\]')] := T_RBRACK()
*)
//
in (* in-of-local *)

implement
char2tnode(i0) = let
//
typedef AszLt = [ n:int | 0 < n && n < 128 ] int(n)
//
in
//
if
(i0 < 128)
then theMap[$UN.cast{AszLt}(i0)] else T_EOF()
//
end // end of [char2tnode]

end // end of [local]

(* ****** ****** *)
//
implement
tnode_is_AND
  (node) =
(
  case+ node of
  | T_AND() => true | _ => false
)
implement
tnode_is_BAR
  (node) =
(
  case+ node of
  | T_BAR() => true | _ => false
)
//
implement
tnode_is_CLN
  (node) =
(
  case+ node of
  | T_CLN() => true | _ => false
)
//
implement
tnode_is_COMMA
  (node) =
(
  case+ node of
  | T_COMMA() => true | _ => false
)
//
implement
tnode_is_SMCLN
  (node) =
(
  case+ node of
  | T_SMCLN() => true | _ => false
)
//
implement
tnode_is_BARSMCLN
  (node) =
(
  case+ node of
  | T_BAR() => true
  | T_SMCLN() => true | _ => false
)
//
(* ****** ****** *)
//
implement
tnode_is_blank
  (node) =
(
case+ node of
| T_EOL _ => true
| T_BLANK _ => true
| _ (* non-T_BLANK_... *) => false
)
implement
tnode_is_comment
  (node) =
(
case+ node of
| T_COMMENT_line _ => true
| T_COMMENT_rest _ => true
| T_COMMENT_cblock _ => true
| T_COMMENT_mlblock _ => true
| _ (* non-T_COMMENT_... *) => false
)
//
implement
tnode_is_skipped
  (node) =
(
if
tnode_is_blank(node)
then true else tnode_is_comment(node)
)
//
(* ****** ****** *)

(*
local
//
#staload
"libats/SATS/hashtbl_linprb.sats"
//
#staload
_(*anon*) = "libats/DATS/hashfun.dats"
#staload
_(*anon*) = "libats/DATS/hashtbl_linprb.dats"
//
typedef key = string and itm = tnode
vtypedef hashtbl = hashtbl(key, itm)
*)

#staload
"libats/temp/SATS/hashmap_chain.sats"

#staload
"libats/temp/SATS/hashmap_chain.sats"

(* ****** ****** *)

local

#staload _ =
"libats/temp/DATS/hashfun.dats"
#staload _ =
"libats/temp/DATS/linmap_list.dats"
#staload _ =
"libats/temp/DATS/hashmap_chain.dats"

//
(*
#staload
"libats/SATS/hashtbl_linprb.sats"
//
#staload
_(*anon*) = "libats/DATS/hashfun.dats"
#staload
_(*anon*) = "libats/DATS/hashtbl_linprb.dats"
*)
//
typedef key = string and itm = tnode
vtypedef hashtbl = hashmap(key, itm)

//
val
theCap = 229
val
theHashtbl =
hashmap_make_hcap<key,itm>(i2sz(theCap))
val
theHashtbl = $UN.castvwtp0{ptr}(theHashtbl)
//
in (* in-of-local *)

implement
tnode_search(name) = let
//
var res: itm?
//
val tbl =
$UN.castvwtp0{hashtbl}(theHashtbl)
val ans =
hashmap_search<key,itm>(tbl, name, res)
prval ((*void*)) = $UN.cast2void(tbl)
//
in
  if (ans)
  then opt_unsome_get(res)
  else let
    prval () = opt_unnone(res) in T_EOF()
  end // end of [else]
end // end of [tnode_search]

(* ****** ****** *)

implement
tnode_insert
(name, node) = let
//
var res: itm?
val tbl =
$UN.castvwtp0{hashtbl}(theHashtbl)
val ans =
hashmap_insert<key,itm>(tbl, name, node, res)
//
(* val ((*void*)) = assertloc(ans = false) *)
//
prval ((*void*)) = opt_clear(res)
prval ((*void*)) = $UN.cast2void(tbl)
//
in
  // nothing
end // end of [tnode_insert]

end // end of [local]

(* ****** ****** *)

(*
extern val T_DOT : tnode
extern val T_QMARK : tnode
extern val T_PERCENT : tnode
*)
(*
implement T_DOT = T_IDENT_alp "."
implement T_QMARK = T_IDENT_alp "?"
implement T_PERCENT = T_IDENT_alp "%"
*)

(* ****** ****** *)

(*
val () = tnode_insert("@", T_AT)
val () = tnode_insert("!", T_BANG)
*)

(* ****** ****** *)
//
implement
lexing_locatize_node
  (pos0, node) = let
//
#define
locmake
location_make_pos_pos
//
#define
posinc1 position_incby_1
#define
posinceol position_incby_eol
#define
posincneol position_incby_neol
#define
posinctext position_incby_text
//
fun
trans_tnode
(node0: tnode): tnode =
(
case+ node0 of
| T_SPECHAR(c) => let
    val
    node1 = char2tnode(c)
  in
    case+ node1 of
    | T_EOF() => node0 | _(*else*) => node1
  end // end of [T_SPECHAR]
//
| T_IDENT_alp(id) => let
    val
    node1 = tnode_search(id)
  in
    case+ node1 of
    | T_EOF() => node0 | _(*else*) => node1
  end // end of [T_IDENT_alp]
| T_IDENT_sym(id) => let
    val
    node1 = tnode_search(id)
  in
    case+ node1 of
    | T_EOF() => node0 | _(*else*) => node1
  end // end of [T_IDENT_sym]
//
| T_IDENT_dlr(id) => let
    val
    node1 = tnode_search(id)
  in
    case+ node1 of
    | T_EOF() => node0 | _(*else*) => node1
  end // end of [T_IDENT_dlr]
| T_IDENT_srp(id) => let
    val
    node1 = tnode_search(id)
  in
    case+ node1 of
    | T_EOF() => node0 | _(*else*) => node1
  end // end of [T_IDENT_srp]
//
| _ (* rest-of-tnode *) => node0
//
)
//
var pos1: pos_t
val ((*void*)) =
position_copyfrom(pos1, pos0)
//
in
//
(
case+ node of
//
| T_EOF() => ()
| T_ERR() => ()
//
| T_EOL() => posinceol(pos1)
//
| T_BLANK(bs) => posincneol(pos1, bs)
//
| T_CLNLT(cs) => posincneol(pos1, cs)
| T_DOTLT(cs) => posincneol(pos1, cs)
//
| T_SPECHAR(c0) => posinc1(pos1)
//
| T_IDENT_alp(id) => posincneol(pos1, id)
| T_IDENT_sym(id) => posincneol(pos1, id)
//
| T_IDENT_dlr(id) => posincneol(pos1, id)
| T_IDENT_srp(id) => posincneol(pos1, id)
//
| T_IDENT_qual(id) => posincneol(pos1, id)
//
| T_INT1(rep) => posincneol(pos1, rep)
| T_INT2(_, rep) => posincneol(pos1, rep)
| T_INT3(_, rep, _) => posincneol(pos1, rep)
//
| T_FLOAT1(rep) => posincneol(pos1, rep)
| T_FLOAT2(_, rep) => posincneol(pos1, rep)
| T_FLOAT3(_, rep, _) => posincneol(pos1, rep)
//
| T_CHAR_nil(rep) => posincneol(pos1, rep)
| T_CHAR_char(rep) => posincneol(pos1, rep)
| T_CHAR_slash(rep) => posincneol(pos1, rep)
//
| T_STRING_closed(rep) => posinctext(pos1, rep)
| T_STRING_unclsd(rep) => posinctext(pos1, rep)
//
| T_COMMENT_line
    (init, content) =>
  (
    posincneol(pos1, init); // initiative
    posincneol(pos1, content) // comment body
  )
| T_COMMENT_rest
    (init, content) =>
  (
    posincneol(pos1, init); // initiative
    posinctext(pos1, content) // comment body
  )
| T_COMMENT_cblock
    (level, content) => posinctext(pos1, content)
| T_COMMENT_mlblock
    (level, content) => posinctext(pos1, content)
//
| _ (* else *) => ()
//
) ; (* end of [case] *)
(
let
  val
  node = trans_tnode(node)
  val
  loc01 = locmake(pos0, pos1)
in
  position_copyfrom
  (
    pos0, pos1
  ) ; token_make_node(loc01, node)
end // end of [let]
)
//
end // end of [lexing_locatize_node]

(* ****** ****** *)

extern castfn ofg0{a:tflt}(list0_vt(a)) : [n:int | n >= 0] list1_vt(a, n)

implement
lexing_locatize_nodelst
  (pos, nodes) = let
//
fun
loop
( pos: &pos_t >> _
, nodes: tnodelst
, tokens: tokenlst_vt): tokenlst_vt =
(
case+ nodes of
| list1_nil() =>
  ofg0(list0_vt_reverse(g0ofg1 tokens))
| list1_cons(node, nodes) => let
    val token =
    lexing_locatize_node(pos, node)
  in
    loop(pos, nodes, list1_vt_cons(token, tokens))
  end // end of [list_cons]
)
//
in
  loop(pos, nodes, list1_vt_nil(*void*))
end // end of [lexing_locatize_tnodelst]

(* ****** ****** *)

implement
lexing_preprocess_tokenlst
  (toks) = let
//
fnx
loop0
( xs0: tokenlst_vt
, res: tokenlst_vt): tokenlst_vt =
(
case+ xs0 of
| ~list1_vt_nil() => res
| ~list1_vt_cons(x0, xs1) =>
   (loop1(x0, xs1, res))
)
and
loop1
( x0: token
, xs1: tokenlst_vt
, res: tokenlst_vt): tokenlst_vt =
(
case+ xs1 of
| ~list1_vt_nil() =>
   list1_vt_cons(x0, res)
| ~list1_vt_cons(x1, xs2) =>
   (loop2(x0, x1, xs2, res))
)
and
loop2
( x0: token
, x1: token
, xs2: tokenlst_vt
, res: tokenlst_vt): tokenlst_vt =
(
case+ x0.node() of
//
| T_EOL() =>
  loop1(x1, xs2, res)
| T_BLANK _ =>
  loop1(x1, xs2, res)
//
| T_COMMENT_line _ =>
  loop1(x1, xs2, res)
| T_COMMENT_rest _ =>
  loop1(x1, xs2, res)
| T_COMMENT_cblock _ =>
  loop1(x1, xs2, res)
| T_COMMENT_mlblock _ =>
  loop1(x1, xs2, res)
//
| T_AT() =>
  (
    case+ x1.node() of
    | T_LPAREN() => let
        val loc = x0.loc()+x1.loc()
        val x01 =
        token_make_node(loc, T_TUPLE(0))
      in
        loop0(xs2, list1_vt_cons(x01, res))
      end // end of ["("]
    | T_LBRACE() => let
        val loc = x0.loc()+x1.loc()
        val x01 =
        token_make_node(loc, T_RECORD(0))
      in
        loop0(xs2, list1_vt_cons(x01, res))
      end // end of ["{"]
//
    | _ (* rest-of-tnode *) =>
      (
        loop1(x1, xs2, list1_vt_cons(x0, res))
      ) (* end of [rest-of-tnode] *)
//
  )
//
| T_OP() =>
  (
    case+ x1.node() of
    | T_LPAREN() => let
        val loc = x0.loc()+x1.loc()
        val x01 =
        token_make_node(loc, T_OP_par())
      in
        loop0(xs2, list1_vt_cons(x01, res))
      end // end of [T_LPAREN]
    | T_IDENT_sym(id) => let
        val loc = x0.loc()+x1.loc()
        val x01 =
        token_make_node(loc, T_OP_sym(id))
      in
        loop0(xs2, list1_vt_cons(x01, res))
      end // end of [T_IDENT_sym]
    | _ (* rest-of-tnode *) =>
      (
        loop1(x1, xs2, list1_vt_cons(x0, res))
      ) (* end of [rest-of-tnode] *)
  )
//
| T_DLR() =>
  (
    case+ x1.node() of
    | T_LPAREN() => let
        val loc = x0.loc()+x1.loc()
        val x01 =
        token_make_node(loc, T_TUPLE(1))
      in
        loop0(xs2, list1_vt_cons(x01, res))
      end // end of ["("]
    | T_LBRACE() => let
        val loc = x0.loc()+x1.loc()
        val x01 =
        token_make_node(loc, T_RECORD(1))
      in
        loop0(xs2, list1_vt_cons(x01, res))
      end // end of ["{"]
//
    | _ (* rest-of-tnode *) =>
      (
        loop1(x1, xs2, list1_vt_cons(x0, res))
      ) (* end of [rest-of-tnode] *)
//
  )
| T_SRP() =>
  (
    case+ x1.node() of
    | T_LBRACK() => let
        val loc = x0.loc()+x1.loc()
        val x01 =
        token_make_node(loc, T_EXISTS(1))
      in
        loop0(xs2, list1_vt_cons(x01, res))
      end // end of ["("]
    | _ (* rest-of-tnode *) =>
        loop1(x1, xs2, list1_vt_cons(x0, res))
  )
//
| T_LAM(k0) =>
  (
    case+ x1.node() of
    | T_AT() => let
        val loc = x0.loc()+x1.loc()
        val x01 =
        token_make_node(loc, T_LAM(k0+1))
      in
        loop0(xs2, list1_vt_cons(x01, res))
      end // end of [T_AT]
    | _ (* rest-of-tnode *) =>
        loop1(x1, xs2, list1_vt_cons(x0, res))
      // end of [rest-of-tnode]
  )
| T_FIX(k0) =>
  (
    case+ x1.node() of
    | T_AT() => let
        val loc = x0.loc()+x1.loc()
        val x01 =
        token_make_node(loc, T_FIX(k0+1))
      in
        loop0(xs2, list1_vt_cons(x01, res))
      end // end of [T_AT]
    | _ (* rest-of-tnode *) =>
        loop1(x1, xs2, list1_vt_cons(x0, res))
      // end of [rest-of-tnode]
  )
//
| T_VAL(VLKval) =>
  (
    case+ x1.node() of
    | T_IDENT_sym("+") => let
        val loc = x0.loc()+x1.loc()
        val x01 =
        token_make_node(loc, T_VAL(VLKvalp))
      in
        loop0(xs2, list1_vt_cons(x01, res))
      end // end of [T_IDENT_sym(+)]
    | T_IDENT_sym("-") => let
        val loc = x0.loc()+x1.loc()
        val x01 =
        token_make_node(loc, T_VAL(VLKvaln))
      in
        loop0(xs2, list1_vt_cons(x01, res))
      end // end of [T_IDENT_sym(-)]
    | _ (* rest-of-tnode *) =>
        loop1(x1, xs2, list1_vt_cons(x0, res))
      // end of [rest-of-tnode]
  )
//
| T_CASE _ =>
  (
    case+ x1.node() of
    | T_IDENT_sym("+") => let
        val loc = x0.loc()+x1.loc()
        val x01 =
        token_make_node(loc, T_CASE(1))
      in
        loop0(xs2, list1_vt_cons(x01, res))
      end // end of [T_IDENT_sym(+)]
    | T_IDENT_sym("-") => let
        val loc = x0.loc()+x1.loc()
        val x01 =
        token_make_node(loc, T_CASE(~1))
      in
        loop0(xs2, list1_vt_cons(x01, res))
      end // end of [T_IDENT_sym(-)]
    | _ (* rest-of-tnode *) =>
        loop1(x1, xs2, list1_vt_cons(x0, res))
      // end of [rest-of-tnode]
  )
//
(*
| T_LTGT((*void*)) => let
    val loc = x0.loc()
    val x00 =
    token_make_node(loc, T_LT())
    val x01 =
    token_make_node(loc, T_GT())
  in
    loop1
    ( x1, xs2
    , list_vt_cons(x01, list_vt_cons(x00, res)))
  end // end of [T_GTLT]
*)
| T_GTLT((*void*)) => let
    val loc = x0.loc()
    val x00 =
    token_make_node(loc, T_GT())
    val x01 =
    token_make_node(loc, T_LT())
  in
    loop1
    ( x1, xs2
    , list1_vt_cons(x01, list1_vt_cons(x00, res)))
  end // end of [T_GTLT]
//
(*
| T_DOTLTGTDOT() => let
    val loc = x0.loc()
    val x00 =
    token_make_node(loc, T_DOTLT())
    val x01 =
    token_make_node(loc, T_GTDOT())
  in
    loop1
    ( x1, xs2
    , list_vt_cons(x01, list_vt_cons(x00, res)))
  end // end of [T_GTLT]
*)
//
(*
| T_IDENT_dlr(id) =>
  (
    case+ x1.node() of
    | T_DOT() => let
        val loc =
        x0.loc()+x1.loc()
        val x01 =
        token_make_node
        (loc, T_IDENT_qual(id+"."))
      in
        loop0(xs2, list_vt_cons(x01, res))
      end // end of [T_DOT]
    | _ (* rest-of-tnode *) =>
        loop1(x1, xs2, list_vt_cons(x0, res))
  ) (* end of [T_IDENT_dlr] *)
*)
//
| _ (* rest-of-tnode *) =>
  (
    loop1(x1, xs2, list1_vt_cons(x0, res))
  ) (* end of [rest-of-tnode] *)
//
)
//
in
//
ofg0(list0_vt_reverse<token>(g0ofg1(loop0(toks, list1_vt_nil()))))
//
end // end of [lexing_preprocess_tokenlst]

(* ****** ****** *)

(* end of [xats_lexing_token.dats] *)
