implement
print2_tnode(tnd) =
(
case+ tnd of
//
| T_EOF() => ()
| T_ERR() =>
  print("*ERROR*")
//
| T_EOL() => println!("\n")
//
| T_BLANK(x) => print(x)
//
//| T_CLNLT(x) => print(x)
| T_CLNLT() => print(":<")
| T_CLNLTGT() => print(":<>")
| T_CLNLTBNG() => print(":<!")
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
| T_EXISTS(knd) => print("[")
  //print!("exists(", knd, ")")
//
| T_TUPLE(knd) => print(str) where
  {
    val str =
    (
      case+ knd of
      | 0 => "@("
      | 1 => "$("
      | 2 => "@tup"
      | 3 => "$tup"
      | _ => "@("
    ): string
  }

  (* print!("tuple(", knd, ")") *)
| T_RECORD(knd) => print(str) where
  {
    val str =
    (
      case+ knd of
      | 0 => "@{"
      | 1 => "${"
      | 2 => "@rec"
      | 3 => "$rec"
      | _ => "@("
    ): string
  }
  (* print!("record(", knd, ")") *)
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
| T_CASE(k0) => print(str) where
  {
    val str =
    (
      case+ k0 of
      | 0 => "case"
      | 1  => "case+"
      | _  => "case-"
    ): string
  }
  (* print!("case(", k0, ")") *)
//
| T_SCASE() => print("scase")
//
| T_ENDIF() => print("endif")
| T_ENDSIF() => print("endsif")
| T_ENDCASE() => print("endcase")
| T_ENDSCASE() => print("endscase")
//
| T_LAM(knd) => print(str) where
  {
    val str =
    (
      case+ knd of
      | 0 => "lam"
      | _ => "lam@"
    ): string
  }
  (* print!("lam(", knd, ")") *)
| T_FIX(knd) => print(str) where
  {
    val str =
    (
      case+ knd of
      | 0 => "fix"
      | _ => "fix@"
    ): string
  }
  (* print!("fix(", knd, ")") *)
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
| T_VAL(vlk) => show(vlk)
  (* print!("VAL(", vlk, ")") *)
| T_VAR() => print!("var")
//
| T_FUN(fnk) => show(fnk)
  (* print!("FUN(", fnk, ")") *)
//
| T_IMPLMNT(knd) => show(knd)
  (* print!("implmnt(", knd, ")") *)
//
| T_ABSSORT() =>
  print!("abssort")
//
| T_SORTDEF() =>
  print!("sortdef")
//
| T_SEXPDEF(knd) => print!(sdef) where
  {
  (* val () = println!(knd) *)
  val sdef = (
    case- knd of
    | PROPSORT => "propdef"
    | VIEWSORT => "viewdef"
    | TYPESORT => "typedef"
    | VTYPESORT => "vtypedef"
  ) : string
  }
  (* print!("sexpdef(", knd, ")") *)

//
| T_ABSTYPE(knd) => print!(abst) where
  {
  (* val () = println!(knd) *)
  val abst = (
    case- knd of
    | TFLTSORT => "abstflt" // 0
    | TBOXSORT => "abstbox" // 1
    | VTFLTSORT => "absvtflt" // 2
    | VTBOXSORT => "absvtbox" // 3
    | PROPSORT => "absprop" // 4
    | VIEWSORT => "absview" // 6
    (* | _ => "ERRRRRRRROR" *)

  ) : string
  }
  (* print!("abstype(", knd, ")") *)
//
| T_ABSIMPL() =>
  print!("absimpl")
| T_ABSOPEN() =>
  print!("absopen")
//
| T_DATASORT() =>
  print!("datasort")
| T_DATATYPE(knd) => print(str) where
  {
  val str = (
    case- knd of
    | TYPESORT => "datatype" // 0
    | VTYPESORT => "datavtype" // 2
    | PROPSORT => "dataprop" // 4
    | VIEWSORT => "dataview" // 6
  ) : string
  }
  (* print!("datatype(", knd, ")") *)
//
| T_WITHTYPE(knd) => print(str) where
  {
  val str = (
    case- knd of
    | TYPESORT => "withtype" // 0
    | VTYPESORT => "withvtype" // 2
    | PROPSORT => "withprop" // 4
    | VIEWSORT => "withview" // 6
  ) : string
  }
  (* print!("withtype(", knd, ")") *)
//
| T_SRP_NONFIX() =>
  print!("#nonfix")
| T_SRP_FIXITY(knd) => print(str) where
  {
  val str = (
    case- knd of
    | INFIX => "infix"
    | INFIXL => "infixl"
    | INFIXR => "infixr"
    | PREFIX => "prefix"
    | POSTFIX => "postfix"
  ) : string
  }
  (* print!("#fixity(", knd, ")") *)
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
