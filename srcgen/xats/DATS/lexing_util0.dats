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

(*
#staload "libats/SATS/gint.sats"
#staload _ = "libats/DATS/gint.dats"

#staload "libats/SATS/char.sats"
#staload _ = "libats/DATS/char.dats"

#staload "libats/SATS/gptr.sats"
#staload _ = "libats/DATS/gptr.dats"

#staload "libats/SATS/list.sats"
#staload _ = "libats/DATS/list.dats"

#staload "libats/SATS/list_vt.sats"
#staload _ = "libats/DATS/list_vt.dats"

#staload "libats/SATS/filebas.sats"
#staload _ = "libats/DATS/filebas.dats"
*)
#include "share/HATS/temptory_staload_bucs320.hats"


#staload
UN = "libats/SATS/unsafe.sats"

(* ****** ****** *)

extern
castfn ofg0{a:tflt}
(list0_vt(a)) : [n:int | n >= 0] list1_vt(a, n)


(* ****** ****** *)

#staload LOC = "./../SATS/location.sats"
#staload _ = "./location.dats"

#staload "./../SATS/lexbuf.sats"
#staload _ = "./lexbuf.dats"
#staload "./../SATS/lexing.sats"

(* ****** ****** *)

#define OCT 8
#define DEC 10
#define HEX 16

(* ****** ****** *)
//
extern
fun
isEOL(c: char): bool
//
extern
fun
isBLANK(c: char): bool
//
extern
fun
isDOT(c: char): bool
extern
fun
isCOLON(c: char): bool
//
extern
fun
isALNUM(c: char): bool
extern
fun
isDIGIT(c: char): bool
extern
fun
isXDIGIT(c: char): bool
//
extern
fun
isALNUM_(c: char): bool
//
extern
fun
isIDENTFST(c: char): bool
extern
fun
isIDENTRST(c: char): bool
extern
fun
isSYMBOLIC(c: char): bool
//
extern
fun isSLASH(c: char): bool
extern
fun isSLASH4(cs: string): bool
//
extern
fun isSHARP(c: char): bool
extern
fun isDOLLAR(c: char): bool
//
extern
fun isASTRSK(c: char): bool
//
extern
fun isLPAREN(c: char): bool
extern
fun isRPAREN(c: char): bool
//
extern
fun isSQUOTE(c: char): bool
extern
fun isDQUOTE(c: char): bool
//
(* ****** ****** *)
//
implement
isEOL(c) = (c = '\n')
//
implement
isBLANK(c) =
  if (c = ' ')
  then true
  else (if (c = '\t') then true else false)
//
(* ****** ****** *)
//
implement
isDOT(c) = (c = '.')
implement
isCOLON(c) = (c = ':')
//
(* ****** ****** *)
//
implement
isALNUM(c) = char0_isalnum(c)
implement
isDIGIT(c) = char0_isdigit(c)
implement
isXDIGIT(c) = char0_isxdigit(c)
//
(* ****** ****** *)
//
implement
isALNUM_(c) = (char0_isalnum(c) || (c = '_'))
//
(* ****** ****** *)

implement
isIDENTFST(c) =
(
  ifcase
  | char0_isalpha(c) => true
  | ( c = '_' ) => true
  | _ (*rest-of-char*) => false
) (* end of [isIDENTFST] *)

implement
isIDENTRST(c) =
(
  ifcase
  | char0_isalnum(c) => true
  | ( c = '_' ) => true
  | ( c = '$' ) => true
  | ( c = '\'' ) => true // HX: ML tradition
  | _ (*rest-of-char*) => false
) (* end of [isIDENTRST] *)

(* ****** ****** *)

implement
isSYMBOLIC(c) = let
//
val
SYMBOLIC = "%&+-./:=@~`^|*!?<>#"
//
(*
//
val
SYMBOLIC = "%&+-./:=@~`^|*!?<>#$"
//
*)
in
  $extfcall(ptr, "strchr", SYMBOLIC, c) > the_null_ptr
end // end of [SYMBOLIC_test]

(* ****** ****** *)

implement
isSLASH(c) = (c = '/')
implement
isSLASH4(cs) = (0 = $extfcall(int, "strncmp", cs, "////", 4))

(* ****** ****** *)

implement
isSHARP(c) = ( c = '#' )
implement
isDOLLAR(c) = ( c = '$' )

(* ****** ****** *)

implement
isASTRSK(c) = ( c = '*' )

(* ****** ****** *)

implement
isLPAREN(c) = ( c = '\(' )
implement
isRPAREN(c) = ( c = '\)' )

(* ****** ****** *)

implement
isSQUOTE(c) = ( c = '\'' )
implement
isDQUOTE(c) = ( c = '\"' )

(* ****** ****** *)
//
extern
fun
testing_digits(buf: &lexbuf >> _): int
and
testing_xdigits(buf: &lexbuf >> _): int
//
extern
fun
testing_sign_digits(buf: &lexbuf >> _): int
//
(* ****** ****** *)

implement
testing_digits(buf) = loop(buf, 0) where
{

  fun
  loop(buf: &lexbuf >> _, k: int): int = let

    val i0 = lexbuf_getc(buf)
    val c0 = char0_chr(i0)

  in

    if isDIGIT(c0)
    then (loop(buf, k+1))
    else k where
    {
      val () = lexbuf_unget(buf, i0)
    }

  end

}

implement
testing_sign_digits(buf) = let

  val i0 = lexbuf_getc(buf)
  val c0 = char0_chr(i0)

in

  ifcase
  | c0 = '+' => 1+testing_digits(buf)
  | c0 = '-' => 1+testing_digits(buf)
  | isDIGIT(c0) => 1+testing_digits(buf)
  | _(* else *) => 0 where
    {
      val () = lexbuf_unget(buf, i0)
    }

end

(* ****** ****** *)

implement
testing_xdigits(buf) = loop(buf, 0) where
{

  fun
  loop(buf: &lexbuf >> _, k: int): int = let

    val i0 = lexbuf_getc(buf)
    val c0 = char0_chr(i0)

  in

    if isXDIGIT(c0)
    then (loop(buf, k+1)) // end of [then]
    else k where
    {
      val () = lexbuf_unget(buf, i0)
    }

  end

}
(* end of [testing_xdigits] *)

(*
let
  val () = lexbuf_unget(buf, i0) in k
end // end of [else]
*)


(* ****** ****** *)
//
extern
fun
testing_floatsfx(buf: &lexbuf >> _, i0: int): int

extern
fun
testing_floatsfx_hex(buf: &lexbuf >> _, i0: int): int

(* ****** ****** *)

implement
testing_floatsfx(buf, i0) = let

  val c0 = char0_chr(i0)

in

  ifcase
  | c0 = '.' => loop0(buf, 1)
  | c0 = 'e' => loop1(buf, i0, 0)
  | c0 = 'E' => loop1(buf, i0, 0)
  | _(* else *) => 0 where
    {
      val () = lexbuf_unget(buf, i0)
    }

end where
{

  fun
  loop0(buf: &lexbuf >> _, k: int): int = let

    val i0 = lexbuf_getc(buf)
    val c0 = char0_chr(i0)

  in

    if isDIGIT(c0)
    then loop0(buf, k+1)
    else loop1(buf, i0, k)

  end // end of [loop0]

  and
  loop1(buf: &lexbuf >> _, i0: int, k: int): int = let

    val c0 = char0_chr(i0)

  in

    ifcase
    | c0 = 'e' => (k+1+testing_sign_digits(buf))
    | c0 = 'E' => (k+1+testing_sign_digits(buf))
    | _(* else *) => k where
      {
        // exponent-less
        val () = lexbuf_unget(buf, i0)
      }

  end

}

implement
testing_floatsfx_hex(buf, i0) = let

  val c0 = char0_chr(i0)

in

  ifcase
  | c0 = '.' => loop0(buf, 1)
  | c0 = 'p' => loop1(buf, i0, 0)
  | c0 = 'P' => loop1(buf, i0, 0)
  | _(* else *) => 0 where
    {
      val () = lexbuf_unget(buf, i0)
    }

end where
{

  fun
  loop0(buf: &lexbuf >> _, k: int): int = let

    val i0 = lexbuf_getc(buf)
    val c0 = char0_chr(i0)

  in

    if isXDIGIT(c0)
    then loop0(buf, k+1)
    else loop1(buf, i0, k)

  end

  and
  loop1(buf: &lexbuf >> _, i0: int, k: int): int = let

    val c0 = char0_chr(i0)

  in

    ifcase
    | c0 = 'p' => (k+1+testing_sign_digits(buf))
    | c0 = 'P' => (k+1+testing_sign_digits(buf))
    | _(* else *) => k where // exponent-less
      {
        val () = lexbuf_unget(buf, i0)
      }

  end

} (* testing_floatsfx_hex *)

(* ****** ****** *)

extern
fun
lexing_isIDENTFST(buf: &lexbuf >> _, i0: int) : tnode
extern
fun
lexing_isSYMBOLIC(buf: &lexbuf >> _, i0: int) : tnode

(* ****** ****** *)

extern
fun
lexing_COMMENT_line(buf: &lexbuf >> _, sym: string) : tnode
extern
fun
lexing_COMMENT_rest(buf: &lexbuf >> _, sym: string) : tnode

(* ****** ****** *)

extern
fun
lexing_COMMENT_cblock(buf: &lexbuf >> _, i0: int, i1: int) : tnode
extern
fun
lexing_COMMENT_mlblock(buf: &lexbuf >> _, i0: int, i1: int) : tnode

(* ****** ****** *)

local

(* ****** ****** *)

fun
lexing_isEOL(buf: &lexbuf >> _, i0: int) : tnode = T_EOL() where // HX: newline token
{
  val () = lexbuf_get_none(buf)
}
(* end of [lexing_isEOL] *)

(* ****** ****** *)

fun
lexing_isBLANK(buf: &lexbuf >> _, i0: int) : tnode = loop(buf) where
{

  fun
  loop(buf: &lexbuf >> _): tnode = let

    val i0 = lexbuf_getc(buf)
    val c0 = char0_chr(i0)

  in

    if isBLANK(c0)
    then loop(buf)
    else T_BLANK(lexbuf_get_fullseg(buf)) where
    {
      val () = lexbuf_unget(buf, i0)
    }

  end

} (* end of [lexing_isBLANK] *)

(* ****** ****** *)

fun
lexing_isSLASH(buf: &lexbuf >> _, i0: int) : tnode = let

  val i1 = lexbuf_getc(buf)
  val c1 = char0_chr(i1)

in

  ifcase
  | c1 = '*' => lexing_COMMENT_cblock(buf, i0, i1)
  | c1 = '/' => let
      val-T_IDENT_sym(sym) = lexing_isSYMBOLIC(buf, i0)
    in

      if isSLASH4(sym)
      then lexing_COMMENT_rest(buf, sym)
      else lexing_COMMENT_line(buf, sym)

    end
  | _(* else *) => lexing_isSYMBOLIC(buf, i0) where
    {
      val () = lexbuf_unget(buf, i1)
    } (* end of [......] *)

end (* end of [lexing_isSLASH] *)

(* ****** ****** *)

fun
lexing_isDIGIT
(buf: &lexbuf >> _, i0: int) : tnode =
(

  ifcase
  | c0 = '0' => loop0(buf)
  | _(* else *) => loop1(buf)

) where
{

  fun
  loop0(buf: &lexbuf >> _): tnode = let

    val i0 = lexbuf_getc(buf)
    val c0 = char0_chr(i0)

  in

    ifcase
    | c0 = 'x' => loop0x(buf)
    | c0 = 'X' => loop0X(buf)
    | _(* else *) =>
    (
      if isDIGIT(c0)
      then loop0d(buf)
      else T_INT1(lexbuf_get_fullseg(buf)) where
      {
        val () = lexbuf_unget(buf, i0)
      }
    )

  end

  and
  loop0d(buf: &lexbuf >> _): tnode = let

    val i0 = lexbuf_getc(buf)
    val c0 = char0_chr(i0)

  in

    if isDIGIT(c0)
    then loop0d(buf)
    else let
      val k0 = testing_floatsfx(buf, i0)
    in

      if (k0 = 0)
      then T_INT2(OCT, lexbuf_get_fullseg(buf))
      else T_FLOAT2(DEC, lexbuf_get_fullseg(buf))

    end

  end // end of [loop0d]

  and
  loop0x(buf: &lexbuf >> _): tnode = let

    val i0 = lexbuf_getc(buf)
    val c0 = char0_chr(i0)

  in

    if isXDIGIT(c0)
    then loop0x(buf)
    else let
      val k0 = testing_floatsfx_hex(buf, i0)
    in

      if (k0 = 0)
      then T_INT2(HEX, lexbuf_get_fullseg(buf))
      else T_FLOAT2(HEX, lexbuf_get_fullseg(buf))

    end // end of [else]

  end

  and
  loop0X(buf: &lexbuf >> _): tnode = loop0x(buf)

(* ****** ****** *)

fun
loop1(buf: &lexbuf >> _): tnode = let

  val i0 = lexbuf_getc(buf)
  val c0 = char0_chr(i0)

in

  if isDIGIT(c0)
  then loop1(buf)
  else let
    val k0 = testing_floatsfx(buf, i0)
  in
    if (k0 = 0)
    then T_INT1(lexbuf_get_fullseg(buf))
    else T_FLOAT1(lexbuf_get_fullseg(buf))
  end

end // end of [loop1]

(* ****** ****** *)

val c0 = char0_chr(i0)

(* ****** ****** *)

} (* end of [lexing_isDIGIT] *)

(* ****** ****** *)

implement
lexing_isIDENTFST(buf, i0) = loop(buf) where
{
//
  fun
  loop(buf: &lexbuf >> _): tnode = let

    val i0 = lexbuf_getc(buf)
    val c0 = char0_chr(i0)
    (*
    val () = println!("lexing_isIDENTFST: c0 = ", c0)
    *)

  in

    if isIDENTRST(c0)
    then loop(buf)
    else T_IDENT_alp(lexbuf_get_fullseg(buf)) where
    {
      val () = lexbuf_unget(buf, i0)
    }

  end

} (* end of [lexing_isIDENTFST] *)

(* ****** ****** *)

implement
lexing_isSYMBOLIC(buf, i0) = loop(buf) where
{

  fun
  loop(buf: &lexbuf >> _): tnode = let

    val i0 = lexbuf_getc(buf)
    val c0 = char0_chr(i0)

  in

    if isSYMBOLIC(c0)
    then loop(buf)
    else T_IDENT_sym(lexbuf_get_fullseg(buf)) where
    {
      val () = lexbuf_unget(buf, i0)
    }

  end

} (* end of [lexing_isSYMBOLIC] *)

(* ****** ****** *)

fun
lexing_isSHARP(buf: &lexbuf >> _, i0: int) : tnode = loop(buf, 0) where
{
//
  fun
  loop(buf: &lexbuf >> _, k: int): tnode = let

    val i1 = lexbuf_getc(buf)
    val c1 = char0_chr(i1)

  in

    if isALNUM_(c1)
    then loop(buf, k+1)
    else
    (
      if (k > 0)
      then T_IDENT_srp(lexbuf_get_fullseg(buf)) where
      {
        val () = lexbuf_unget(buf, i1)
      }
      else lexing_isSYMBOLIC(buf, i0) where
      {
        val () = lexbuf_unget(buf, i1)
      }
    )

end // end of [loop]
//
} (* end of [lexing_isSHARP] *)

(* ****** ****** *)

fun
lexing_isDOLLAR(buf: &lexbuf >> _, i0: int) : tnode = loop(buf, 0) where
{
//
  fun
  loop(buf: &lexbuf >> _, k: int): tnode = let

    val i1 = lexbuf_getc(buf)
    val c1 = char0_chr(i1)

  in

    if isALNUM_(c1)
    then loop(buf, k+1)
    else
    (
      if (k > 0)
      then
      (
        if (c1 = '.')
        then T_IDENT_qual(lexbuf_get_fullseg(buf))
        else T_IDENT_dlr(lexbuf_get_fullseg(buf)) where
        {
          val () = lexbuf_unget(buf, i1)
        }
      )
      else lexing_isSYMBOLIC(buf, i0) where
      {
        val () = lexbuf_unget(buf, i1)
      }
    )

  end

} (* end of [lexing_isDOLLAR] *)

(* ****** ****** *)

fun
lexing_isLPAREN(buf: &lexbuf >> _, i0: int) : tnode = let

  val i1 = lexbuf_getc(buf)
  val c1 = char0_chr(i1)

in

  ifcase
  | c1 = '*' => lexing_COMMENT_mlblock(buf, i0, i1)
  | _(* else *) => T_SPECHAR(i0) where
    {
      val () = lexbuf_unget(buf, i1)
      val () = lexbuf_get_none(buf)
    } (* end of [......] *)

end (* end of [lexing_isLPAREN] *)

(* ****** ****** *)

fun
lexing_isSQUOTE(buf: &lexbuf >> _, i0: int) : tnode = loop(buf) where
{
//
  fun
  loop(buf: &lexbuf >> _): tnode = let

    val i0 = lexbuf_getc(buf)
    val c0 = char0_chr(i0)

  in

    ifcase
    | c0 = '\\' => loop1(buf)
    | c0 = '\'' => loop2(buf)
    | _(*else*) => loop3(buf)

  end

  and
  loop1(buf: &lexbuf >> _): tnode = let

    val i0 = lexbuf_getc(buf)
    val c0 = char0_chr(i0)

  in

    ifcase
    | char0_isdigit(c0) => loop11(buf)
    | char0_isprint(c0) => loop12(buf)
    | _ (* else *) => loop12(buf)

  end // end of [loop1]

  and
  loop2 (buf: &lexbuf >> _): tnode = T_CHAR_nil(lexbuf_get_fullseg(buf))

  and
  loop3(buf: &lexbuf >> _): tnode = let

    val i0 = lexbuf_getc(buf)
    val c0 = char0_chr(i0)

  in

    ifcase
    | isSQUOTE(c0) => T_CHAR_char(lexbuf_get_fullseg(buf))
    | _ (* else *) => (T_CHAR_char(lexbuf_get_fullseg(buf))) where
      {
        val () = lexbuf_unget(buf, i0)
      }

  end // end of [loop3]

  and
  loop11(buf: &lexbuf >> _): tnode = let

    val i0 = lexbuf_getc(buf)
    val c0 = char0_chr(i0)

  in

    ifcase
    | char0_isdigit(c0) => loop11(buf)
    | isSQUOTE(c0) => T_CHAR_slash(lexbuf_get_fullseg(buf))
    | _ (* else *) => T_CHAR_slash(lexbuf_get_fullseg(buf)) where
      {
        val () = lexbuf_unget(buf, i0)
      }

  end

  and
  loop12(buf: &lexbuf >> _): tnode = let
    val i0 = lexbuf_getc(buf)
    val c0 = char0_chr(i0)

  in

    ifcase
    | isSQUOTE(c0) => T_CHAR_slash(lexbuf_get_fullseg(buf))
    | _ (* else *) => T_CHAR_slash(lexbuf_get_fullseg(buf)) where
      {
        val () = lexbuf_unget(buf, i0)
      }

  end // end of [loop12]

} (* end of [lexing_isSQUOTE] *)

(* ****** ****** *)

fun
lexing_isDQUOTE
(buf: &lexbuf >> _, i0: int) : tnode = loop(buf) where
{

  fun
  loop(buf: &lexbuf >> _): tnode = let

    val i0 = lexbuf_getc(buf)
    val c0 = char0_chr(i0)

    (*
    val () = println! ("i0 = ", i0)
    *)

  in

    ifcase
    | c0 = '\"' => T_STRING_closed(lexbuf_get_fullseg(buf))//unclsd
    | c0 = '\\' => loop(buf) where
      {
        val i0 = lexbuf_getc(buf)
      } (* end of [......] *)
    | _(* else *) =>
      if (i0 >= 0)
      then loop(buf)
      else T_STRING_unclsd(* closed *)(lexbuf_get_fullseg(buf))

  end

} (* end of [lexing_isDQUOTE] *)

(* ****** ****** *)

in (* in-of-local *)

implement
lexing_tnode(buf) = let

  val i0 = lexbuf_getc(buf)
  val c0 = char0_chr(i0)

  (*
  val () = println!("lexing_tnode: c0 = ", c0)
  *)

in

  ifcase
  | isEOL(c0) => lexing_isEOL(buf, i0)
  | isBLANK(c0) => lexing_isBLANK(buf, i0)
  | isSLASH(c0) => lexing_isSLASH(buf, i0)
  | isDIGIT(c0) => lexing_isDIGIT(buf, i0)
  | isIDENTFST(c0) => lexing_isIDENTFST(buf, i0)
  | isSHARP(c0) => lexing_isSHARP(buf, i0)
  | isDOLLAR(c0) => lexing_isDOLLAR(buf, i0)
  | isSYMBOLIC(c0) => lexing_isSYMBOLIC(buf, i0)
  | isLPAREN(c0) => lexing_isLPAREN(buf, i0)
  | isSQUOTE(c0) => lexing_isSQUOTE(buf, i0)
  | isDQUOTE(c0) => lexing_isDQUOTE(buf, i0)
  | _(* else *) =>
    (
      if (i0 >= 0) then T_SPECHAR(i0) else T_EOF()
    ) where
    {
      val ((*void*)) = lexbuf_get_none(buf)
    }

end // end of [lexing_token]

(* ****** ****** *)

implement
lexing_COMMENT_rest(buf, sym) = loop0(buf) where
{

  fun
  loop0(buf: &lexbuf >> _): tnode = let

    val i0 = lexbuf_getc(buf)
    val c0 = char0_chr(i0)

  in

    if (i0 >= 0)
    then loop0(buf)
    else T_COMMENT_rest(sym, lexbuf_get_fullseg(buf))

  end // end of [loop0]

} (* end of [lexing_COMMENT_rest] *)

(* ****** ****** *)

implement
lexing_COMMENT_line(buf, sym) = loop0(buf) where
{

  fun
  loop0(buf: &lexbuf >> _): tnode = let

    val i0 = lexbuf_getc(buf)
    val c0 = char0_chr(i0)

  in

    if isEOL(c0)
    then T_COMMENT_line (sym, lexbuf_get_fullseg(buf)) where
    {
      val () = lexbuf_unget(buf, i0)
    }
    else
    (
      if (i0 >= 0)
      then loop0(buf)
      else T_COMMENT_line(sym, lexbuf_get_fullseg(buf))
    )

  end

} (* end of [lexing_COMMENT_line] *)

(* ****** ****** *)

implement
lexing_COMMENT_cblock(buf, c0, c1) = let
//
  fun
  loop0(buf: &lexbuf >> _, lvl: int): tnode =
    if (lvl > 0)
    then let

      val i0 = lexbuf_getc(buf)
      val c0 = char0_chr(i0)

    in

      ifcase
      | isASTRSK(c0) => loop1(buf, lvl)
      | _(* non-ASTRSK *) =>
        (
          if (i0 >= 0)
          then loop0(buf, lvl)
          else T_COMMENT_cblock(lvl, lexbuf_get_fullseg(buf))
        )

    end

    else T_COMMENT_cblock(0(*lvl*), lexbuf_get_fullseg(buf))

  and
  loop1(buf: &lexbuf >> _, lvl: int): tnode = let

    val i1 = lexbuf_getc(buf)
    val c1 = char0_chr(i1)

  in

    ifcase
    | isSLASH(c1) => loop0(buf, lvl-1)
    | isASTRSK(c1) => loop1(buf, lvl-0)
    | _(*non-ASTRSK-SLASH*) => loop0(buf, lvl)

  end // end of [loop1]

in
  loop0(buf, 1(*lvl*))
end // end of [lexing_COMMENT_cblock]

(* ****** ****** *)

implement
lexing_COMMENT_mlblock(buf, c0, c1) = let
//
  fun
  loop0(buf: &lexbuf >> _, lvl: int): tnode =
    if (lvl > 0)
    then let

      val i0 = lexbuf_getc(buf)
      val c0 = char0_chr(i0)

    in

      ifcase
      | isASTRSK(c0) => loop1(buf, lvl)
      | isLPAREN(c0) => loop2(buf, lvl)
      | _(* non-ASTRSK-LPAREN *) =>
      (
        if (i0 >= 0)
        then loop0(buf, lvl)
        else T_COMMENT_mlblock(lvl, lexbuf_get_fullseg(buf))
      )

    end // end of [then]

    else T_COMMENT_mlblock(0(*lvl*), lexbuf_get_fullseg(buf))

  and
  loop1(buf: &lexbuf >> _, lvl: int): tnode = let

    val i1 = lexbuf_getc(buf)
    val c1 = char0_chr(i1)

  in

    ifcase
    | isASTRSK(c1) => loop1(buf, lvl-0)
    | isRPAREN(c1) => loop0(buf, lvl-1)
    | _(*non-ASTRSK-LPAREN*) => loop0(buf, lvl)

  end // end of [loop1]

  and
  loop2(buf: &lexbuf >> _, lvl: int): tnode = let

    val i1 = lexbuf_getc(buf)
    val c1 = char0_chr(i1)

  in

    ifcase
    | isASTRSK(c1) => loop0(buf, lvl+1)
    | isLPAREN(c1) => loop2(buf, lvl+0)
    | _(*non-ASTRSK-LPAREN*) => loop0(buf, lvl)

  end // end of [loop2]

in

  loop0(buf, 1(*lvl*))

end

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

implement
fpath_tokenize(fpath) = let
//
  val opt = FILEref_open_opt(fpath, "r"(* file_mode_r *))
//
in
  case+ opt of
  | ~optn0_vt_none() => optn1_vt_none()
  | ~optn0_vt_some(inp) => optn1_vt_some(fileref_tokenize(inp))
end // end of [fpath_tokenize]

(* ****** ****** *)

local

#staload
"./../../util/SATS/cblist.sats"
#staload
"./../../util/SATS/Posix/cblist.sats"
#staload _ =
"./../../util/DATS/Posix/cblist.dats"

in (* in-of-local *)

implement
string_tokenize(inp) = let
//
  fun
  loop(buf: &lexbuf >> _,res: tnodelst_vt): tnodelst_vt = let

    val tnd = lexing_tnode(buf)
    (*
    val (_) = println!("string_tokenize: loop: tnd = ", tnd)
    *)

  in

    case+ tnd of
    | T_EOF() => (list1_vt_cons(tnd, res))
    | _(*non-EOF*) => (loop(buf, list1_vt_cons(tnd, res)))

  end // end of [loop]

  var buf: lexbuf

  val cbs = string2cblist(inp)
  val (_) = lexbuf_initize_cblist(buf, cbs)
  val tnds = loop(buf, list1_vt_nil(*void*))

in

  toks where
  {

    var pos0: pos_t

    val tnds = ofg0(list0_vt_reverse(g0ofg1 tnds))

    val ((*void*)) = $LOC.position_initize(pos0, 0, 0, 0)
    val toks = lexing_locatize_nodelst(pos0, list1_vt2t(tnds))
    //
    (* val ((*freed*)) = list0_vt_free(g0ofg1 tnds) *)
    //

  } (* end of [where] *)

end // end of [string_tokenize]

(* ****** ****** *)

implement
fileref_tokenize(inp) = let

  val BSZ = i2sz(8012)

  fun
  loop(buf: &lexbuf >> _, res: tnodelst_vt): tnodelst_vt = let

    val tnd = lexing_tnode(buf)

  in

    case+ tnd of
    | T_EOF() => (list1_vt_cons(tnd, res))
    | _(*non-EOF*) => (loop(buf, list1_vt_cons(tnd, res)))

  end // end of [loop]

  var buf: lexbuf

  val-~optn1_vt_some(cbs) = (fileref_get_cblist_vt(inp, BSZ))

  local

    val cbs = $UN.castvwtp1(cbs)
    val (_) = lexbuf_initize_cblist(buf, cbs)

  in

    val tnds = loop(buf, list1_vt_nil(*void*))

  end // end of [local]
//
in

  toks where
  {

    var pos0: pos_t

    val () = cblist_vt_free(cbs)
    val tnds = list0_vt_reverse(g0ofg1 tnds)

    val ((*void*)) = $LOC.position_initize(pos0, 0, 0, 0)

    val nxt = list1_vt2t(ofg0(tnds))
    val toks = lexing_locatize_nodelst(pos0, nxt)

    (* val ((*freed*)) = list0_vt_free(tnds) *)

  }

end // end of [fileref_tokenize]

end // end of [local]

(* ****** ****** *)

(* end of [xats_lexing_util0.dats] *)
