(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                             *)
(* Created by Luca Cardelli                                    *)
(* Last modified on Sat Aug 15 20:53:56 PDT 1998 by heydon     *)

INTERFACE String;
(* Mutable strings of bytes *)

TYPE
  T = REF ARRAY OF CHAR;
  Index = CARDINAL;
  Size = CARDINAL;
  Comparison = {Lt, Eq, Gt};

VAR
  Empty: T;

PROCEDURE New(size: Size; init: CHAR): T RAISES ANY;
(* Make a new string of length size, all full of the init char *)

PROCEDURE FromArray(READONLY array: ARRAY OF CHAR): T RAISES ANY;
(* Make a new string out of an array of chars. *)

PROCEDURE FromSubArray(READONLY array: ARRAY OF CHAR; start: Index; size: Size)
  : T RAISES ANY;
(* Make a new string out of an array of chars. *)

PROCEDURE IsEmpty(string: T): BOOLEAN RAISES ANY;
(* Whether string is an empty text. Do not use string=NIL. *)

PROCEDURE Length(string: T): Size RAISES ANY;
(* Length of a string. *)

PROCEDURE GetChar(string: T; index: Index): CHAR RAISES ANY;
(* Get a char out of a string. *)

PROCEDURE SetChar(string: T; index: Index; char: CHAR) RAISES ANY;
(* Set a char of a string. *)

PROCEDURE GetSub(source: T; sourceStart: Index; sourceSize: Size)
  : T RAISES ANY;
(* Make a new string by extracting a portion of an old one. *)

PROCEDURE SetSub(
    destin: T;
    destinStart: Index;
    source: T;
    sourceStart: Index;
    sourceSize: Size) RAISES ANY;
(* Overwrite the destination, starting from index, with a portion of the
   source. *)

PROCEDURE Cat(string1, string2: T): T RAISES ANY;
(* Make a new string which is the concatenation of two strings. *)

PROCEDURE CatSub(
    string1: T;
    start1: Index;
    size1: Size;
    string2: T;
    start2: Index;
    size2: Size)
    : T RAISES ANY;
(* Make a new string which is the concatenation of part of two strings. *)

PROCEDURE Replace(
    destin: T;
    destinStart: Index;
    destinSize: Size;
    source: T;
    sourceStart: Index;
    sourceSize: Size)
    : T RAISES ANY;
(* Make a new string by replacing part of destination by part of source. *)

PROCEDURE Equal(string1, string2: T): BOOLEAN RAISES ANY;
(* Compare two strings for equality. *)

PROCEDURE EqualSub(
    string1: T;
    start1: Index;
    size1: Size;
    string2: T;
    start2: Index;
    size2: Size)
    : BOOLEAN RAISES ANY;
(* Compare parts of two strings for equality. *)

PROCEDURE Compare(string1, string2: T): Comparison RAISES ANY;
(* Compare two strings. *)

PROCEDURE CompareSub(
    string1: T;
    start1: Index;
    size1: Size;
    string2: T;
    start2: Index;
    size2: Size;
    ignoreCase: BOOLEAN)
    : Comparison RAISES ANY;
(* Compare parts of two strings. *)
(* -- ignoreCase not implemented. *)

(* --

   PROCEDURE Find( source: T; sourceStart: Index; sourceSize: Size; pattern:
   T; forward: BOOLEAN; ignoreCase: BOOLEAN) : INTEGER RAISES ANY;
   (* Find the first occurrence of a string in part of another string. Return
      -1 if not found. *)

   PROCEDURE FindChar( source: T; sourceStart: Index; sourceSize: Size;
   pattern: CHAR; forward: BOOLEAN; ignoreCase: BOOLEAN) : INTEGER RAISES ANY;
   (* Find the first occurrence of a character in part of a string. Return -1
      if not found. *)

*)

PROCEDURE FromText(text: TEXT): T RAISES ANY;
(* Convert from a TEXT. *)

PROCEDURE ToText(string: T): TEXT RAISES ANY;
(* Convert to a TEXT. *)

END String.
