(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                             *)
(* Created by Luca Cardelli                                    *)
(* Last modified on Sat Aug 15 20:47:09 PDT 1998 by heydon     *)

MODULE String;
IMPORT Text;

(* Modifiable strings of bytes *)

PROCEDURE New(size: Size; init: CHAR): T =
  VAR res: T; i: INTEGER;
  BEGIN
    res := NEW(T, size);
    i := 0;
    WHILE i < size DO res^[i] := init; INC(i) END;
    RETURN res;
  END New;

PROCEDURE FromArray(READONLY array: ARRAY OF CHAR): T =
  VAR res: T; i, size: INTEGER;
  BEGIN
    size := NUMBER(array);
    res := NEW(T, size);
    i := 0;
    WHILE i < size DO res^[i] := array[i]; INC(i) END;
    RETURN res;
  END FromArray;

PROCEDURE FromSubArray(READONLY array: ARRAY OF CHAR;
  start: Index; size: Size): T =
  VAR res: T; i: INTEGER;
  BEGIN
    res := NEW(T, size);
    i := 0;
    WHILE i < size DO res^[i] := array[start + i]; INC(i) END;
    RETURN res;
  END FromSubArray;

PROCEDURE IsEmpty(string: T): BOOLEAN =
  BEGIN RETURN NUMBER(string^) = 0; END IsEmpty;

PROCEDURE Length(string: T): Size = 
  BEGIN RETURN NUMBER(string^); END Length;

PROCEDURE GetChar(string: T; index: Index): CHAR =
  BEGIN RETURN string^[index]; END GetChar;

PROCEDURE SetChar(string: T; index: Index; char: CHAR) =
  BEGIN string^[index] := char; END SetChar;

PROCEDURE GetSub(source: T; sourceStart: Index; sourceSize: Size): T =
  VAR res: T; i: INTEGER;
  BEGIN
    res := NEW(T, sourceSize);
    i := 0;
    WHILE i < sourceSize DO
      res^[i] := source^[sourceStart + i];
      INC(i);
    END;
    RETURN res;
  END GetSub;

PROCEDURE SetSub(
    destin: T;
    destinStart: Index;
    source: T;
    sourceStart: Index;
    sourceSize: Size) =
  VAR i: INTEGER;
  BEGIN
    i := 0;
    WHILE i < sourceSize DO
      destin^[destinStart + i] := source^[sourceStart + i];
      INC(i);
    END;
  END SetSub;

PROCEDURE Cat(string1, string2: T): T =
  VAR res: T; i, size1, size2: INTEGER;
  BEGIN
    size1 := NUMBER(string1^);
    size2 := NUMBER(string2^);
    res := NEW(T, size1 + size2);
    i := 0;
    WHILE i < size1 DO res^[i] := string1^[i]; INC(i); END;
    i := 0;
    WHILE i < size2 DO res^[size1 + i] := string2^[i]; INC(i); END;
    RETURN res;
  END Cat;

PROCEDURE CatSub(
    string1: T;
    start1: Index;
    size1: Size;
    string2: T;
    start2: Index;
    size2: Size)
    : T =
  VAR res: T; i: INTEGER;
  BEGIN
    res := NEW(T, size1 + size2);
    i := 0;
    WHILE i < size1 DO res^[i] := string1^[start1 + i]; INC(i) END;
    i := 0;
    WHILE i < size2 DO
      res^[size1 + i] := string2^[start2 + i];
      INC(i);
    END;
    RETURN res;
  END CatSub;

PROCEDURE Replace(
    destin: T;
    destinStart: Index;
    destinSize: Size;
    source: T;
    sourceStart: Index;
    sourceSize: Size)
    : T =
  VAR res: T; i, j, k, destinLength: INTEGER;
  BEGIN
    destinLength := NUMBER(destin^);
    res := NEW(T, destinLength - destinSize + sourceSize);
    i := 0;
    j := 0;
    WHILE j < destinStart DO
      res^[i] := destin^[j];
      INC(i);
      INC(j);
    END;
    k := sourceStart;
    WHILE k < sourceStart + sourceSize DO
      res^[i] := source^[k];
      INC(i);
      INC(k);
    END;
    j := j + destinSize;
    WHILE j < destinLength DO
      res^[i] := destin^[j];
      INC(i);
      INC(j);
    END;
    RETURN res;
  END Replace;

PROCEDURE Equal(string1, string2: T): BOOLEAN =
  VAR length, i: INTEGER;
  BEGIN
    length := NUMBER(string1^);
    IF length # NUMBER(string2^) THEN RETURN FALSE END;
    i := 0;
    WHILE i < length DO
      IF string1^[i] # string2^[i] THEN RETURN FALSE END;
      INC(i);
    END;
    RETURN TRUE;
  END Equal;

PROCEDURE EqualSub(
    string1: T;
    start1: Index;
    size1: Size;
    string2: T;
    start2: Index;
    size2: Size)
    : BOOLEAN =
  VAR i: INTEGER;
  BEGIN
    IF size1 # size2 THEN RETURN FALSE END;
    i := 0;
    WHILE i < size1 DO
      IF string1^[start1 + i] # string2^[start2 + i] THEN RETURN FALSE END;
      INC(i);
    END;
    RETURN TRUE;
  END EqualSub;

PROCEDURE Compare(string1, string2: T): Comparison =
  VAR size1, size2, i: INTEGER;
  BEGIN
    size1 := NUMBER(string1^);
    size2 := NUMBER(string2^);
    i := 0;
    LOOP
      IF (i = size1) AND (i = size2) THEN RETURN Comparison.Eq END;
      IF i = size1 THEN RETURN Comparison.Lt END;
      IF i = size2 THEN RETURN Comparison.Gt END;
      IF string1^[i] # string2^[i] THEN
        IF string1^[i] < string2^[i] THEN 
	  RETURN Comparison.Lt 
	ELSE 
	  RETURN Comparison.Gt 
	END;
      END;
      INC(i);
    END;
  END Compare;

PROCEDURE CompareSub(
    string1: T;
    start1: Index;
    size1: Size;
    string2: T;
    start2: Index;
    size2: Size;
    ignoreCase: BOOLEAN)
    : Comparison =
  VAR i: INTEGER;
  BEGIN
(* -- ignoreCase not implemented. *)
    ignoreCase := FALSE;
    i := 0;
    LOOP
      IF (start1+i >= size1) AND (start2+i >= size2) THEN 
	RETURN Comparison.Eq;
      END;
      IF start1+i >= size1 THEN RETURN Comparison.Lt END;
      IF start2+i >= size2 THEN RETURN Comparison.Gt END;
      IF string1^[start1+i] # string2^[start2+i] THEN
        IF string1^[start1+i] < string2^[start2+i] THEN 
	  RETURN Comparison.Lt 
	ELSE 
	  RETURN Comparison.Gt 
	END;
      END;
      INC(i);
    END;
  END CompareSub;

(* --

   PROCEDURE FindChar( source: T; sourceStart: Index; sourceSize: Size;
   pattern: CHAR; forward: BOOLEAN; ignoreCase: BOOLEAN) : INTEGER; VAR i,
   limit: INTEGER; BEGIN ASSERT(forward, "FindChar :backward search not
   implemented"); ASSERT(NOT (ignoreCase), "FindChar :ignoreCase search not
   implemented"); i := sourceStart; limit := MIN(NUMBER(source^), i +
   sourceSize); LOOP IF i >= limit THEN RETURN -1 END; IF source^[i] = pattern
   THEN RETURN i END; INC(i); END; END FindChar;

   PROCEDURE Find( source: T; sourceStart: Index; sourceSize: Size; pattern:
   T; forward: BOOLEAN; ignoreCase: BOOLEAN) : INTEGER; BEGIN ASSERT(FALSE,
   "String.Find not implemented"); RETURN 0; END Find;

*)

PROCEDURE FromText(text: TEXT): T =
  VAR res: T;
  BEGIN
    res := NEW(T, Text.Length(text));
    Text.SetChars(res^, text);
    RETURN res;
  END FromText;

PROCEDURE ToText(string: T): TEXT =
  BEGIN RETURN Text.FromChars(string^); END ToText;

BEGIN 
  Empty := New(0, ' '); 
END String.
