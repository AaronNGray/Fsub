(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                             *)
(* Created by Luca Cardelli                                    *)
(* Last modified on Sat Aug 15 22:21:28 PDT 1998 by heydon     *)

MODULE Err;
IMPORT Text, Fmt, Formatter;

REVEAL
  Location =
    BRANDED OBJECT END;

TYPE
  LineLocation =
    Location BRANDED OBJECT
      fileName: TEXT;
      line, char: INTEGER;
    END;

  CharLocation =
    Location BRANDED OBJECT
      fileName: TEXT;
      begChar, endChar: INTEGER;
    END;

PROCEDURE Setup() =
  BEGIN
  END Setup;

PROCEDURE NewCharLocation(READONLY begInfo, endInfo: LocationInfo): Location =
  BEGIN
    RETURN 
      NEW(CharLocation, fileName:=begInfo.fileName, 
	begChar:=begInfo.char, endChar:=endInfo.char);
  END NewCharLocation;

PROCEDURE NewLineLocation(READONLY info: LocationInfo): Location =
  BEGIN
    RETURN NEW(LineLocation, fileName:=info.fileName, 
	line:=info.line, char:=info.lineChar);
  END NewLineLocation;

(* --
PROCEDURE SetCharLocation(
    VAR (*in-out*) location: Location;
    fileName: TEXT; begChar, endChar: INTEGER) RAISES ANY =
  BEGIN
    IF location=NIL THEN
      location := NewCharLocation(fileName, begChar, endChar);
    END;
  END SetCharLocation;

PROCEDURE SetLineLocation(
    VAR (*in-out*) location: Location;
    fileName: TEXT; line, char: INTEGER) RAISES ANY =
  BEGIN
    IF location=NIL THEN
      location := NewLineLocation(fileName, line, char);
    END;
  END SetLineLocation;
-- *)

PROCEDURE Msg(fmt: Formatter.T; msg: TEXT := "") RAISES ANY =
  BEGIN
    IF NOT Text.Empty(msg) THEN
      Formatter.PutText(fmt, msg);
      Formatter.PutChar(fmt, '\n');
      Formatter.Flush(fmt);
    END;
  END Msg;

PROCEDURE Raise() RAISES ANY=
  BEGIN
    RAISE Fail;
  END Raise;

PROCEDURE Fault(fmt: Formatter.T; msg: TEXT := "") RAISES ANY = 
  BEGIN 
    IF Text.Empty(msg) THEN msg := "Internal error" END;
    Formatter.PutText(fmt, msg);
    Formatter.PutChar(fmt, '\n');
    Formatter.Flush(fmt);
    Raise();
  END Fault;

PROCEDURE PrintLocation(fmt: Formatter.T;
    location: Location; currentLine: INTEGER) RAISES ANY =
  VAR relLine: INTEGER;
  BEGIN
    TYPECASE location OF <*NOWARN*>
    | NULL =>
    | CharLocation(loc) =>
      IF Text.Empty(loc.fileName) THEN
        IF loc.begChar=loc.endChar THEN
          Formatter.PutText(fmt, "(char " & Fmt.Int(loc.begChar) & ")");
        ELSE
          Formatter.PutText(fmt, "(chars " & Fmt.Int(loc.begChar) 
	  & ".." & Fmt.Int(loc.endChar) & ")");
	END;
      ELSE
        Formatter.PutText(fmt, "(file " & loc.fileName);
        IF loc.begChar=loc.endChar THEN
          Formatter.PutText(fmt, ") (char " & Fmt.Int(loc.begChar) & ")");
        ELSE
          Formatter.PutText(fmt, ") (chars " & Fmt.Int(loc.begChar) 
	  & ".." & Fmt.Int(loc.endChar) & ")");
	END;
      END;
    | LineLocation(loc) =>
      IF Text.Empty(loc.fileName) THEN
	relLine := loc.line-(currentLine+1);
	IF relLine=-1 THEN
          Formatter.PutText(fmt, "(last input line, char " 
	    & Fmt.Int(loc.char) & ")");
	ELSE
          Formatter.PutText(fmt, "(input line " & Fmt.Int(relLine) 
	    & ", char " & Fmt.Int(loc.char) & ")");
	END;
      ELSE
        Formatter.PutText(fmt, "(file " & loc.fileName 
            & ") (line " & Fmt.Int(loc.line) 
	    & ", char " & Fmt.Int(loc.char) & ")");
      END;
    END;
  END PrintLocation;

BEGIN
END Err.
