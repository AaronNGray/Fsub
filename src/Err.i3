(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE Err;
IMPORT Formatter;

EXCEPTION Fail;

TYPE
  Location <: ROOT;

  LocationInfo =
    RECORD
      fileName: TEXT;
      char: INTEGER;
      line, lineChar: INTEGER;
    END;

PROCEDURE Setup() RAISES ANY;
(* To be called before any other use of this module. *)

PROCEDURE Raise() RAISES ANY;
(* To be called to raise Fail (this helps in breakpointing). *)

PROCEDURE NewLineLocation(READONLY info: LocationInfo): Location RAISES ANY;

PROCEDURE NewCharLocation(READONLY begInfo, endInfo: LocationInfo): Location RAISES ANY;

PROCEDURE Msg(fmt: Formatter.T; msg: TEXT := "") RAISES ANY;

PROCEDURE Fault(fmt: Formatter.T; msg: TEXT := "") RAISES ANY;

PROCEDURE PrintLocation(fmt: Formatter.T; 
  location: Location; currentLine: INTEGER) RAISES ANY;

END Err.
