(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE Command;
IMPORT Text, Out, Formatter;

TYPE 
  List = OBJECT
      first: T;
      rest: List;
    END;

  VAR list: List;

  PROCEDURE Setup() RAISES ANY =
    BEGIN
      list:=NIL;
    END Setup;

  PROCEDURE Insert(command: T; list: List): List RAISES ANY =
    BEGIN
      IF list=NIL THEN 
	RETURN NEW(List, first:=command, rest:=list);
      ELSIF Text.Compare(command.name, list.first.name) < 0 THEN
	RETURN NEW(List, first:=command, rest:=list);
      ELSE
	list.rest := Insert(command, list.rest);
	RETURN list;
      END;
  END Insert;

  PROCEDURE Register(command: T) RAISES ANY =
    BEGIN
      list:=Insert(command, list);
    END Register;

  PROCEDURE Exec(name: TEXT; arg: TEXT:=NIL) RAISES ANY =
    VAR scan: List;
    BEGIN
      IF Text.Equal(name, "?") THEN
	scan := list;
	WHILE scan#NIL DO
	  scan.first.Exec("?");
	  scan:=scan.rest;
	END;
	Formatter.Flush(Out.out);
      ELSE
        scan:=list;
	WHILE scan#NIL DO
	  IF Text.Equal(name, scan.first.name) THEN
	    scan.first.Exec(arg);
	    Formatter.Flush(Out.out);
	    RETURN;
	  END;
	  scan:=scan.rest;
	END;
	Formatter.PutText(Out.out, "Command not found: " & name & "\n");
	Formatter.Flush(Out.out);
      END;
    END Exec;

BEGIN

END Command.
