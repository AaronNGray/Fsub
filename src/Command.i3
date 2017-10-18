(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE Command;

TYPE
  T =
    BRANDED OBJECT
      name: TEXT;
    METHODS
      Exec(arg: TEXT:=NIL) RAISES ANY;
    END;

PROCEDURE Setup() RAISES ANY;

PROCEDURE Register(command:T) RAISES ANY;

PROCEDURE Exec(name: TEXT; arg: TEXT:=NIL) RAISES ANY;
(* IF name="?", all the register commands are invoked
   with argument "?"; they should print info about themselves. *)

END Command.
