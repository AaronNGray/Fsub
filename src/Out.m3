(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE Out;
IMPORT Stdio, Formatter;

PROCEDURE Setup() RAISES ANY =
  BEGIN
    out := Formatter.New(Stdio.stdout, 75);
  END Setup;

BEGIN
END Out.
