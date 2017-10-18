(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                             *)
(* Created by Luca Cardelli                                    *)
(* Last modified on Sat Aug 15 19:54:19 PDT 1998 by heydon     *)

MODULE Out;
IMPORT Stdio, Formatter;

PROCEDURE Setup() =
  BEGIN
    out := Formatter.New(Stdio.stdout, 75);
  END Setup;

BEGIN
END Out.
