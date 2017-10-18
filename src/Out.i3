(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE Out;
IMPORT Formatter;

PROCEDURE Setup() RAISES ANY;
(* To be called before any other use of this module. *)

(* === Formatting Streams === *)

VAR
  out: Formatter.T;
  (* The normal result stream. *)

END Out.
