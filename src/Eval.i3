(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE Eval;
IMPORT Tree, Value;

  PROCEDURE Setup() RAISES ANY;
  (* To be called before any other use of this module. *)

  PROCEDURE TypeBinding(binding: Tree.TypeBinding; env: Value.Env)
    : Value.Env RAISES ANY;

  PROCEDURE TermBinding(binding: Tree.TermBinding; env: Value.Env)
    : Value.Env RAISES ANY;

  PROCEDURE Term(term: Tree.Term; env: Value.Env): Value.Val RAISES ANY;

END Eval.
