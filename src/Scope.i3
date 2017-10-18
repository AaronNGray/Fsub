(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE Scope;
IMPORT Tree;

TYPE
  Env <: ROOT;

  PROCEDURE Setup() RAISES ANY;
  (* To be called before any other use of this module. *)

VAR
  topEnv: Env;

PROCEDURE SetAbsoluteIndex(name: Tree.IdeName; env: Env) RAISES ANY;

PROCEDURE ScopeContext(context: Tree.Context; env: Env): Env 
  RAISES ANY;

PROCEDURE ScopeTypeBinding(binding: Tree.TypeBinding; env: Env): Env 
  RAISES ANY;

PROCEDURE ScopeTermBinding(binding: Tree.TermBinding; env: Env): Env 
  RAISES ANY;

PROCEDURE ScopeType(type: Tree.Type; env: Env): Tree.Type RAISES ANY;

PROCEDURE ScopeTerm(term: Tree.Term; env: Env) RAISES ANY;

END Scope.

