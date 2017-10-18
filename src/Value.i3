(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                             *)
(* Created by Luca Cardelli                                    *)
(* Last modified on Sat Aug 15 20:55:56 PDT 1998 by heydon     *)

INTERFACE Value;
IMPORT Formatter, Tree, Check;

  TYPE
    Env = BRANDED OBJECT 
	name: Tree.IdeName;
        rest: Env;
      END;

    TypeDefEnv =
      Env BRANDED OBJECT
      END;
      (* Type defs are expanded by Scope; this structure is
	used just for top-level printing. It does not
	affect deBruijn numbers. *)

    TypeEnv =
      Env BRANDED OBJECT
	type: Set;
      END;

    TermEnv =
      Env BRANDED OBJECT
	val: Val;
      END;

    Set =
      BRANDED OBJECT
	type: Tree.Type;
	env: Env;
      END;

    Val = 
      BRANDED OBJECT 
        tag: Tree.IdeName:=NIL;
      END;

    ValTop =
      Val BRANDED OBJECT
      END;

    ValFun = 
      Val BRANDED OBJECT 
        fun: Tree.TermFun;
	env: Env;
      END;

    ValFun2 = 
      Val BRANDED OBJECT 
        fun: Tree.TermFun2;
	env: Env;
      END;

    ValSusp = 
      Val BRANDED OBJECT 
        term: Tree.Term;
	env: Env;
      END;

  VAR topEnv: Env;

  PROCEDURE Setup() RAISES ANY;
  (* To be called before any other use of this module. *)

  PROCEDURE PrintTypeBinding(fmt: Formatter.T;
    checkEnv, checkEnvStop: Check.Env; env, envStop: Env;
    VAR (*in-out*)uniVars: Tree.Env) RAISES ANY;

  PROCEDURE PrintTermBinding(fmt: Formatter.T;
    checkEnv, checkEnvStop: Check.Env; env, envStop: Env;
    subst: Check.Subst; VAR (*in-out*)uniVars: Tree.Env) RAISES ANY;

  PROCEDURE PrintVal(fmt: Formatter.T; val: Val; printEnv: Tree.Env) 
    RAISES ANY;

END Value.

(*
    UnitVal =
      T BRANDED "UnitVal" OBJECT
      END;

    CharVal =
      T BRANDED "CharVal" OBJECT
        char: CHAR
      END;

    IntVal =
      T BRANDED "IntVal" OBJECT
        int: INTEGER;
      END;

    RealVal =
      T BRANDED "RealVal" OBJECT
        real: REAL;
      END;

    StringVal =
      T BRANDED "StringVal" OBJECT
        string: String.T;
      END;

*)
