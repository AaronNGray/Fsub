(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE Check;
IMPORT Err, Formatter, Tree;

  TYPE

    Subst =
      BRANDED OBJECT 
        rest: Subst;
      END;

    SubstType =
      Subst BRANDED OBJECT
        name: Tree.IdeName;
        type: Type;
      END;

    SubstRank =
      Subst BRANDED OBJECT
        name: Tree.IdeName;
        rank: INTEGER;
      END;

    SubstShift =
      Subst BRANDED OBJECT
        shift: INTEGER;
      END;
    
    Type = 
      BRANDED OBJECT
        location: Err.Location;
	tag: Tree.IdeName:=NIL;
      END;

    TypeUniVar =
      Type BRANDED OBJECT
        name: Tree.IdeName;
      END;

    TypeIde =
      Type BRANDED OBJECT
        name: Tree.IdeName;
        index: INTEGER; (* deBruijn, > 0 *)
      END;

    TypeTop =
      Type BRANDED OBJECT
      END;

    TypeArrow =
      Type BRANDED OBJECT
        dom,rng: Type;
      END;

    TypeForall =
      Type BRANDED OBJECT
        binder: Tree.IdeName; omit: BOOLEAN;
        bound,body: Type;
      END;

    TypeRec =
      Type BRANDED OBJECT
        binder: Tree.IdeName;
        body: Type;
      END;

    Env <: Tree.Env;
      (* Inherit from Tree.Env for printing *) 

    TypeDefEnv <: TypeDefEnvBase;
    TypeDefEnvBase =
      Env BRANDED OBJECT
	bound, type: Type;
      END;
      (* Type defs are expanded by Scope; this structure is
	used just for top-level printing. It does not
	affect deBruijn numbers. *)

    TypeEnv <: TypeEnvBase;
    TypeEnvBase =
      Env BRANDED OBJECT
	bound: Type;
      END;

    TermEnv <: TermEnvBase;
    TermEnvBase =
      Env BRANDED OBJECT
	type: Type;
      END;

VAR topEnv: Env;

  PROCEDURE Setup() RAISES ANY;
  (* To be called before any other use of this module. *)

  PROCEDURE NewTypeDefEnv(name: Tree.IdeName; 
    bound, type: Type; rest: Env): Env RAISES ANY;
  PROCEDURE NewTypeEnv(name: Tree.IdeName; bound: Type; rest: Env)
    : Env RAISES ANY;
  PROCEDURE NewTermEnv(name: Tree.IdeName; type: Type; rest: Env)
    : Env RAISES ANY;

  PROCEDURE SubType(type1, type2: Type; env: Env; VAR(*in-out*)subst: Subst)
     RAISES ANY;
  (* Check subtyping of two normal-form types. *)

  PROCEDURE CheckContext(context: Tree.Context; env: Env): Env RAISES ANY;

  PROCEDURE CheckTypeBinding(binding: Tree.TypeBinding; env: Env;
    VAR(*in-out*)subst: Subst): Env RAISES ANY;

  PROCEDURE CheckTermBinding(binding: Tree.TermBinding; env: Env;
    VAR(*in-out*)subst: Subst): Env RAISES ANY;

  PROCEDURE CheckType(type: Tree.Type; env: Env; VAR(*out*) nfType:Type)
    RAISES ANY;

  PROCEDURE CheckTerm(term: Tree.Term; env: Env; 
    VAR(*in-out*)subst: Subst): Type RAISES ANY;

  PROCEDURE ExpandTermEnv(env, endEnv: Env; subst: Subst): Env RAISES ANY;

  PROCEDURE PrintUniVar(fmt: Formatter.T; name: Tree.IdeName; rank: INTEGER;
    VAR(*in-out*) uniVars: Tree.Env) RAISES ANY;

  PROCEDURE PrintType(fmt: Formatter.T; type: Type; env: Env; subst: Subst;
    VAR(*in-out*)uniVars: Tree.Env) RAISES ANY;

END Check.
