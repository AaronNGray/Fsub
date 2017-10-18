(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                             *)
(* Created by Luca Cardelli                                    *)
(* Last modified on Sat Aug 15 20:55:58 PDT 1998 by heydon     *)

INTERFACE Tree;
IMPORT Formatter, Gram, Parse;

TYPE

  IdeName =
    Parse.Tree BRANDED OBJECT
      text: TEXT;
      variant: INTEGER;
      absoluteEnvIndex: INTEGER;
    END;

  Grammar =
    Parse.Tree BRANDED OBJECT
      gramInfo: Gram.GramInfo;
    END;

  TypeBinding =
    Parse.Tree BRANDED OBJECT
      binder: IdeName;
      bound: Type;
      type: Type;
      rest: TypeBinding;
    END; 

  TermBinding =
    Parse.Tree BRANDED OBJECT
      binder: IdeName;
      bound: Type;
      term: Term;
      rest: TermBinding;
    END; 

  JudgeContext =
    Parse.Tree BRANDED OBJECT
      context: Context;
    END;

  JudgeType =
    Parse.Tree BRANDED OBJECT
      context: Context;
      type: Type;
    END;

  JudgeSubtype =
    Parse.Tree BRANDED OBJECT
      context: Context;
      subType, superType: Type;
    END;

  JudgeTerm =
    Parse.Tree BRANDED OBJECT
      context: Context;
      term: Term;
      type: Type;
    END;

  Context =
    Parse.Tree BRANDED OBJECT 
      binder: IdeName;
      rest: Context;
    END;

  ContextType =
    Context BRANDED OBJECT
      (* omit: BOOLEAN; Not implemented *)
      bound: Type;
    END;

  ContextTerm =
    Context BRANDED OBJECT
      type: Type;
    END;

  Type = 
    Parse.Tree BRANDED OBJECT 
      tag: IdeName:=NIL;
    END;

  TypeIde =
    Type BRANDED OBJECT
      name: IdeName;
      index: INTEGER; (* deBruijn, > 0 *)
    END;

  TypePatternPosition =
    Type BRANDED OBJECT
      position: INTEGER;
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
      binder: IdeName; omit: BOOLEAN;
      bound,body: Type;
    END;

  TypePatternForall =
    Type BRANDED OBJECT
      position: INTEGER; omit: BOOLEAN;
      bound,body: Type;
    END;

  TypeRec =
    Type BRANDED OBJECT
      binder: IdeName; 
      body: Type;
    END;

  Term = 
    Parse.Tree BRANDED OBJECT END;

  TermIde =
    Term BRANDED OBJECT
      name: IdeName;
      index: INTEGER; (* deBruijn, > 0 *)
      omitArgs: BOOLEAN:=TRUE; (* for type inference *)
      omitCount: INTEGER:=0;
    END;

  TermPatternPosition =
    Term BRANDED OBJECT
      position: INTEGER;
    END;

  TermTop =
    Term BRANDED OBJECT
    END;

  TermFun =
    Term BRANDED OBJECT
      binder: IdeName;
      bound: Type;
      body: Term
    END;

  TermPatternFun =
    Term BRANDED OBJECT
      position: INTEGER;
      bound: Type;
      body: Term
    END;

  TermAppl =
    Term BRANDED OBJECT
      fun,arg: Term;
    END;

  TermFun2 =
    Term BRANDED OBJECT
      binder: IdeName; omit: BOOLEAN;
      bound: Type;
      body: Term
    END;

  TermPatternFun2 =
    Term BRANDED OBJECT
      position: INTEGER; omit: BOOLEAN;
      bound: Type;
      body: Term
    END;

  TermAppl2 =
    Term BRANDED OBJECT
      fun: Term;
      arg: Type;
    END;

  TermFold =
    Term BRANDED OBJECT
      recType: Type;
      arg: Term;
    END;

  TermUnfold =
    Term BRANDED OBJECT
      arg: Term;
    END;

  TermRec =
    Term BRANDED OBJECT
      binder: IdeName;
      bound: Type;
      body: Term
    END;

  Env <: EnvBase;
  EnvBase =
    OBJECT
      name: IdeName;
      rest: Env;
    END;

  PROCEDURE Setup() RAISES ANY;
  (* To be called before any other use of this module,
     after Gram.Setup and Act.Setup. *)

  PROCEDURE GetGrammars(VAR (*out*) actionTermGrammar, 
    actionTypeGrammar: Parse.NonTerminal) RAISES ANY;

  VAR 
    noName: IdeName;
    phrase: Parse.NonTerminal;
    env: Parse.GrammarEnv; (* The env for phrase *)

  PROCEDURE Copy(tree: Parse.Tree): Parse.Tree RAISES ANY;

  PROCEDURE SameIdeName(name1, name2: IdeName): BOOLEAN RAISES ANY;

  PROCEDURE FmtIdeName(name: IdeName; env: Env): TEXT RAISES ANY;

  PROCEDURE FmtIde(name: IdeName; index: INTEGER; env: Env): TEXT RAISES ANY;

  PROCEDURE NewEnv(name: IdeName; rest: Env): Env RAISES ANY;
  PROCEDURE BeEnv(env: Env; name: IdeName; rest: Env) RAISES ANY;

  PROCEDURE PrintIdeName(fmt: Formatter.T; name: IdeName; env: Env) RAISES ANY;

  PROCEDURE PrintIde(fmt: Formatter.T; name: IdeName; index: INTEGER; 
    env: Env) RAISES ANY;

  PROCEDURE PrintType(fmt: Formatter.T; type: Type; env: Env) RAISES ANY;

  PROCEDURE PrintTerm(fmt: Formatter.T; term: Term; env: Env) RAISES ANY;

  PROCEDURE PrintTypeBinding(fmt: Formatter.T; binding: TypeBinding; env: Env)
    RAISES ANY;

  PROCEDURE PrintTermBinding(fmt: Formatter.T; binding: TermBinding; env: Env)
    RAISES ANY;

END Tree.

(*
  Unit =
    Term BRANDED "Unit" OBJECT
    END;

  Char =
    Term BRANDED "Char" OBJECT
      char: CHAR;
    END;

  Int =
    Term BRANDED "Int" OBJECT
      int: INTEGER;
    END;

  Real =
    Term BRANDED "Real" OBJECT
      real: REAL
    END;

  String =
    Term BRANDED "String" OBJECT
      string: String.T;
    END;

*)
