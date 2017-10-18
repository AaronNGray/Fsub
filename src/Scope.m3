(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE Scope;
IMPORT Err, Out, Fmt, Formatter, Scanner, Tree;

REVEAL

  Env = BRANDED OBJECT END;

TYPE

  TypeDefEnv =
    Env BRANDED OBJECT
      name: Tree.IdeName;
      type: Tree.Type;
      rest: Env;
    END;

  TypeEnv =
    Env BRANDED OBJECT
      name: Tree.IdeName;
      rest: Env;
    END;

  TermEnv =
    Env BRANDED OBJECT
      name: Tree.IdeName;
      rest: Env;
    END;

PROCEDURE ScopeError(msg: TEXT; location: Err.Location) RAISES ANY =
    VAR info: Err.LocationInfo;
    BEGIN
      Scanner.CurrentLocationInfo((*out*)info);
      Formatter.PutText(Out.out, msg); 
	Formatter.NewLine(Out.out);
	Formatter.PutText(Out.out, "  ");
	Err.PrintLocation(Out.out, location, info.line);
	Formatter.NewLine(Out.out);
      Formatter.PutText(Out.out, "Error detected ");
        Err.PrintLocation(Out.out, Err.NewLineLocation(info), info.line);
	Formatter.NewLine(Out.out);
      Err.Raise();
   END ScopeError;


PROCEDURE Length(env: Env): INTEGER RAISES ANY =
  VAR len: INTEGER;
  BEGIN
    len := 0;
    LOOP
      TYPECASE env OF
      | NULL => 
	 RETURN len;
      | TypeDefEnv(node) =>
	INC(len);
	env := node.rest;
      | TypeEnv(node) =>
	INC(len);
	env := node.rest;
      | TermEnv(node) =>
	INC(len);
	env := node.rest;
      END;
    END
  END Length;
  
PROCEDURE SetAbsoluteIndex(name: Tree.IdeName; env: Env) RAISES ANY =
  BEGIN
    (*
    IF name.absoluteEnvIndex>=0 THEN 
      Err.Msg(Out.out, "SetAbsoluteIndex "
          & name.text & " " & Fmt.Int(name.absoluteEnvIndex));
    END;
    *)
    LOOP
      TYPECASE env OF
      | NULL => 
	  ScopeError(
	    "Free variable in action must be bound at the top level: " & 
	    Tree.FmtIdeName(name, NIL), name.location);
      | TypeDefEnv(node) =>
	IF Tree.SameIdeName(name, node.name) THEN
	  name.absoluteEnvIndex := Length(env); EXIT;
	ELSE
	  env := node.rest;
	END;
      | TypeEnv(node) =>
	IF Tree.SameIdeName(name, node.name) THEN
	  name.absoluteEnvIndex := Length(env); EXIT;
	ELSE
	  env := node.rest;
	END;
      | TermEnv(node) =>
	IF Tree.SameIdeName(name, node.name) THEN 
	  name.absoluteEnvIndex := Length(env); EXIT;
	ELSE
	  env := node.rest;
	END;
      END;
    END
  END SetAbsoluteIndex;

PROCEDURE CheckAbsoluteIndex(name: Tree.IdeName; 
	location: Err.Location; env: Env) RAISES ANY =
  BEGIN
    IF name.absoluteEnvIndex >= 0 THEN
      IF name.absoluteEnvIndex # Length(env) THEN
	ScopeError(
	  "Variable free in syntax action would be captured in expansion: " & name.text,
	  location);
      END;
    END;
  END CheckAbsoluteIndex;
    
PROCEDURE LookupTermIde((*mod*)name: Tree.IdeName; 
	location: Err.Location; env: Env): INTEGER RAISES ANY =
  VAR index: INTEGER;
  BEGIN
    index := 1;
    LOOP
      TYPECASE env OF
      | NULL => 
	  ScopeError(
	    "Unbound term identifier: " & Tree.FmtIdeName(name, NIL),
	    location);
      | TypeDefEnv(node) =>
	IF Tree.SameIdeName(name, node.name) THEN
	  ScopeError(
	    "Type identifier found in term position: " & name.text,
	    location);
	ELSE
	  env := node.rest;
	END;
      | TypeEnv(node) =>
	IF Tree.SameIdeName(name, node.name) THEN
	  ScopeError(
	    "Type identifier found in term position: " & name.text,
	    location);
	ELSE
	  INC(index);
	  env := node.rest;
	END;
      | TermEnv(node) =>
	IF Tree.SameIdeName(name, node.name) THEN
	  CheckAbsoluteIndex(name, location, env);
	  RETURN index;
	ELSE
	  INC(index);
	  env := node.rest;
	END;
      END;
    END;
  END LookupTermIde;

PROCEDURE LookupTypeIde((*mod*)ide: Tree.TypeIde; 
    location: Err.Location; env: Env): Tree.Type RAISES ANY =
  VAR index: INTEGER;
  BEGIN
    index := 1;
    LOOP
      TYPECASE env OF
      | NULL => 
	  ScopeError(
	    "Unbound type identifier: " & Tree.FmtIdeName(ide.name, NIL),
	    location);
      | TypeDefEnv(node) =>
	  IF Tree.SameIdeName(ide.name, node.name) THEN 
	    CheckAbsoluteIndex(ide.name, location, env);
	    RETURN ScopeType(Tree.Copy(node.type), env);
	  ELSE
	    env := node.rest;
	  END;
      | TypeEnv(node) =>
	  IF Tree.SameIdeName(ide.name, node.name) THEN 
	    CheckAbsoluteIndex(ide.name, location, env);
	    ide.index := index;
	    RETURN ide;
	  ELSE
	    INC(index);
	    env := node.rest;
	  END;
      | TermEnv(node) =>
	IF Tree.SameIdeName(ide.name, node.name) THEN
	  ScopeError(
	    "Term identifier found in type position: " & ide.name.text,
	    location);
	ELSE
	  INC(index);
	  env := node.rest;
	END;
      END;
    END;
  END LookupTypeIde;

PROCEDURE ScopeTypeBinding(binding: Tree.TypeBinding; env: Env)
    : Env RAISES ANY =
  VAR type: Tree.Type;
  BEGIN
    TYPECASE binding OF
    | NULL => RETURN env;
    | Tree.TypeBinding(node) =>
	type := node.type;
	node.bound := ScopeType(node.bound, env);
	node.type := ScopeType(node.type, env);
	node.type.tag := node.binder;
	TYPECASE node.type OF
	| Tree.TypeRec(nodeRec) => nodeRec.body.tag := node.binder;
	ELSE
	END;
	RETURN ScopeTypeBinding(node.rest,
	  NEW(TypeDefEnv, name:=node.binder, type:=type, rest:=env));
    END;
  END ScopeTypeBinding;

PROCEDURE ScopeTermBinding(binding: Tree.TermBinding; env: Env)
    : Env RAISES ANY =
  VAR type: Tree.Type;
  BEGIN
    TYPECASE binding OF
    | NULL => RETURN env;
    | Tree.TermBinding(node) =>
	IF node.bound#NIL THEN
	  node.bound := ScopeType(node.bound, env);
	END;
	ScopeTerm(node.term, env);
	RETURN ScopeTermBinding(node.rest,
	  NEW(TermEnv, name:=node.binder, rest:=env));
    END;
  END ScopeTermBinding;

PROCEDURE ScopeType(type: Tree.Type; env: Env): Tree.Type RAISES ANY =
  BEGIN
    TYPECASE type OF
    | NULL => Err.Fault(Out.out, "ScopeType NIL");
    | Tree.TypeIde(node) =>
	RETURN LookupTypeIde((*mod*)node, type.location, env);
    | Tree.TypeTop(node) =>
	RETURN node;
    | Tree.TypeArrow(node) =>
	node.dom := ScopeType(node.dom, env);
	node.rng := ScopeType(node.rng,
	  NEW(TermEnv, name:=Tree.noName, rest:=env));
	RETURN node;
    | Tree.TypeForall(node) =>
	node.bound := ScopeType(node.bound, env);
	node.body := ScopeType(node.body,
	  NEW(TypeEnv, name:=node.binder, rest:=env));
	RETURN node;
    | Tree.TypeRec(node) =>
	node.body := ScopeType(node.body,
	  NEW(TypeEnv, name:=node.binder, rest:=env));
	RETURN node;
    | Tree.TypePatternPosition, Tree.TypePatternForall =>
	ScopeError("Pattern positions (_n) not allowed here",
	  type.location);
    ELSE Err.Fault(Out.out, "ScopeType");
    END;
  END ScopeType;

PROCEDURE ScopeTerm(term: Tree.Term; env: Env) RAISES ANY =
  BEGIN
    TYPECASE term OF
    | NULL => Err.Fault(Out.out, "ScopeTerm NIL");
    | Tree.TermIde(node) =>
	node.index := LookupTermIde((*mod*)node.name, term.location, env);
    | Tree.TermTop(node) =>
    | Tree.TermFun(node) =>
	node.bound := ScopeType(node.bound, env);
	ScopeTerm(node.body,
	  NEW(TermEnv, name:=node.binder, rest:=env));
    | Tree.TermAppl(node) =>
	ScopeTerm(node.fun, env);
	ScopeTerm(node.arg, env);
    | Tree.TermFun2(node) =>
	node.bound := ScopeType(node.bound, env);
	ScopeTerm(node.body,
	  NEW(TypeEnv, name:=node.binder, rest:=env));
    | Tree.TermAppl2(node) =>
	ScopeTerm(node.fun, env);
	node.arg := ScopeType(node.arg, env);
    | Tree.TermFold(node) =>
	node.recType := ScopeType(node.recType, env);
	ScopeTerm(node.arg, env);
    | Tree.TermUnfold(node) =>
	ScopeTerm(node.arg, env);
    | Tree.TermRec(node) =>
	node.bound := ScopeType(node.bound, env);
	ScopeTerm(node.body,
	  NEW(TermEnv, name:=node.binder, rest:=env));
    | Tree.TermPatternPosition, Tree.TermPatternFun, Tree.TermPatternFun2 =>
	ScopeError("Pattern positions (_n) not allowed here",
	  term.location);
    ELSE Err.Fault(Out.out, "ScopeTerm");
    END;
  END ScopeTerm;

  PROCEDURE ScopeContext(context: Tree.Context; env: Env): Env RAISES ANY =
    BEGIN
      TYPECASE context OF
      | NULL => RETURN env;
      | Tree.ContextType(node) =>
	  node.bound:=
	    ScopeType(node.bound, env);
	  RETURN ScopeContext(node.rest,
	    NEW(TypeEnv, name:=node.binder, rest:=env));
      | Tree.ContextTerm(node) =>
	  node.type:=
	    ScopeType(node.type, env);
	  RETURN ScopeContext(node.rest,
	    NEW(TermEnv, name:=node.binder, rest:=env));
      END;
    END ScopeContext;

PROCEDURE Setup() RAISES ANY =
  BEGIN
    topEnv := NIL;
  END Setup;

BEGIN
END Scope.
