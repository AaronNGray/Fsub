(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                             *)
(* Created by Luca Cardelli                                    *)
(* Last modified on Sat Aug 15 21:07:03 PDT 1998 by heydon     *)

MODULE Check;
IMPORT Err, Out, Text, Command, Scanner, Formatter, Tree;

  REVEAL

    Env =
      Tree.Env BRANDED OBJECT END;

    TypeDefEnv =
      TypeDefEnvBase BRANDED OBJECT END;

    TypeEnv =
      TypeEnvBase BRANDED OBJECT END;

    TermEnv =
      TermEnvBase BRANDED OBJECT END;

  TYPE
    QuantifierSubtyping = {LeastBound, TopBound, EqualBounds};

  VAR traceType, traceSubtype, traceTerm: BOOLEAN;
  VAR quantifierSubtyping: QuantifierSubtyping;

PROCEDURE NewTypeDefEnv(name: Tree.IdeName; 
    bound, type: Type; rest: Env): Env RAISES ANY =
  VAR env: Env;
  BEGIN
    env := NEW(TypeDefEnv, bound:=bound, type:=type);
    Tree.BeEnv(env, name, rest);
    RETURN env;
  END NewTypeDefEnv;

PROCEDURE NewTypeEnv(name: Tree.IdeName; bound: Type; rest: Env)
  : Env RAISES ANY =
  VAR env: Env;
  BEGIN
    env := NEW(TypeEnv, bound:=bound);
    Tree.BeEnv(env, name, rest);
    RETURN env;
  END NewTypeEnv;

PROCEDURE NewTermEnv(name: Tree.IdeName; type: Type; rest: Env)
  : Env RAISES ANY =
  VAR env: Env;
  BEGIN
    env := NEW(TermEnv, type:=type);
    Tree.BeEnv(env, name, rest);
    RETURN env;
  END NewTermEnv;

  PROCEDURE Setup() RAISES ANY =
    BEGIN 
      topEnv := NIL;
      traceType:=FALSE;
      traceSubtype:=FALSE;
      traceTerm:=FALSE;
      Command.Register(
        NEW(Command.T, name:="TraceType", 
  	Exec:=PrintTraceType));
      Command.Register(
        NEW(Command.T, name:="TraceSubtype", 
  	Exec:=PrintTraceSubtype));
      Command.Register(
        NEW(Command.T, name:="TraceTerm", 
  	Exec:=PrintTraceTerm));
      quantifierSubtyping:=QuantifierSubtyping.LeastBound;
      Command.Register(
        NEW(Command.T, name:="QuantifierSubtyping",
  	Exec:=SetQuantifierSubtyping));
    END Setup;

  PROCEDURE AddUniVar(name: Tree.IdeName; VAR (*in-out*) uniVars: Tree.Env)
    RAISES ANY =
  VAR scan: Tree.Env;
  BEGIN
    scan:=uniVars;
    LOOP
      IF scan=NIL THEN
	uniVars := Tree.NewEnv(name, uniVars);
	RETURN;
      END;
      IF Tree.SameIdeName(name, scan.name) THEN RETURN
      ELSE scan := scan.rest;
      END;
    END;
  END AddUniVar;

  PROCEDURE PrintUniVar(fmt: Formatter.T; name: Tree.IdeName; 
    rank: INTEGER; VAR(*in-out*) uniVars: Tree.Env) RAISES ANY =
  BEGIN
    AddUniVar(name, (*in-out*)uniVars);
    Tree.PrintIde(fmt, name, rank, uniVars);
  END PrintUniVar;

  PROCEDURE PrintTraceType(self: Command.T; arg: TEXT) RAISES ANY =
    BEGIN
      IF Text.Equal(arg, "?") THEN
	Formatter.PutText(Out.out, self.name & " {On Off} is ");
	IF traceType THEN Formatter.PutText(Out.out, "On\n");
	ELSE Formatter.PutText(Out.out, "Off\n"); END;
      ELSIF Text.Equal(arg, "On") THEN traceType:=TRUE;
      ELSIF Text.Equal(arg, "Off") THEN traceType:=FALSE;
      ELSE
	Formatter.PutText(Out.out, "Command " & self.name 
	  & ": bad argument: " & arg & "\n");
      END;
    END PrintTraceType;

  PROCEDURE PrintTraceSubtype(self: Command.T; arg: TEXT) RAISES ANY =
    BEGIN
      IF Text.Equal(arg, "?") THEN
	Formatter.PutText(Out.out, self.name & " {On Off} is ");
	IF traceSubtype THEN Formatter.PutText(Out.out, "On\n");
	ELSE Formatter.PutText(Out.out, "Off\n"); END;
      ELSIF Text.Equal(arg, "On") THEN traceSubtype:=TRUE;
      ELSIF Text.Equal(arg, "Off") THEN traceSubtype:=FALSE;
      ELSE
	Formatter.PutText(Out.out, "Command " & self.name 
	  & ": bad argument: " & arg & "\n");
      END;
    END PrintTraceSubtype;

  PROCEDURE PrintTraceTerm(self: Command.T; arg: TEXT) RAISES ANY =
    BEGIN
      IF Text.Equal(arg, "?") THEN
	Formatter.PutText(Out.out, self.name & " {On Off} is ");
	IF traceTerm THEN Formatter.PutText(Out.out, "On\n");
	ELSE Formatter.PutText(Out.out, "Off\n"); END;
      ELSIF Text.Equal(arg, "On") THEN traceTerm:=TRUE;
      ELSIF Text.Equal(arg, "Off") THEN traceTerm:=FALSE;
      ELSE
	Formatter.PutText(Out.out, "Command " & self.name 
	  & ": bad argument: " & arg & "\n");
      END;
    END PrintTraceTerm;

  PROCEDURE SetQuantifierSubtyping(self: Command.T; arg: TEXT) RAISES ANY =
    BEGIN
      IF Text.Equal(arg, "?") THEN
	Formatter.PutText(Out.out, self.name & 
          " {LeastBound TopBound EqualBounds} is ");
	IF quantifierSubtyping = QuantifierSubtyping.LeastBound
        THEN Formatter.PutText(Out.out, "LeastBound\n");
	ELSIF quantifierSubtyping = QuantifierSubtyping.TopBound
        THEN Formatter.PutText(Out.out, "TopBound\n");
	ELSE Formatter.PutText(Out.out, "EqualBounds\n"); 
        END;
      ELSIF Text.Equal(arg, "LeastBound") THEN 
        quantifierSubtyping:=QuantifierSubtyping.LeastBound;
      ELSIF Text.Equal(arg, "TopBound") THEN 
        quantifierSubtyping:=QuantifierSubtyping.TopBound;
      ELSIF Text.Equal(arg, "EqualBounds") THEN 
        quantifierSubtyping:=QuantifierSubtyping.EqualBounds;
      ELSE
	Formatter.PutText(Out.out, "Command " & self.name 
	  & ": bad argument: " & arg & "\n");
      END;
    END SetQuantifierSubtyping;

  PROCEDURE TypeError(msg: TEXT; type: Type; env:Env; subst: Subst)
    RAISES ANY =
    VAR info: Err.LocationInfo; uniVars: Tree.Env;
    BEGIN
      uniVars:=NIL;
      Scanner.CurrentLocationInfo((*out*)info);
      Formatter.Begin(Out.out, 2);
        Formatter.PutText(Out.out, "Type error. ");
        Formatter.PutText(Out.out, msg); 
      IF type#NIL THEN
      Formatter.UnitedBreak(Out.out);
	Formatter.Begin(Out.out, 2);
          PrintType(Out.out, type, env, subst, (*in-out*)uniVars);
          Formatter.PutText(Out.out, "  ");
	Formatter.UnitedBreak(Out.out);
	  Err.PrintLocation(Out.out, type.location, info.line);
	Formatter.End(Out.out);
      END;
      Formatter.End(Out.out);
      Formatter.NewLine(Out.out);
      Formatter.Begin(Out.out, 2);
        Formatter.PutText(Out.out, "Error detected ");
        Err.PrintLocation(Out.out, Err.NewLineLocation(info), info.line);
      Formatter.End(Out.out);
      Formatter.NewLine(Out.out);
      Err.Raise();
    END TypeError;

  PROCEDURE SubtypeError(msg: TEXT; type1, type2: Type; env: Env; 
      subst: Subst) RAISES ANY =
    VAR info: Err.LocationInfo; uniVars: Tree.Env;
    BEGIN
      uniVars:=NIL;
      Scanner.CurrentLocationInfo((*out*)info);
      Formatter.Begin(Out.out, 2);
        Formatter.PutText(Out.out, "Subtype error. "); 
        Formatter.PutText(Out.out, msg); 
      Formatter.UnitedBreak(Out.out);
	Formatter.Begin(Out.out, 2);
          Formatter.PutText(Out.out, "#1: ");
          PrintType(Out.out, type1, env, subst, (*in-out*)uniVars);
	  Formatter.PutText(Out.out, "  ");
        Formatter.UnitedBreak(Out.out);
	  Err.PrintLocation(Out.out, type1.location, info.line);
	Formatter.End(Out.out);
      Formatter.UnitedBreak(Out.out);
	Formatter.Begin(Out.out, 2);
          Formatter.PutText(Out.out, "#2: ");
          PrintType(Out.out, type2, env, subst, (*in-out*)uniVars); 
	  Formatter.PutText(Out.out, "  ");
        Formatter.UnitedBreak(Out.out);
	  Err.PrintLocation(Out.out, type2.location, info.line);
	Formatter.End(Out.out);
      Formatter.End(Out.out);
      Formatter.NewLine(Out.out);
      Formatter.Begin(Out.out, 2);
        Formatter.PutText(Out.out, "Error detected ");
        Err.PrintLocation(Out.out, Err.NewLineLocation(info), info.line);
      Formatter.End(Out.out);
      Formatter.NewLine(Out.out);
      Err.Raise();
    END SubtypeError;

PROCEDURE PrintSomething(something: ROOT; env: Env; subst: Subst;
    VAR(*in-out*)uniVars: Tree.Env) RAISES ANY =
  BEGIN
    uniVars:=NIL;
    TYPECASE something OF
    | Type(type) => PrintType(Out.out, type, env, subst, (*in-out*)uniVars);
    | Tree.Type(type) => Tree.PrintType(Out.out, type, env);
    | Tree.Term(term) => Tree.PrintTerm(Out.out, term, env);
    ELSE Formatter.PutText(Out.out, "<?>");
    END;
  END PrintSomething;

PROCEDURE TraceEnter(proc: TEXT; one, two: ROOT; env: Env; subst: Subst) RAISES ANY =
  VAR uniVars: Tree.Env;
  BEGIN
    uniVars:=NIL;
    Formatter.Begin(Out.out, 2);
      Formatter.PutText(Out.out, "==> " & proc & " ");
      IF one#NIL THEN 
        Formatter.UnitedBreak(Out.out);
	PrintSomething(one, env, subst, (*in-out*)uniVars);
      END;
      IF two#NIL THEN 
	Formatter.PutText(Out.out, " "); 
        Formatter.UnitedBreak(Out.out);
	PrintSomething(two, env, subst, (*in-out*)uniVars); 
      END;
      Formatter.NewLine(Out.out);
  END TraceEnter;

PROCEDURE TraceExit(proc: TEXT; one, two: ROOT; env: Env; subst: Subst) RAISES ANY =
  VAR uniVars: Tree.Env;
  BEGIN
      uniVars:=NIL;
      Formatter.PutText(Out.out, "<== " & proc);
      IF one#NIL THEN 
        Formatter.UnitedBreak(Out.out);
	PrintSomething(one, env, subst, (*in-out*)uniVars);
      END;
      IF two#NIL THEN 
	Formatter.PutText(Out.out, " "); 
        Formatter.UnitedBreak(Out.out);
	PrintSomething(two, env, subst, (*in-out*)uniVars); 
      END;
    Formatter.End(Out.out);
    Formatter.NewLine(Out.out);
  END TraceExit;

PROCEDURE Lift(type: Type; lift, limit: INTEGER): Type RAISES ANY =
  BEGIN
    IF lift=0 THEN RETURN type END;
    TYPECASE type OF
    | TypeUniVar =>
	(* we are in the context of some substitution *)
	RETURN type;
    | TypeIde(node) =>
	IF node.index > limit THEN
	  RETURN NEW(TypeIde, location:=node.location, tag:=node.tag,
	    name:=node.name, index:=node.index+lift);
	ELSE
	  RETURN type;
	END;
    | TypeTop => RETURN type;
    | TypeArrow(node) =>
	RETURN NEW(TypeArrow, location:=node.location, tag:=node.tag,
	  dom:=Lift(node.dom, lift, limit),
	  rng:=Lift(node.rng, lift, limit+1));
    | TypeForall(node) =>
	RETURN NEW(TypeForall, location:=node.location, tag:=node.tag,
	  binder:=node.binder, omit:=node.omit,
	  bound:=Lift(node.bound, lift, limit),
	  body:=Lift(node.body, lift, limit+1));
    | TypeRec(node) =>
	RETURN NEW(TypeRec, location:=node.location, tag:=node.tag,
	  binder:=node.binder, 
	  body:=Lift(node.body, lift, limit+1));
    ELSE
      <*NOWARN*> Err.Fault(Out.out, "Lift");
    END;
  END Lift;

  PROCEDURE Retrieve(name: Tree.IdeName; subst: Subst; 
      VAR(*out*) type: Type; VAR(*out*) rank: INTEGER): BOOLEAN RAISES ANY =
    VAR shift: INTEGER;
    BEGIN
      shift := 0;
      LOOP
        TYPECASE subst OF <*NOWARN*>
        | NULL => 
	    Err.Fault(Out.out, "Check.Retrieve, unknown: " & 
	      Tree.FmtIdeName(name, NIL));
        | SubstType(node) =>
	    IF Tree.SameIdeName(name, node.name) THEN
	      type := Lift(node.type, shift, 0);
	      RETURN TRUE;
	    ELSE 
	      subst:=node.rest;
	    END;
	| SubstRank(node) =>
	    IF Tree.SameIdeName(name, node.name) THEN
	      rank := node.rank+shift;
	      RETURN FALSE;
	    ELSE 
	      subst:=node.rest;
	    END;
	| SubstShift(node) =>
	    shift := shift+node.shift;
	    subst:=node.rest;
	END;
      END;
    END Retrieve;

  PROCEDURE SetRank(name: Tree.IdeName; rank: INTEGER; subst: Subst) 
    RAISES ANY =
    VAR shift: INTEGER;
    BEGIN
      shift := 0;
      LOOP
        TYPECASE subst OF <*NOWARN*>
        | NULL => 
	    Err.Fault(Out.out, "Check.SetRank, unknown: " & 
	      Tree.FmtIdeName(name, NIL));
        | SubstType(node) =>
	    subst:=node.rest;
	| SubstRank(node) =>
	    IF Tree.SameIdeName(name, node.name) THEN
	      node.rank := rank-shift;
	      RETURN;
	    ELSE 
	      subst:=node.rest;
	    END;
	| SubstShift(node) =>
	    shift := shift+node.shift;
	    subst:=node.rest;
	END;
      END;
    END SetRank;

  PROCEDURE RemoveRank(name: Tree.IdeName; subst: Subst): Subst RAISES ANY =
    VAR curr,prev: Subst;
    BEGIN
      curr := subst;
      prev := NIL;
      LOOP
        TYPECASE curr OF <*NOWARN*>
        | NULL => 
	    Err.Fault(Out.out, "Check.RemoveRank, unknown: " & 
	      Tree.FmtIdeName(name, NIL));
        | SubstType(node) =>
	    prev:=curr;
	    curr:=node.rest;
	| SubstRank(node) =>
	    IF Tree.SameIdeName(name, node.name) THEN
	      IF prev=NIL THEN RETURN curr.rest
	      ELSE prev.rest := curr.rest; RETURN subst;
	      END;
	    ELSE 
	      prev:=curr;
	      curr:=node.rest;
	    END;
	| SubstShift(node) =>
	    prev:=curr;
	    curr:=node.rest;
	END;
      END;
    END RemoveRank;

PROCEDURE OccurCheck(var: TypeUniVar; rank1: INTEGER; initType,type: Type; 
    env: Env; subst: Subst; level: INTEGER:=0) RAISES ANY =
  VAR instType: Type; rank2: INTEGER;
  BEGIN
    TYPECASE type OF <*NOWARN*>
    | TypeUniVar(node) =>
	IF Retrieve(node.name, subst, (*out*)instType, (*out*)rank2) THEN
	  OccurCheck(var, rank1, initType, instType, env, subst, level);
	ELSIF Tree.SameIdeName(var.name, node.name) THEN	  
          TypeError("Type inference occur check: instantiation type for " 
	    & Tree.FmtIdeName(var.name, env) 
	    & " already contains " & Tree.FmtIdeName(var.name, env) & ": ", 
	   initType, env, subst);
	ELSIF rank2<rank1 THEN SetRank(node.name, rank1, subst);
	END;
    | TypeIde(node) =>
	IF (node.index>level) AND (node.index<rank1) THEN
          TypeError("Type inference rank check: instantiation type for " 
	    & Tree.FmtIdeName(var.name, env) 
	    & " contains a (different) variable " 
	    & Tree.FmtIdeName(node.name, env)
	    & " that is bound deeper than the "
	    & Tree.FmtIdeName(var.name, env) & " binder: ", 
	    initType, env, subst);
	END;
    | TypeTop =>
    | TypeArrow(node) =>
        OccurCheck(var, rank1, initType, node.dom, env, subst, level);
        OccurCheck(var, rank1+1, initType, node.rng, 
	  NewTermEnv(Tree.noName, node.dom, env),
	  NEW(SubstShift, shift:=1, rest:=subst), level+1);
    | TypeForall(node) =>
        OccurCheck(var, rank1, initType, node.bound, env, subst, level);
        OccurCheck(var, rank1+1, initType, node.body, 
	  NewTypeEnv(node.binder, node.bound, env),
	  NEW(SubstShift, shift:=1, rest:=subst), level+1);	
    | TypeRec(node) =>
        OccurCheck(var, rank1+1, initType, node.body, 
	  NewTypeEnv(node.binder,
	    NEW(TypeTop, location:=node.location), env),
	  NEW(SubstShift, shift:=1, rest:=subst), level+1);	
    END;
  END OccurCheck;

PROCEDURE Instantiate(var: TypeUniVar; rank: INTEGER; type: Type; env: Env;
    VAR (*in-out*)subst: Subst) RAISES ANY =
  BEGIN
    OccurCheck(var, rank, type, type, env, subst);
    subst:= 
      NEW(SubstType, name:=var.name, type:=type, 
	  rest:=RemoveRank(var.name, subst));
  END Instantiate;

PROCEDURE LookupTypeIde(name: Tree.IdeName; index: INTEGER; env: Env)
    : Type RAISES ANY =
  VAR i: INTEGER;
  BEGIN
    i := index;
    LOOP
      IF i<=0 THEN 
	Err.Fault(Out.out, "LookupTypeIde: " & Tree.FmtIde(name, index, env));
      END;
      TYPECASE env OF
      | NULL => 
	Err.Fault(Out.out, "LookupTypeIde: " & Tree.FmtIde(name, index, env));
      | TypeDefEnv(node) =>
	  env:=node.rest;
      | TypeEnv(node) =>
	  IF i=1 THEN
	    RETURN Lift(node.bound, index, 0);
	  ELSE
	    DEC(i);
	    env:=node.rest;
	  END;
      | TermEnv(node) =>
	  DEC(i);
	  env:=node.rest;
      ELSE Err.Fault(Out.out, "LookupTypeIde");
      END;
    END;
  END LookupTypeIde;

PROCEDURE LookupTermIde(name: Tree.IdeName; index: INTEGER; env: Env)
    : Type RAISES ANY =
  VAR i: INTEGER;
  BEGIN
    i := index;
    LOOP
      IF i<=0 THEN 
	Err.Fault(Out.out, "LookupTermIde: " & Tree.FmtIde(name, index, env));
      END;
      TYPECASE env OF
      | NULL => 
	Err.Fault(Out.out, "LookupTermIde: " & Tree.FmtIde(name, index, env));
      | TypeDefEnv(node) =>
	  env:=node.rest;
      | TypeEnv(node) =>
	  DEC(i);
	  env:=node.rest;
      | TermEnv(node) =>
	  IF i=1 THEN
	    RETURN Lift(node.type, index, 0);
	  ELSE
	    DEC(i);
	    env:=node.rest;
	  END;
      ELSE Err.Fault(Out.out, "LookupTermIde");
      END;
    END;
  END LookupTermIde;

VAR varCounter: INTEGER:=0;

PROCEDURE Strip(type: Type; VAR (*in-out*)subst: Subst;
    VAR (*in-out*)omitCount: INTEGER): Type RAISES ANY =
  VAR res, instType: Type; rank: INTEGER;
  BEGIN
    TYPECASE type OF
    | TypeUniVar(node) =>
	IF Retrieve(node.name, subst, (*out*)instType, (*out*)rank) THEN
	  RETURN Strip(instType, (*in-out*)subst, (*in-out*)omitCount);
	ELSE RETURN type
	END;
    | TypeForall(node) =>
	IF node.omit THEN 
	  res :=
	    Schemate(node.body, node.binder, node.bound, (*in-out*)subst);
	  INC(omitCount);
	  RETURN Strip(res, (*in-out*)subst, (*in-out*)omitCount);
	ELSE RETURN type
	END;
    ELSE RETURN type;
    END;
  END Strip;

PROCEDURE ExposeArrow(type: Type; env: Env; 
    VAR (*in-out*) subst: Subst): TypeArrow RAISES ANY =
  VAR domVar, rngVar: TypeUniVar; arrowType: TypeArrow;
    instType: Type; rank: INTEGER;
  BEGIN
    TYPECASE type OF
    | TypeUniVar(node) =>
	IF Retrieve(node.name, subst, (*out*)instType, (*out*)rank) THEN
	  RETURN ExposeArrow(instType, env, (*in-out*)subst);
	ELSE
          INC(varCounter);
          domVar :=
	    NEW(TypeUniVar, name:=NEW(Tree.IdeName, 
	      text:=node.name.text&"dom?", variant:=varCounter,
              absoluteEnvIndex:=-1));
          subst:=NEW(SubstRank, name:=domVar.name, rank:=rank, rest:=subst);
          INC(varCounter);
          rngVar :=
	    NEW(TypeUniVar, name:=NEW(Tree.IdeName, 
	      text:=node.name.text&"rng?", variant:=varCounter,
              absoluteEnvIndex:=-1));
          subst:=NEW(SubstRank, name:=domVar.name, rank:=rank, rest:=subst);
	  arrowType := NEW(TypeArrow, location:=node.location, tag:=node.tag,
	    	     dom:=domVar, rng:=rngVar);
	  subst := NEW(SubstType, name:=node.name, type:=arrowType,
		rest:=RemoveRank(node.name, subst));
	  RETURN arrowType;
	END;
    | TypeIde(ideType) => 
	RETURN ExposeArrow(LookupTypeIde(ideType.name, ideType.index, env), 
	  env, (*in-out*)subst)
    | TypeArrow(arrowType) => RETURN arrowType;
    ELSE
      <*NOWARN*> TypeError("\'->\' type expected: ", type, env, subst);
    END;
  END ExposeArrow;

PROCEDURE ExposeForall(type: Type; env: Env; 
    VAR (*in-out*) subst: Subst): TypeForall RAISES ANY =
  VAR boundVar, bodyVar: TypeUniVar; forallType: TypeForall;
    instType: Type; rank: INTEGER;
  BEGIN
    TYPECASE type OF
    | TypeUniVar(node) =>
	IF Retrieve(node.name, subst, (*out*)instType, (*out*)rank) THEN
	  RETURN ExposeForall(instType, env, (*in-out*)subst);
	ELSE
	  (* expose a non-question-mark forall type; maybe question mark
	     itself should be a variable to be instantiated later *)
          INC(varCounter);
          boundVar :=
	    NEW(TypeUniVar, name:=NEW(Tree.IdeName, 
	      text:=node.name.text&"bnd?", variant:=varCounter,
              absoluteEnvIndex:=-1));
          subst:=NEW(SubstRank, name:=boundVar.name, rank:=rank, rest:=subst);
          INC(varCounter);
          bodyVar :=
	    NEW(TypeUniVar, name:=NEW(Tree.IdeName, 
	      text:=node.name.text&"bdy?", variant:=varCounter,
              absoluteEnvIndex:=-1));
          subst:=NEW(SubstRank, name:=bodyVar.name, rank:=rank, rest:=subst);
	  forallType := NEW(TypeForall, location:=node.location, tag:=node.tag,
	    bound:=boundVar, body:=bodyVar);
	  subst := NEW(SubstType, name:=node.name, type:=forallType,
		rest:=RemoveRank(node.name, subst));
	  RETURN forallType;
	END;
    | TypeIde(ideType) => 
	RETURN ExposeForall(LookupTypeIde(ideType.name, ideType.index, env), 
	  env, (*in-out*)subst)
    | TypeForall(forallType) => RETURN forallType;
    ELSE
      <*NOWARN*> TypeError("\'All\' type expected: ", type, env, subst);
    END;
  END ExposeForall;

PROCEDURE ExposeRec(type: Type; env: Env; 
    VAR (*in-out*) subst: Subst): TypeRec RAISES ANY =
  VAR bodyVar: TypeUniVar; recType: TypeRec;
    instType: Type; rank: INTEGER;
  BEGIN
    TYPECASE type OF
    | TypeUniVar(node) =>
	IF Retrieve(node.name, subst, (*out*)instType, (*out*)rank) THEN
	  RETURN ExposeRec(instType, env, (*in-out*)subst);
	ELSE
          INC(varCounter);
          bodyVar :=
	    NEW(TypeUniVar, name:=NEW(Tree.IdeName, 
	      text:=node.name.text&"rec?", variant:=varCounter,
              absoluteEnvIndex:=-1));
          subst:=NEW(SubstRank, name:=bodyVar.name, rank:=rank, rest:=subst);
	  recType := NEW(TypeRec, location:=node.location, tag:=node.tag,
	    body:=bodyVar);
	  subst := NEW(SubstType, name:=node.name, type:=recType,
		rest:=RemoveRank(node.name, subst));
	  RETURN recType;
	END;
    | TypeIde(ideType) => 
	RETURN ExposeRec(LookupTypeIde(ideType.name, ideType.index, env), 
	  env, (*in-out*)subst)
    | TypeRec(recType) => RETURN recType
    ELSE
      <*NOWARN*> TypeError("\'Rec\' type expected: ", type, env, subst);
    END;
  END ExposeRec;

PROCEDURE Schemate(type: Type; binder: Tree.IdeName ; bound: Type;
    VAR (*in-out*) subst: Subst): Type RAISES ANY =
  VAR  newVar: TypeUniVar;
  BEGIN
    IF ISTYPE(bound, TypeTop) THEN
      INC(varCounter);
      newVar :=
	NEW(TypeUniVar, name:=NEW(Tree.IdeName, 
	  text:=binder.text&"?", variant:=varCounter,
          absoluteEnvIndex:=-1));
      subst := NEW(SubstRank, name:=newVar.name, rank:=1, rest:=subst);
      RETURN Replace(type, 1, newVar);
    ELSE
      RETURN Replace(type, 1, bound);
    END;
  END Schemate;

PROCEDURE Replace(type: Type; index: INTEGER; withType: Type)
    : Type RAISES ANY =
  BEGIN
    TYPECASE type OF
    | TypeUniVar =>
	(* this variable could not depend on index, so it stays as it is *)
	RETURN type;
    | TypeIde(node) =>
	IF node.index=index THEN 
	  RETURN Lift(withType, index-1, 0);
	ELSIF node.index>index THEN
	  RETURN NEW(TypeIde, location:=node.location, tag:=node.tag,
	    name:=node.name, index:=node.index-1);
	ELSE RETURN type;
	END;
    | TypeTop => RETURN type;
    | TypeArrow(node) =>
	RETURN NEW(TypeArrow, location:=node.location, tag:=node.tag,
	  dom:=Replace(node.dom, index, withType),
	  rng:=Replace(node.rng, index+1, withType));
    | TypeForall(node) =>
	RETURN NEW(TypeForall, location:=node.location, tag:=node.tag,
	  binder:=node.binder, omit:=node.omit,
	  bound:=Replace(node.bound, index, withType),
	  body:=Replace(node.body, index+1, withType));
    | TypeRec(node) =>
	RETURN NEW(TypeRec,
	  location:=node.location, tag:=node.tag, binder:=node.binder,
	  body:=Replace(node.body, index+1, withType));
    ELSE
      <*NOWARN*> Err.Fault(Out.out, "Replace");
    END;
  END Replace;

TYPE 
  Constraints = SET OF Constraint;
  Constraint = {Pos, Neg};

PROCEDURE Constr(type1, type2: Type; env: Env; index: INTEGER;
  covariant: BOOLEAN; VAR (*in-out*)constraints: Constraints) RAISES ANY =
VAR ide1: TypeIde; newEnv, bodyEnv: Env;
  recConstraints: Constraints;
BEGIN
  IF type1=type2 THEN
  ELSIF ISTYPE(type2, TypeTop) THEN
  ELSIF ISTYPE(type1, TypeUniVar) THEN
  ELSIF ISTYPE(type2, TypeUniVar) THEN
  ELSIF ISTYPE(type1, TypeIde) AND ISTYPE(type2, TypeIde) AND
      (NARROW(type1, TypeIde).index = NARROW(type2, TypeIde).index) THEN
    IF index=NARROW(type1, TypeIde).index THEN
      IF covariant THEN 
	constraints := constraints + Constraints{Constraint.Pos};
      ELSE 
	constraints := constraints + Constraints{Constraint.Neg};
      END;
    END;
  ELSIF ISTYPE(type1, TypeIde) THEN
    ide1 := NARROW(type1, TypeIde);
    Constr(LookupTypeIde(ide1.name, ide1.index, env), 
      type2, env, index, covariant, (*in-out*)constraints);
  ELSE
    TYPECASE type2 OF
    | TypeIde => (*SKIP*)
    | TypeArrow(node2) =>
        TYPECASE type1 OF
	| TypeArrow(node1) =>
	    Constr(node2.dom, node1.dom, env, index,
	      NOT covariant, (*in-out*)constraints);
	    Constr(node1.rng, node2.rng,
	      NewTermEnv(Tree.noName, node2.dom, env),
	      index+1, covariant, (*in-out*)constraints);
	ELSE
	END;
    | TypeForall(node2) =>
        TYPECASE type1 OF
	| TypeForall(node1) =>
	    IF node1.omit#node2.omit THEN RETURN END;
	    Constr(node2.bound, node1.bound, env, index,
	      NOT covariant, (*in-out*)constraints);
	    IF quantifierSubtyping=QuantifierSubtyping.EqualBounds THEN 
	      Constr(node1.bound, node2.bound, env, index,
	        NOT covariant, (*in-out*)constraints);
	    END;
            IF quantifierSubtyping=QuantifierSubtyping.TopBound THEN
              bodyEnv := NewTypeEnv(node2.binder, 
                NEW(TypeTop, location:=node2.location), env);
            ELSE
              bodyEnv := NewTypeEnv(node2.binder, node2.bound, env);
            END;
	    Constr(node1.body, node2.body, bodyEnv,
	      index+1, covariant, (*in-out*)constraints);
	ELSE 
	END;
    | TypeRec(node2) =>
        TYPECASE type1 OF
	| TypeRec(node1) =>
	    newEnv := NewTypeEnv(node1.binder, 
	      NEW(TypeTop, location:=node1.location), env);
	    Constr(node1.body, node2.body, newEnv,
	      index+1, covariant, (*in-out*)constraints);
	    recConstraints := Constraints{};
	    Constr(node1.body, node2.body, newEnv,
	      1, TRUE, (*in-out*)recConstraints);
	    IF Constraint.Neg IN recConstraints THEN
	      IF constraints # Constraints{} THEN 
	        constraints := Constraints{Constraint.Pos, Constraint.Neg};
	      END;
	    END;
	ELSE 
	END;
    ELSE 
    END;
  END;
END Constr;

PROCEDURE SubType(type1, type2: Type; env: Env; VAR(*in-out*)subst: Subst)
  RAISES ANY =
VAR instType1, instType2: Type; rank1, rank2: INTEGER; 
  ide1: TypeIde; constraints: Constraints; bodyEnv: Env;
BEGIN
  IF traceSubtype THEN TraceEnter("SubType", type1, type2, env, subst); END;
  IF type1=type2 THEN
  ELSIF ISTYPE(type2, TypeTop) THEN
  ELSIF ISTYPE(type1, TypeUniVar) AND ISTYPE(type2, TypeUniVar) AND
      Tree.SameIdeName(
	NARROW(type1, TypeUniVar).name, 
	NARROW(type2, TypeUniVar).name) THEN
  ELSIF ISTYPE(type1, TypeUniVar) THEN
    IF Retrieve(NARROW(type1, TypeUniVar).name, subst, 
	(*out*)instType1, (*out*)rank1)
    THEN SubType(instType1, type2, env, (*in-out*)subst)
    ELSE 
      Instantiate(NARROW(type1, TypeUniVar), rank1, type2, env, 
	(*in-out*)subst)
    END;
  ELSIF ISTYPE(type2, TypeUniVar) THEN
    IF Retrieve(NARROW(type2, TypeUniVar).name, subst, 
	(*out*)instType2, (*out*)rank2)
    THEN SubType(type1, instType2, env, (*in-out*)subst)
    ELSE
      Instantiate(NARROW(type2, TypeUniVar), rank2, type1, env, 
	(*in-out*)subst)
    END;
  ELSIF ISTYPE(type1, TypeIde) AND ISTYPE(type2, TypeIde) AND
    (NARROW(type1, TypeIde).index = NARROW(type2, TypeIde).index) THEN
  ELSIF ISTYPE(type1, TypeIde) THEN
    ide1 := NARROW(type1, TypeIde);
    SubType(LookupTypeIde(ide1.name, ide1.index, env), 
      type2, env, (*in-out*) subst);
  ELSE
    TYPECASE type2 OF
    | TypeIde =>
	SubtypeError("Expecting #1 <: #2:", type1, type2, env,subst);
    | TypeArrow(node2) =>
        TYPECASE type1 OF
	| TypeArrow(node1) =>
	    SubType(node2.dom, node1.dom, env, (*in-out*) subst);
	    subst:=NEW(SubstShift, shift:=1, rest:=subst);
	    SubType(node1.rng, node2.rng,
	      NewTermEnv(Tree.noName, node2.dom, env),
	      (*in-out*) subst);
	    subst:=NEW(SubstShift, shift:=-1, rest:=subst);
	ELSE SubtypeError("Expecting #1 <: #2:", type1, type2, env,subst);
	END;
    | TypeForall(node2) =>
        TYPECASE type1 OF
	| TypeForall(node1) =>
	    IF node1.omit#node2.omit THEN
	      SubtypeError("Expecting #1 <: #2:", type1, type2, env, subst);
	    END;
	    SubType(node2.bound, node1.bound, env, (*in-out*) subst);
	    IF quantifierSubtyping=QuantifierSubtyping.EqualBounds THEN 
	      SubType(node1.bound, node2.bound, env, (*in-out*) subst);
	    END;
	    subst:=NEW(SubstShift, shift:=1, rest:=subst);
            IF quantifierSubtyping=QuantifierSubtyping.TopBound THEN
              bodyEnv := NewTypeEnv(node2.binder, 
                NEW(TypeTop, location:=node2.location), env);
            ELSE
              bodyEnv := NewTypeEnv(node2.binder, node2.bound, env);
            END;
	    SubType(node1.body, node2.body, bodyEnv, (*in-out*) subst);
	    subst:=NEW(SubstShift, shift:=-1, rest:=subst);
	ELSE SubtypeError("Expecting #1 <: #2:", type1, type2, env, subst);
	END;
    | TypeRec(node2) =>
        TYPECASE type1 OF
	| TypeRec(node1) =>
	    constraints := Constraints{};
	    Constr(node1.body, node2.body, env, 1, TRUE, 
	      (*in-out*) constraints);
	    subst:=NEW(SubstShift, shift:=1, rest:=subst);
	    SubType(node1.body, node2.body, 
	      NewTypeEnv(node1.binder, NEW(TypeTop, location:=node1.location),
		env), (*in-out*) subst);
	    IF Constraint.Neg IN constraints THEN
	      SubType(node2.body, node1.body, 
	        NewTypeEnv(node2.binder, NEW(TypeTop,location:=node2.location),
		  env), (*in-out*) subst);
	    END;
	    subst:=NEW(SubstShift, shift:=-1, rest:=subst);
	ELSE SubtypeError("Expecting #1 <: #2:", type1, type2, env, subst);
	END;
    ELSE Err.Fault(Out.out, "SubType");
    END;
  END;
  IF traceSubtype THEN TraceExit("SubType", type1, type2, env, subst); END;
END SubType;

PROCEDURE CheckTypeBinding(binding: Tree.TypeBinding; env: Env;
    VAR(*in-out*)subst: Subst): Env RAISES ANY =
  VAR nfBound, nfType: Type;
  BEGIN
    TYPECASE binding OF
    | NULL => RETURN env;
    | Tree.TypeBinding(node) =>
	CheckType(node.bound, env, (*out*)nfBound);
	CheckType(node.type, env, (*out*)nfType);
	SubType(nfType, nfBound, env, (*in-out*)subst);
	RETURN CheckTypeBinding(node.rest, 
	  NewTypeDefEnv(node.binder, nfBound, nfType, env),
	  (*in-out*)subst);
    END;
  END CheckTypeBinding;

PROCEDURE CheckTermBinding(binding: Tree.TermBinding; env: Env;
    VAR(*in-out*)subst: Subst): Env RAISES ANY =
  VAR nfBound, nfType: Type;
  BEGIN
    TYPECASE binding OF
    | NULL => RETURN env;
    | Tree.TermBinding(node) =>
	IF node.bound=NIL THEN nfBound:=NIL;
	ELSE CheckType(node.bound, env, (*out*)nfBound);
	END;
	nfType := CheckTerm(node.term, env, (*in-out*)subst);
	IF nfBound=NIL THEN nfBound := nfType;
	ELSE SubType(nfType, nfBound, env, (*in-out*)subst);
	END;
	subst:=NEW(SubstShift, shift:=1, rest:=subst);
	RETURN CheckTermBinding(node.rest, 
	  NewTermEnv(node.binder, nfBound, env),
	  (*in-out*)subst);
    END;
  END CheckTermBinding;

PROCEDURE ExpandType(type, initType: Type; subst: Subst): Type RAISES ANY =
  VAR instType: Type; rank: INTEGER;
  BEGIN
    TYPECASE type OF
    | TypeUniVar(node) =>
	IF Retrieve(node.name, subst, (*out*)instType, (*out*)rank) THEN
	  RETURN ExpandType(instType, initType, subst);
	ELSE TypeError("Type inference: variable " 
	  & Tree.FmtIdeName(node.name, NIL)
	  & " is still unresolved at the top-level", initType, NIL, subst);
	END;
    | TypeIde => RETURN type;
    | TypeTop => RETURN type;
    | TypeArrow(node) =>
	RETURN NEW(TypeArrow, location:=node.location, tag:=node.tag,
	  dom:=ExpandType(node.dom, initType, subst),
	  rng:=ExpandType(node.rng, initType,
	   NEW(SubstShift, shift:=1, rest:=subst)));
    | TypeForall(node) =>
	RETURN NEW(TypeForall, location:=node.location, tag:=node.tag,
	  binder:=node.binder, omit:=node.omit,
	  bound:=ExpandType(node.bound, initType, subst),
	  body:=ExpandType(node.body, initType,
	   NEW(SubstShift, shift:=1, rest:=subst)));
    | TypeRec(node) =>
	RETURN NEW(TypeRec, location:=node.location, tag:=node.tag,
	  binder:=node.binder, 
	  body:=ExpandType(node.body, initType,
	   NEW(SubstShift, shift:=1, rest:=subst)));
    ELSE
      <*NOWARN*> Err.Fault(Out.out, "ExpandType");
    END;
  END ExpandType;

PROCEDURE ExpandTermEnv(env, endEnv: Env; subst: Subst): Env RAISES ANY =
  BEGIN
    IF env=endEnv THEN RETURN env END;
    TYPECASE env OF
    | TermEnv(node) =>
        RETURN
          NewTermEnv(node.name, ExpandType(node.type, node.type, subst),
	    ExpandTermEnv(node.rest, endEnv, 
	      NEW(SubstShift, shift:=-1, rest:=subst)));
    ELSE
      <*NOWARN*> Err.Fault(Out.out, "ExpandTermEnv");
    END;
  END ExpandTermEnv;

PROCEDURE Contractive(type: Type; index: INTEGER): BOOLEAN RAISES ANY =
  BEGIN
    TYPECASE type OF
    | TypeUniVar => 
	(* It doesn't matter if it is instantiated or not *)
	RETURN TRUE;
    | TypeIde(node) => RETURN node.index # index;
    | TypeTop => RETURN TRUE;
    | TypeArrow => RETURN TRUE;
    | TypeForall => RETURN TRUE;
    | TypeRec(node) =>
	RETURN Contractive(node.body, 1)
	  AND Contractive(node.body, index+1);
    ELSE
      <*NOWARN*> Err.Fault(Out.out, "Contractive");
    END;
  END Contractive;

(* This just copies a Tree.Type into a Type, but we pass along the
   env so we trace. *)
PROCEDURE CheckType(type: Tree.Type; env: Env; VAR(*out*) nfType:Type) 
    RAISES ANY =
  VAR nfDom, nfRng, nfBound, nfBody: Type;
  BEGIN
    IF traceType THEN TraceEnter("CheckType", type, NIL, env, NIL); END;
    TYPECASE type OF
    | Tree.TypeIde(node) =>
	(* redundant because of the scoping checks already done.
	nfBound := LookupTypeIde(node.name, node.index, env); *)
	nfType := NEW(TypeIde, location:=node.location, tag:=node.tag,
	  name:=node.name, index:=node.index);
    | Tree.TypeTop(node) =>
	nfType := NEW(TypeTop, location:=node.location, tag:=node.tag);
    | Tree.TypeArrow(node) =>
	CheckType(node.dom, env, (*out*)nfDom);
	CheckType(node.rng, 
	  NewTermEnv(Tree.noName, nfDom, env),
	  (*out*)nfRng);
	nfType := NEW(TypeArrow, location:=node.location, tag:=node.tag,
	  dom:=nfDom, rng:=nfRng);
    | Tree.TypeForall(node) =>
	CheckType(node.bound, env, (*out*)nfBound);
	CheckType(node.body,
	  NewTypeEnv(node.binder, nfBound, env),
	  (*out*)nfBody);
	nfType := NEW(TypeForall, location:=node.location, tag:=node.tag,
	  binder:=node.binder, omit:=node.omit, bound:=nfBound, body:=nfBody);
    | Tree.TypeRec(node) =>
	CheckType(node.body,
	  NewTypeEnv(node.binder, nfBound, env),
	  (*out*)nfBody);
	nfType := NEW(TypeRec, location:=node.location, tag:=node.tag,
	  binder:=node.binder, body:=nfBody);
	IF NOT Contractive(nfBody, 1) THEN
	  TypeError("Recursive type is not contractive: ", 
	    nfType, env, NIL);
	END;
    ELSE Err.Fault(Out.out, "CheckType");
    END;
    IF traceType THEN TraceExit("CheckType", nfType, NIL, env, NIL); END;
  END CheckType;

PROCEDURE CheckTerm(term: Tree.Term; env: Env; 
    VAR (*in-out*) subst: Subst): Type RAISES ANY =
  VAR nfDom, nfRng, nfBound, nfBody, nfType, nfArg, nfArgType, res:  Type;
    nfArrow: TypeArrow; nfForall: TypeForall; nfRec: TypeRec; newEnv: Env;
  BEGIN
    IF traceTerm THEN TraceEnter("CheckTerm", term, NIL, env, NIL); END;
    TYPECASE term OF
    | Tree.TermIde(node) =>
	res := LookupTermIde(node.name, node.index, env);
        IF node.omitArgs THEN 
	  res := Strip(res, (*in-out*)subst, (*in-out*)node.omitCount);
	END;
    | Tree.TermTop(node) =>
	res := NEW(TypeTop, location:=node.location);
    | Tree.TermFun(node) =>
	CheckType(node.bound, env, (*out*)nfDom);
	subst:=NEW(SubstShift, shift:=1, rest:=subst);
	nfRng := CheckTerm(node.body, 
	  NewTermEnv(node.binder, nfDom, env), (*in-out*)subst);
	subst:=NEW(SubstShift, shift:=-1, rest:=subst);
	res := NEW(TypeArrow, location:=node.location, dom:=nfDom, rng:=nfRng);
    | Tree.TermFun2(node) =>
	CheckType(node.bound, env, (*out*)nfBound);
	subst:=NEW(SubstShift, shift:=1, rest:=subst);
	nfBody := CheckTerm(node.body, 
	  NewTypeEnv(node.binder, nfBound, env), (*in-out*)subst);
	subst:=NEW(SubstShift, shift:=-1, rest:=subst);
	res := NEW(TypeForall, location:=node.location, 
	  binder:=node.binder, omit:=node.omit, bound:=nfBound, body:=nfBody);
    | Tree.TermAppl(node) =>
	nfType := CheckTerm(node.fun, env, (*in-out*) subst);
	nfArrow := ExposeArrow(nfType, env, (*in-out*) subst);
	nfDom := CheckTerm(node.arg, env, (*in-out*) subst);
	SubType(nfDom, nfArrow.dom, env, (*in-out*) subst);
	res := Replace(nfArrow.rng, 1, NIL); (* to shift down *)
    | Tree.TermAppl2(node) =>
	nfType := CheckTerm(node.fun, env, (*in-out*) subst);
	nfForall := ExposeForall(nfType, env, (*in-out*) subst);
	CheckType(node.arg, env, (*out*)nfArg);
	SubType(nfArg, nfForall.bound, env, (*in-out*) subst);
	res := Replace(nfForall.body, 1, nfArg);
    | Tree.TermFold(node) =>
	CheckType(node.recType, env, (*out*)nfType);
	nfRec := ExposeRec(nfType, env, (*in-out*) subst);
	nfType := Replace(nfRec.body, 1, nfRec);
	nfArgType := CheckTerm(node.arg, env, (*in-out*) subst);
	SubType(nfArgType, nfType, env, (*in-out*) subst);
	res := nfRec;
    | Tree.TermUnfold(node) =>
	nfType := CheckTerm(node.arg, env, (*in-out*) subst);
	nfRec := ExposeRec(nfType, env, (*in-out*) subst);
	res := Replace(nfRec.body, 1, nfRec);
    | Tree.TermRec(node) =>
	CheckType(node.bound, env, (*out*)nfDom);
	subst:=NEW(SubstShift, shift:=1, rest:=subst);
	newEnv := NewTermEnv(node.binder, nfDom, env);
	nfRng := CheckTerm(node.body, newEnv, (*in-out*)subst);
	SubType(nfRng, Lift(nfDom, 1, 0), newEnv, (*in-out*) subst);
	subst:=NEW(SubstShift, shift:=-1, rest:=subst);
	res := nfDom;
    ELSE Err.Fault(Out.out, "CheckTerm");
    END;
    IF traceTerm THEN TraceExit("CheckTerm", res, NIL, env, subst); END;
    RETURN res;
  END CheckTerm;

  PROCEDURE CheckContext(context: Tree.Context; env: Env): Env RAISES ANY =
    VAR nfBound, nfDom: Type;
    BEGIN
      TYPECASE context OF <*NOWARN*>
      | NULL => RETURN env;
      | Tree.ContextType(node) =>
	CheckType(node.bound, env, (*out*)nfBound);
	RETURN CheckContext(node.rest,
	  NewTypeEnv(node.binder, nfBound, env));
      | Tree.ContextTerm(node) =>
	CheckType(node.type, env, (*out*)nfDom);
	RETURN CheckContext(node.rest,
	  NewTermEnv(node.binder, nfDom, env));
      END;
    END CheckContext;

  PROCEDURE PrintTag(fmt: Formatter.T; name: Tree.IdeName) RAISES ANY =
  BEGIN
    Formatter.PutChar(fmt, '<');
    Formatter.PutText(fmt, name.text);
    Formatter.PutChar(fmt, '>');
  END PrintTag;

PROCEDURE PrintTypeIde(fmt: Formatter.T; name: Tree.IdeName; index: INTEGER;
  env: Env) RAISES ANY =
  VAR i: INTEGER;
  BEGIN
    i := index;
    LOOP
      IF i<=0 THEN 
	Formatter.Begin(fmt);
	Formatter.PutText(fmt, Tree.FmtIde(name, index, env) & "@");
	Formatter.End(fmt);
	RETURN;
      END;
      TYPECASE env OF
      | NULL => 
	Formatter.Begin(fmt);
	Formatter.PutText(fmt, Tree.FmtIde(name, index, env) & "@");
	Formatter.End(fmt);
	RETURN;
      | TypeDefEnv(node) =>
	  env:=node.rest;
      | TypeEnv(node) =>
	  IF i=1 THEN
	    IF Tree.SameIdeName(name, node.name) THEN
	      Tree.PrintIde(fmt, name, index, env);
	    ELSE
	      Formatter.Begin(fmt);
	      Formatter.PutText(fmt, Tree.FmtIde(name, index, env) & "(=");
	      Tree.PrintIde(fmt, node.name, index,env);
	      Formatter.PutChar(fmt, ')');
	      Formatter.End(fmt);
	    END;
            RETURN;
	  ELSE
	    DEC(i);
	    env:=node.rest;
	  END;
      | TermEnv(node) =>
	  DEC(i);
	  env:=node.rest;
      ELSE Err.Fault(Out.out, "PrintTypeIde");
      END;
    END;
  END PrintTypeIde;

  PROCEDURE PrintType(fmt: Formatter.T; type: Type; env: Env; subst: Subst;
    VAR(*in-out*) uniVars: Tree.Env) RAISES ANY =
  VAR newEnv: Env; instType: Type; rank: INTEGER;
  BEGIN
    IF type=NIL THEN Formatter.PutChar(fmt, '_'); RETURN END;
    IF type.tag#NIL THEN PrintTag(fmt, type.tag); RETURN END;
    TYPECASE type OF
    | TypeUniVar(node) =>
	IF Retrieve(node.name, subst, (*out*)instType, (*out*)rank) THEN
	  PrintType(fmt, instType, env, subst, (*in-out*)uniVars);
	ELSE	  
	  PrintUniVar(fmt, node.name, rank, (*in-out*)uniVars);
	END;
    | TypeIde(node) =>
	PrintTypeIde(fmt, node.name, node.index, env);
    | TypeTop =>
	Formatter.PutText(fmt, "Top");
    | TypeArrow(node) =>
	Formatter.Begin(fmt, 2);
          Formatter.PutChar(fmt, '{');
	  PrintType(fmt, node.dom, env, subst, (*in-out*)uniVars);
	  Formatter.PutText(fmt, "->");
	Formatter.UnitedBreak(fmt);
	  PrintType(fmt, node.rng, 
	    NewTermEnv(Tree.noName, node.dom, env),
	    NEW(SubstShift, shift:=1, rest:=subst), (*in-out*)uniVars);
          Formatter.PutChar(fmt, '}');
	Formatter.End(fmt);
    | TypeForall(node) =>
	Formatter.Begin(fmt, 2);
	  Formatter.Begin(fmt, 4);
            Formatter.PutText(fmt, "{All(");
	  Formatter.UnitedBreak(fmt);
	    Formatter.Begin(fmt, 2);
  	      newEnv := NewTypeEnv(node.binder, node.bound, env);
	      Tree.PrintIdeName(fmt, node.binder, newEnv);
	      IF node.omit THEN Formatter.PutChar(fmt, '?') END;
	      IF NOT ISTYPE(node.bound, TypeTop) THEN
	        Formatter.PutText(fmt, "<:");
	    Formatter.UnitedBreak(fmt);
	        PrintType(fmt, node.bound, env, subst, (*in-out*)uniVars);
	      END;
	    Formatter.End(fmt);
            Formatter.PutChar(fmt, ')');
	  Formatter.End(fmt);
	Formatter.UnitedBreak(fmt);
	  PrintType(fmt, node.body, newEnv,
	    NEW(SubstShift, shift:=1, rest:=subst), (*in-out*)uniVars);
          Formatter.PutChar(fmt, '}');
	Formatter.End(fmt);
    | TypeRec(node) =>
	Formatter.Begin(fmt, 2);
	  Formatter.Begin(fmt, 4);
            Formatter.PutText(fmt, "{Rec(");
  	    newEnv := NewTypeEnv(node.binder, 
		NEW(TypeTop, location:=node.location), env);
	    Tree.PrintIdeName(fmt, node.binder, newEnv);
            Formatter.PutChar(fmt, ')');
	  Formatter.End(fmt);
	Formatter.UnitedBreak(fmt);
	  PrintType(fmt, node.body, newEnv,
	    NEW(SubstShift, shift:=1, rest:=subst), (*in-out*)uniVars);
          Formatter.PutChar(fmt, '}');
	Formatter.End(fmt);
    ELSE
	Formatter.PutText(fmt, "<?>");
    END;
  END PrintType;

BEGIN
END Check.
