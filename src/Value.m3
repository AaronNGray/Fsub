(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                             *)
(* Created by Luca Cardelli                                    *)
(* Last modified on Sat Aug 15 22:31:24 PDT 1998 by heydon     *)

MODULE Value;
IMPORT Err, Out, Formatter, Tree, Check;

  PROCEDURE Setup() =
    BEGIN
      topEnv := NIL;
    END Setup;

  PROCEDURE PrintTag(fmt: Formatter.T; name: Tree.IdeName) RAISES ANY =
  BEGIN
    Formatter.Begin(fmt);
    Formatter.PutChar(fmt, '<');
    Formatter.PutText(fmt, name.text);
    Formatter.PutChar(fmt, '>');
    Formatter.End(fmt);
  END PrintTag;

  PROCEDURE PrintVal(fmt: Formatter.T; val: Val; printEnv: Tree.Env)
    RAISES ANY =
  BEGIN
    IF val=NIL THEN Formatter.PutChar(fmt, '_'); RETURN END;
    IF val.tag#NIL THEN PrintTag(fmt, val.tag); RETURN END;
    TYPECASE val OF
    | ValTop => Formatter.PutText(fmt, "top");
    | ValFun(val) => PrintTerm(fmt, val.fun, val.env, printEnv);
    | ValFun2(val) => PrintTerm(fmt, val.fun, val.env, printEnv);
    | ValSusp => Formatter.PutText(fmt, "<recursive>"); (* ---- *)
    ELSE Formatter.PutText(fmt, "<?>");
    END;
  END PrintVal;

  PROCEDURE PrintSet(fmt: Formatter.T; set: Set; printEnv: Tree.Env)
    RAISES ANY =
  BEGIN
    PrintType(fmt, set.type, set.env, printEnv);
  END PrintSet;

PROCEDURE PrintIde(fmt: Formatter.T; name: Tree.IdeName; index: INTEGER; 
    env: Env; printEnv: Tree.Env) RAISES ANY =
  VAR i: INTEGER;
  BEGIN
    i := index;
    LOOP
      IF i<0 THEN Err.Fault(Out.out, "Value.PrintIde") END;
      TYPECASE env OF <*NOWARN*>
      | NULL => 
	  Err.Fault(Out.out, "PrintIde: " & Tree.FmtIde(name, index, NIL));
      | TypeDefEnv(node) =>
	  env:=node.rest;
      | TypeEnv(node) =>
	  IF i=1 THEN
            IF NOT Tree.SameIdeName(name, node.name) THEN 
	      Err.Fault(Out.out, "Value.PrintIde (type)");
	    END;
	    IF node.type=NIL THEN
	      Tree.PrintIde(fmt, name, index, printEnv);
	    ELSE
	      PrintSet(fmt, node.type, printEnv);
	    END;
	    EXIT;
	  ELSE
	    DEC(i);
	    env:=node.rest;
	  END;
      | TermEnv(node) =>
	  IF i=1 THEN
            IF NOT Tree.SameIdeName(name, node.name) THEN 
	      Err.Fault(Out.out, "Value.PrintIde (term)");
	    END;
	    IF node.val=NIL THEN
	      Tree.PrintIde(fmt, name, index, printEnv);
	    ELSE
	      PrintVal(fmt, node.val, printEnv);
	    END;
	    EXIT;
	  ELSE
	    DEC(i);
	    env:=node.rest;
	  END;
      END;
    END;
  END PrintIde;

PROCEDURE PrintTermBinding(fmt: Formatter.T;
    checkEnv, checkEnvStop: Check.Env; env, envStop: Env;
    subst: Check.Subst; VAR (*in-out*)uniVars: Tree.Env) RAISES ANY =
  BEGIN
    Formatter.Begin(fmt, 2);
    Formatter.PutText(Out.out, "let ");
    PrintTermBinding1(fmt, checkEnv, checkEnvStop, env, envStop, 
      subst, (*in-out*)uniVars);
    Formatter.End(fmt);
  END PrintTermBinding;

PROCEDURE PrintTermBinding1(fmt: Formatter.T;
    checkEnv, checkEnvStop: Check.Env; env, envStop: Env;
    subst: Check.Subst; VAR (*in-out*)uniVars: Tree.Env) RAISES ANY =
  BEGIN
    IF (checkEnv=checkEnvStop) AND (env=envStop) THEN RETURN END;
    IF (checkEnv=checkEnvStop) OR (env=envStop) OR
	NOT Tree.SameIdeName(checkEnv.name, env.name) THEN
	Err.Fault(Out.out, "Envs do not match. (1)");
    END;
    subst := NEW(Check.SubstShift, shift:=-1, rest:=subst);
    PrintTermBinding1(fmt, checkEnv.rest, checkEnvStop, 
	env.rest, envStop, subst, (*in-out*)uniVars);
    Formatter.UnitedBreak(fmt);
    TYPECASE checkEnv OF
    | Check.TermEnv(checkNode) =>
	TYPECASE env OF
	| TermEnv(valueNode) =>
	    Formatter.Begin(fmt, 2);
	      Formatter.Begin(fmt, 4);
	        Tree.PrintIdeName(fmt, checkNode.name, checkEnv);
                Formatter.PutText(fmt, " : ");
	      Formatter.UnitedBreak(fmt);
	        Check.PrintType(fmt, checkNode.type, checkNode.rest, subst, 
		  (*in-out*)uniVars);
                Formatter.PutText(fmt, " = ");
	      Formatter.End(fmt);
	    Formatter.UnitedBreak(fmt);
	      PrintVal(fmt, valueNode.val, checkNode.rest);
              Formatter.PutChar(fmt, ' ');
	    Formatter.End(fmt);
	ELSE Err.Fault(Out.out, "Envs do not match. (3)");
	END;
    ELSE Err.Fault(Out.out, "PrintTermBinding1");
    END;
  END PrintTermBinding1;

PROCEDURE PrintTypeBinding(fmt: Formatter.T;
    checkEnv, checkEnvStop: Check.Env; env, envStop: Env;
      VAR(*in-out*)uniVars: Tree.Env) RAISES ANY =
  BEGIN
    Formatter.Begin(fmt, 2);
    Formatter.PutText(Out.out, "Let ");
    PrintTypeBinding1(fmt, checkEnv, checkEnvStop, env, envStop, 
	(*in-out*)uniVars);
    Formatter.End(fmt);
  END PrintTypeBinding;

PROCEDURE PrintTypeBinding1(fmt: Formatter.T;
    checkEnv, checkEnvStop: Check.Env; env, envStop: Env;
    VAR(*in-out*)uniVars: Tree.Env) RAISES ANY =
  BEGIN
    IF (checkEnv=checkEnvStop) AND (env=envStop) THEN RETURN END;
    IF (checkEnv=checkEnvStop) OR (env=envStop) OR
	NOT Tree.SameIdeName(checkEnv.name, env.name) THEN
	Err.Fault(Out.out, "Envs do not match. (1)");
    END;
    PrintTypeBinding1(fmt, checkEnv.rest, checkEnvStop, 
	env.rest, envStop, (*in-out*)uniVars);
    Formatter.UnitedBreak(Out.out);
    TYPECASE checkEnv OF
    | Check.TypeDefEnv(checkNode) =>
	TYPECASE env OF
	| TypeDefEnv =>
	    Formatter.Begin(fmt, 2);
	      Formatter.Begin(fmt, 4);
	        Tree.PrintIdeName(fmt, checkNode.name, checkEnv);
                Formatter.PutText(fmt, " <: ");
	      Formatter.UnitedBreak(fmt);
	        Check.PrintType(fmt, checkNode.bound, checkNode.rest, NIL, 
		  (*in-out*)uniVars);
                Formatter.PutText(fmt, " = ");
	      Formatter.End(fmt);
	    Formatter.UnitedBreak(fmt);
	      Check.PrintType(fmt, checkNode.type, checkNode.rest, NIL, 
		(*in-out*)uniVars);
              Formatter.PutChar(fmt, ' ');
	    Formatter.End(fmt);
	ELSE Err.Fault(Out.out, "Envs do not match. (2)");
	END;
    ELSE Err.Fault(Out.out, "PrintTypeBinding1");
    END;
  END PrintTypeBinding1;

PROCEDURE PrintTerm(fmt: Formatter.T; term: Tree.Term; 
    env: Env; printEnv: Tree.Env) RAISES ANY =
  VAR newPrintEnv: Tree.Env;
  BEGIN
    TYPECASE term OF
    | NULL => Formatter.PutChar(fmt, '_');
    | Tree.TermIde(node) => 
        IF node.omitArgs THEN
	  PrintIde(fmt, node.name, node.index, env, printEnv);
	ELSE
  	  Formatter.Begin(fmt);
	  PrintIde(fmt, node.name, node.index, env, printEnv);
	  Formatter.PutChar(fmt, '!');
  	  Formatter.Begin(fmt);
	END;
    | Tree.TermTop => Formatter.PutText(fmt, "top");
    | Tree.TermFun(node) =>
	Formatter.Begin(fmt, 2);
	  Formatter.Begin(fmt, 4);
            Formatter.PutText(fmt, "{fun(");
	  Formatter.UnitedBreak(fmt);
	    Formatter.Begin(fmt, 2);
	      newPrintEnv := Tree.NewEnv(node.binder, printEnv);
	      Tree.PrintIdeName(fmt, node.binder, newPrintEnv);
	      Formatter.PutText(fmt, ":");
	    Formatter.UnitedBreak(fmt);
	      PrintType(fmt, node.bound, env, printEnv);
	    Formatter.End(fmt);
            Formatter.PutChar(fmt, ')');
	  Formatter.End(fmt);
	Formatter.UnitedBreak(fmt);
	  PrintTerm(fmt, node.body, 
	    NEW(TermEnv, name:=node.binder, val:=NIL, rest:=env),
	    newPrintEnv);
          Formatter.PutChar(fmt, '}');
	Formatter.End(fmt);
    | Tree.TermAppl(node) =>
	Formatter.Begin(fmt, 2);
	  PrintTerm(fmt, node.fun, env, printEnv);
	  Formatter.PutChar(fmt, '(');
	Formatter.UnitedBreak(fmt);
	  PrintTerm(fmt, node.arg, env, printEnv);
	  Formatter.PutChar(fmt, ')');
	Formatter.End(fmt);
    | Tree.TermFun2(node) =>
	Formatter.Begin(fmt, 2);
	  Formatter.Begin(fmt, 4);
            Formatter.PutText(fmt, "{fun(");
	  Formatter.UnitedBreak(fmt);
	    Formatter.Begin(fmt, 2);
	      newPrintEnv := Tree.NewEnv(node.binder, printEnv);
	      Tree.PrintIdeName(fmt, node.binder, newPrintEnv);
	      IF node.omit THEN Formatter.PutChar(fmt, '?') END;
	      IF NOT ISTYPE(node.bound, Tree.TypeTop) THEN
	        Formatter.PutText(fmt, "<:");
	    Formatter.UnitedBreak(fmt);
	        PrintType(fmt, node.bound, env, printEnv);
	      END;
	    Formatter.End(fmt);
            Formatter.PutChar(fmt, ')');
	  Formatter.End(fmt);
	Formatter.UnitedBreak(fmt);
	  PrintTerm(fmt, node.body, NEW(TypeEnv, name:=node.binder, rest:=env),
	    newPrintEnv);
          Formatter.PutChar(fmt, '}');
	Formatter.End(fmt);
    | Tree.TermAppl2(node) =>
	Formatter.Begin(fmt, 2);
	  PrintTerm(fmt, node.fun, env, printEnv);
	  Formatter.PutChar(fmt, '(');
	Formatter.UnitedBreak(fmt);
	  Formatter.PutChar(fmt, ':');
	  PrintType(fmt, node.arg, env, printEnv);
	  Formatter.PutChar(fmt, ')');
	Formatter.End(fmt);
    | Tree.TermFold(node) =>
	Formatter.Begin(fmt, 2);
	  Formatter.Begin(fmt, 4);
            Formatter.PutText(fmt, "fold(");
	  Formatter.UnitedBreak(fmt);
	    Formatter.PutText(fmt, ":");
	    PrintType(fmt, node.recType, env, printEnv);
            Formatter.PutChar(fmt, ')');
	  Formatter.End(fmt);
	Formatter.UnitedBreak(fmt);
	  Formatter.Begin(fmt, 4);
            Formatter.PutChar(fmt, '(');
	  Formatter.UnitedBreak(fmt);
	    PrintTerm(fmt, node.arg, env, printEnv);
            Formatter.PutChar(fmt, ')');
	  Formatter.End(fmt);
	Formatter.End(fmt);
    | Tree.TermUnfold(node) =>
	Formatter.Begin(fmt, 2);
          Formatter.PutText(fmt, "unfold(");
	Formatter.UnitedBreak(fmt);
	  PrintTerm(fmt, node.arg, env, printEnv);
          Formatter.PutChar(fmt, ')');
	Formatter.End(fmt);
    | Tree.TermRec(node) =>
	Formatter.Begin(fmt, 2);
	  Formatter.Begin(fmt, 4);
            Formatter.PutText(fmt, "{rec(");
	  Formatter.UnitedBreak(fmt);
	    Formatter.Begin(fmt, 2);
	      newPrintEnv := Tree.NewEnv(node.binder, printEnv);
	      Tree.PrintIdeName(fmt, node.binder, newPrintEnv);
	      Formatter.PutText(fmt, ":");
	    Formatter.UnitedBreak(fmt);
	      PrintType(fmt, node.bound, env, printEnv);
	    Formatter.End(fmt);
            Formatter.PutChar(fmt, ')');
	  Formatter.End(fmt);
	Formatter.UnitedBreak(fmt);
	  PrintTerm(fmt, node.body, 
	    NEW(TermEnv, name:=node.binder, val:=NIL, rest:=env),
	    newPrintEnv);
          Formatter.PutChar(fmt, '}');
	Formatter.End(fmt);
    ELSE Formatter.PutText(fmt, "<?>");
    END;
  END PrintTerm;

  PROCEDURE PrintType(fmt: Formatter.T; type: Tree.Type; 
    env: Env; printEnv: Tree.Env) RAISES ANY =
  VAR newPrintEnv: Tree.Env;
  BEGIN
    IF type=NIL THEN Formatter.PutChar(fmt, '_'); RETURN END;
    IF type.tag#NIL THEN PrintTag(fmt, type.tag); RETURN END;
    TYPECASE type OF
    | Tree.TypeIde(node) =>
	PrintIde(fmt, node.name, node.index, env, printEnv);
    | Tree.TypeTop => Formatter.PutText(fmt, "Top");
    | Tree.TypeArrow(node) =>
	Formatter.Begin(fmt, 2);
          Formatter.PutChar(fmt, '{');
  	  PrintType(fmt, node.dom, env, printEnv);
	  Formatter.PutText(fmt, "->");
	Formatter.UnitedBreak(fmt);
	  PrintType(fmt, node.rng, 
	    NEW(TermEnv, name:=Tree.noName, val:=NIL, rest:=env), 
	    printEnv);
          Formatter.PutChar(fmt, '}');
	Formatter.End(fmt);
    | Tree.TypeForall(node) =>
	Formatter.Begin(fmt, 2);
          Formatter.PutText(fmt, "{All(");
	  Formatter.Begin(fmt, 2);
	    newPrintEnv := Tree.NewEnv(node.binder, printEnv);
	    Tree.PrintIdeName(fmt, node.binder, newPrintEnv);
	    IF node.omit THEN Formatter.PutChar(fmt, '?') END;
	    IF NOT ISTYPE(node.bound, Tree.TypeTop) THEN
	      Formatter.PutText(fmt, "<:");
	  Formatter.UnitedBreak(fmt);
	      PrintType(fmt, node.bound, env, printEnv);
	    END;
	  Formatter.End(fmt);
          Formatter.PutChar(fmt, ')');
	Formatter.UnitedBreak(fmt);
	  PrintType(fmt, node.body, 
	    NEW(TypeEnv, name:=node.binder, type:=NIL, rest:=env),
	    newPrintEnv);
          Formatter.PutChar(fmt, '}');
	Formatter.End(fmt);
    | Tree.TypeRec(node) =>
	Formatter.Begin(fmt, 2);
          Formatter.PutText(fmt, "{Rec(");
	  newPrintEnv := Tree.NewEnv(node.binder, printEnv);
	  Tree.PrintIdeName(fmt, node.binder, newPrintEnv);
          Formatter.PutChar(fmt, ')');
	Formatter.UnitedBreak(fmt);
	  PrintType(fmt, node.body, 
	    NEW(TypeEnv, name:=node.binder, type:=NIL, rest:=env),
	    newPrintEnv);
          Formatter.PutChar(fmt, '}');
	Formatter.End(fmt);
    ELSE
	Formatter.PutText(fmt, "<?>");
    END;
  END PrintType;

BEGIN
END Value.
