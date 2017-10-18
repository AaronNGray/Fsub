(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE Eval;
IMPORT Text, Err, Out, Tree, Check, Value;

  PROCEDURE Setup() RAISES ANY =
    BEGIN END Setup;

  PROCEDURE LookupVal(name: Tree.IdeName; index: INTEGER; env: Value.Env)
      : Value.Val RAISES ANY =
    VAR i: INTEGER; val: Value.Val;
    BEGIN
      i := index;
      LOOP
	IF i<0 THEN Err.Fault(Out.out, "Eval.LookupVal") END;
	TYPECASE env OF
	| NULL => 
	    Err.Fault(Out.out, "Unbound var: " 
	      & Tree.FmtIde(name, index, NIL));
	| Value.TypeDefEnv(node) =>
	    env := node.rest;
	| Value.TypeEnv(node) =>
	    DEC(i);
	    env := node.rest;
	| Value.TermEnv(node) =>
	    IF i=1 THEN
	      IF NOT Tree.SameIdeName(name, node.name) THEN
		Err.Fault(Out.out, "Eval.LookupVal");
	      END;
	      val := node.val; EXIT;
	    ELSE
	      DEC(i);
	      env := node.rest;
	    END;
	END;
      END;
      TYPECASE val OF
      | Value.ValSusp(node) => RETURN Term(node.term, node.env);
      ELSE RETURN val;
      END;
    END LookupVal;

  PROCEDURE TypeBinding(binding: Tree.TypeBinding; env: Value.Env)
    : Value.Env RAISES ANY =
    VAR val: Value.Val;
    BEGIN
      TYPECASE binding OF
      | NULL => RETURN env;
      | Tree.TypeBinding(node) =>
	  RETURN TypeBinding(node.rest, 
	    NEW(Value.TypeDefEnv, name:=node.binder, rest:=env));
      END;
    END TypeBinding;

  PROCEDURE TermBinding(binding: Tree.TermBinding; env: Value.Env)
    : Value.Env RAISES ANY =
    VAR val: Value.Val;
    BEGIN
      TYPECASE binding OF
      | NULL => RETURN env;
      | Tree.TermBinding(node) =>
	  val:=Term(node.term, env);
	  val.tag:=node.binder;
	  RETURN 
	    TermBinding(node.rest,
	      NEW(Value.TermEnv, name:=node.binder, val:=val, rest:=env));
      END;
    END TermBinding;

  PROCEDURE Strip(val: Value.Val; omitCount: INTEGER): Value.Val RAISES ANY =
    BEGIN
      WHILE omitCount>0 DO
        val := Apply2(val, NEW(Value.Set, type:=NIL, env:=NIL));
        DEC(omitCount);
      END;
      RETURN val;
    END Strip;

  PROCEDURE Term(term: Tree.Term; env: Value.Env): Value.Val RAISES ANY =
    VAR fun, arg: Value.Val; typeArg: Value.Set; val: Value.Val;
      recEnv: Value.Env; susp: Value.ValSusp;
    BEGIN
      TYPECASE term OF
      | NULL => Err.Fault(Out.out, "Eval.Term NIL");
      | Tree.TermIde(node) =>
	  val:= LookupVal(node.name, node.index, env);
	  IF node.omitArgs THEN val := Strip(val, node.omitCount); END;
	  RETURN val;
      | Tree.TermTop(node) =>
	  RETURN NEW(Value.ValTop);
      | Tree.TermFun(node) =>
	  RETURN NEW(Value.ValFun, fun:=node, env:=env);
      | Tree.TermAppl(node) =>
	  TYPECASE Term(node.fun, env) OF
	  | Value.ValFun(clos) =>
	    arg := Term(node.arg, env);
	    val:=
	      Term(clos.fun.body, 
	        NEW(Value.TermEnv, 
		  name:=clos.fun.binder,
		  val:=arg, 
		  rest:=clos.env));
	    RETURN val;
	  ELSE Err.Fault(Out.out, "Eval: application of a non-function");
	  END;
      | Tree.TermFun2(node) =>
	  RETURN NEW(Value.ValFun2, fun:=node, env:=env);
      | Tree.TermAppl2(node) =>
	  RETURN Apply2(Term(node.fun,env), 
	    NEW(Value.Set, type:=node.arg, env:=env));
      | Tree.TermFold(node) => RETURN Term(node.arg, env);
      | Tree.TermUnfold(node) => RETURN Term(node.arg, env);
      | Tree.TermRec(node) =>
	  susp := NEW(Value.ValSusp, term:=node.body, env:=NIL);
	  recEnv := 
	    NEW(Value.TermEnv, name:=node.binder, val:=susp, rest:=env);
	  susp.env := recEnv;
	  RETURN Term(node.body, recEnv);
      ELSE Err.Fault(Out.out, "Eval ?");
      END;
    END Term;

  PROCEDURE Apply2(fun: Value.Val; arg: Value.Set): Value.Val RAISES ANY =
    VAR typeArg: Value.Set;
    BEGIN
      TYPECASE fun OF
      | Value.ValFun2(clos) =>
	  RETURN
	    Term(clos.fun.body, 
	      NEW(Value.TypeEnv, 
		name:=clos.fun.binder,
		type:=arg,
		rest:=clos.env));
      ELSE Err.Fault(Out.out, "Eval: application of a non-function");
      END;
   END Apply2;

BEGIN
END Eval.

