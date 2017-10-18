(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE Act;
IMPORT Formatter, Text, String, Out, Err, Fmt, Scan, Gram, Parse, Tree, 
  Command, Scope;

  TYPE
    Env =
      OBJECT 
        name: Tree.IdeName;
        rest: Env;
      END;

  PROCEDURE DefaultTree(location: Err.Location): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(Tree.TermTop, location:=location);
    END DefaultTree;

  PROCEDURE BuildActionIdentifier(self: Parse.Identifier; name: TEXT;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(Tree.TermTop, location:=Err.NewLineLocation(info));
    END BuildActionIdentifier;

  PROCEDURE BuildActionDelimiter(self: Parse.GivenDelimiter;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(Tree.TermTop, location:=Err.NewLineLocation(info));
    END BuildActionDelimiter;

  PROCEDURE BuildActionKeyword(self: Parse.GivenKeyword;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(Tree.TermTop, location:=Err.NewLineLocation(info));
    END BuildActionKeyword;

  PROCEDURE BuildActionInteger(self: Parse.Integer; int: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(Tree.TermTop, location:=Err.NewLineLocation(info));
    END BuildActionInteger;

  PROCEDURE BuildActionReal(self: Parse.Real; real: REAL;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(Tree.TermTop, location:=Err.NewLineLocation(info));
    END BuildActionReal;

  PROCEDURE BuildActionChar(self: Parse.QuotedChar; char: CHAR;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(Tree.TermTop, location:=Err.NewLineLocation(info));
    END BuildActionChar;

  PROCEDURE BuildActionString(self: Parse.QuotedString; string: String.T;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(Tree.TermTop, location:=Err.NewLineLocation(info));
    END BuildActionString;

  PROCEDURE CheckPatternIde(name: Tree.IdeName; env: Env) RAISES ANY =
  BEGIN
    LOOP
      IF env=NIL THEN
	(* Err.Msg(Out.out, "Free variable in action: "&name.text); *)
        Scope.SetAbsoluteIndex(name, Scope.topEnv);
        RETURN;
      END;
      IF Text.Equal(name.text, env.name.text) THEN RETURN
      ELSE env := env.rest;
      END;
    END;
  END CheckPatternIde;

  PROCEDURE LookupIde(name: Tree.IdeName; env: Env): Tree.IdeName RAISES ANY =
  BEGIN
    LOOP
      IF env=NIL THEN RETURN Tree.Copy(name) END;
      IF Text.Equal(name.text, env.name.text) THEN 
	RETURN Tree.Copy(env.name);
      ELSE env := env.rest;
      END;
    END;
  END LookupIde;

PROCEDURE Copy(tree: Parse.Tree; patEnv: Env): Parse.Tree RAISES ANY =
  VAR res: Parse.Tree; binder: Tree.IdeName;
  BEGIN
    TYPECASE tree OF
    | NULL => res := NIL;
    | Tree.TypeIde(node) =>
	res := 
	  NEW(Tree.TypeIde, tag:=node.tag, 
	    name:=LookupIde(node.name, patEnv),
	    index:=node.index);
    | Tree.TypePatternPosition(node) => 
	res :=NEW(Tree.TypePatternPosition, tag:=node.tag,
	  position:=node.position);
    | Tree.TypeTop(node) => res := NEW(Tree.TypeTop, tag:=node.tag);
    | Tree.TypeArrow(node) =>
	res := NEW(Tree.TypeArrow, tag:=node.tag, 
	  dom:=Copy(node.dom, patEnv), rng:=Copy(node.rng, patEnv));
    | Tree.TypeForall(node) =>
	binder := Tree.Copy(node.binder);
	res := NEW(Tree.TypeForall, tag:=node.tag, binder:=binder,
	  omit:=node.omit, bound:=Copy(node.bound, patEnv), 
	  body:=Copy(node.body, NEW(Env, name:=binder, rest:=patEnv)));
    | Tree.TypePatternForall(node) =>
	res := NEW(Tree.TypePatternForall, tag:=node.tag, 
	  position:=node.position,
	  omit:=node.omit, bound:=Copy(node.bound, patEnv), 
	  body:=Copy(node.body, patEnv));
    | Tree.TypeRec(node) =>
	binder := Tree.Copy(node.binder);
	res := NEW(Tree.TypeRec, tag:=node.tag, binder:=binder,
	  body:=Copy(node.body, NEW(Env, name:=binder, rest:=patEnv)));
    | Tree.TermIde(node) =>
	res := 
	  NEW(Tree.TermIde, name:=LookupIde(node.name, patEnv), 
	    index:=node.index,
	    omitArgs:=node.omitArgs, omitCount:=node.omitCount);
    | Tree.TermPatternPosition(node) => 
	res :=NEW(Tree.TermPatternPosition,position:=node.position);
    | Tree.TermTop => res := NEW(Tree.TermTop);
    | Tree.TermFun(node) =>
	binder := Tree.Copy(node.binder);
	res := NEW(Tree.TermFun, binder:=binder,
	  bound:=Copy(node.bound, patEnv), 
	  body:=Copy(node.body, NEW(Env, name:=binder, rest:=patEnv)));
    | Tree.TermPatternFun(node) =>
	res := NEW(Tree.TermPatternFun, position:=node.position,
	  bound:=Copy(node.bound, patEnv), 
	  body:=Copy(node.body, patEnv));
    | Tree.TermAppl(node) =>
	res := NEW(Tree.TermAppl, fun:=Copy(node.fun, patEnv), 
	    arg:=Copy(node.arg, patEnv));
    | Tree.TermFun2(node) =>
	binder := Tree.Copy(node.binder);
	res := NEW(Tree.TermFun2, binder:=binder, omit:=node.omit,
	  bound:=Copy(node.bound, patEnv), 
	  body:=Copy(node.body, NEW(Env, name:=binder, rest:=patEnv)));
    | Tree.TermPatternFun2(node) =>
	res := NEW(Tree.TermPatternFun2, position:=node.position, 
	  omit:=node.omit, bound:=Copy(node.bound, patEnv), 
	  body:=Copy(node.body, patEnv));
    | Tree.TermAppl2(node) =>
	res := NEW(Tree.TermAppl2, fun:=Copy(node.fun, patEnv), 
	  arg:=Copy(node.arg, patEnv));
    | Tree.TermFold(node) =>
	res := NEW(Tree.TermFold, recType:=Copy(node.recType, patEnv), 
	  arg:=Copy(node.arg, patEnv));
    | Tree.TermUnfold(node) =>
	res := NEW(Tree.TermUnfold, arg:=Copy(node.arg, patEnv));
    | Tree.TermRec(node) =>
	binder := Tree.Copy(node.binder);
	res := NEW(Tree.TermRec, binder:=binder,
	  bound:=Copy(node.bound, patEnv), 
	  body:=Copy(node.body, NEW(Env, name:=binder, rest:=patEnv)));
    ELSE Err.Fault(Out.out, "Act.Copy");
    END;
    IF res#NIL THEN res.location := tree.location END;
    RETURN res;
  END Copy;

  PROCEDURE FetchItem(index: INTEGER; base: INTEGER; patEnv: Env): Parse.Tree
    RAISES ANY =
    VAR res: Parse.Tree;
    BEGIN
      IF (index<0) THEN
	Err.Fault(Out.out, "Undefined pattern index: _"&Fmt.Int(index));
      END;
      res := Parse.Stack[base+index];
      IF res=NIL THEN
	Err.Fault(Out.out, "Undefined pattern index: _"&Fmt.Int(index));
      END;
      RETURN Copy(res, patEnv);
    END FetchItem;

  VAR variantCounter := 0;

  PROCEDURE CheckPattern(action: Parse.Tree) RAISES ANY =
    BEGIN
      TYPECASE action OF
      | NULL => Err.Fault(Out.out, "CheckPattern NIL");
      | Tree.Type(actionType) =>
	  CheckTypePattern(actionType, NIL);
      | Tree.Term(actionTerm) => 
	  CheckTermPattern(actionTerm, NIL);
      ELSE Err.Fault(Out.out, "CheckPattern");
      END;
    END CheckPattern;
    
  PROCEDURE CheckTermPattern(actionTerm: Tree.Term; ideEnv: Env) RAISES ANY =
    VAR tree: Parse.Tree; binder: Tree.IdeName;
    BEGIN
      TYPECASE actionTerm OF
      | NULL => Err.Fault(Out.out, "CheckTermPattern NIL");
      | Tree.TermIde(node) => 
	  CheckPatternIde(node.name, ideEnv);
      | Tree.TermPatternPosition(node) => 
      | Tree.TermTop(node) => 
      | Tree.TermFun(node) =>
	  INC(variantCounter);
	  binder :=
	    NEW(Tree.IdeName, location:=node.binder.location,
	      text:=node.binder.text, variant:=variantCounter,
              absoluteEnvIndex:=-1);
	  CheckTypePattern(node.bound, ideEnv);
	  CheckTermPattern(node.body,
	      NEW(Env, name:=binder, rest:=ideEnv));
      | Tree.TermPatternFun(node) =>
	  CheckTypePattern(node.bound, ideEnv);
	  CheckTermPattern(node.body, ideEnv);
      | Tree.TermAppl(node) =>
	  CheckTermPattern(node.fun, ideEnv); 
	  CheckTermPattern(node.arg, ideEnv);
      | Tree.TermFun2(node) =>
	  INC(variantCounter);
	  binder :=
	    NEW(Tree.IdeName, location:=node.binder.location,
	      text:=node.binder.text, variant:=variantCounter,
              absoluteEnvIndex:=-1);
          CheckTypePattern(node.bound, ideEnv);
	  CheckTermPattern(node.body,
	      NEW(Env, name:=binder, rest:=ideEnv));
      | Tree.TermPatternFun2(node) =>
	  CheckTypePattern(node.bound, ideEnv);
	  CheckTermPattern(node.body, ideEnv);
      | Tree.TermAppl2(node) =>
	CheckTermPattern(node.fun, ideEnv);
	CheckTypePattern(node.arg, ideEnv);
      | Tree.TermFold(node) =>
	CheckTypePattern(node.recType, ideEnv); 
	CheckTermPattern(node.arg, ideEnv);
      | Tree.TermUnfold(node) =>
	CheckTermPattern(node.arg, ideEnv);
      | Tree.TermRec(node) =>
	  INC(variantCounter);
	  binder :=
	    NEW(Tree.IdeName, location:=node.binder.location,
	      text:=node.binder.text, variant:=variantCounter,
              absoluteEnvIndex:=-1);
          CheckTypePattern(node.bound, ideEnv);
	  CheckTermPattern(node.body,
	      NEW(Env, name:=binder, rest:=ideEnv));
      END;
    END CheckTermPattern;

  PROCEDURE CheckTypePattern(actionType: Tree.Type; ideEnv: Env) RAISES ANY =
    VAR tree: Parse.Tree; binder: Tree.IdeName;
    BEGIN
      TYPECASE actionType OF
      | NULL => Err.Fault(Out.out, "CheckTypePattern NIL");
      | Tree.TypeIde(node) => 
	  CheckPatternIde(node.name, ideEnv);
      | Tree.TypePatternPosition(node) => 
      | Tree.TypeTop =>
      | Tree.TypeArrow(node) =>
	  CheckTypePattern(node.dom, ideEnv);
	  CheckTypePattern(node.rng, ideEnv);
      | Tree.TypeForall(node) =>
	  INC(variantCounter);
	  binder := 
	    NEW(Tree.IdeName, location:=node.binder.location,
	      text:=node.binder.text, variant:=variantCounter,
              absoluteEnvIndex:=-1);
 	    CheckTypePattern(node.bound, ideEnv);
	    CheckTypePattern(node.body,
	      NEW(Env, name:=binder, rest:=ideEnv));
      | Tree.TypePatternForall(node) =>
	   CheckTypePattern(node.bound, ideEnv);
	   CheckTypePattern(node.body, ideEnv);
      | Tree.TypeRec(node) =>
	  INC(variantCounter);
	  binder := 
	    NEW(Tree.IdeName, location:=node.binder.location,
	      text:=node.binder.text, variant:=variantCounter,
              absoluteEnvIndex:=-1);
	  CheckTypePattern(node.body,
	      NEW(Env, name:=binder, rest:=ideEnv));
      END;
    END CheckTypePattern;

  PROCEDURE InstantiatePattern(action: Parse.Tree;
	base: INTEGER): Parse.Tree RAISES ANY =
    VAR instance: Parse.Tree;
    BEGIN
      TYPECASE action OF
      | NULL => Err.Fault(Out.out, "InstantiatePattern NIL");
      | Tree.Type(actionType) =>
	  instance := InstantiateTypePattern(actionType, base, NIL, NIL);
          IF showExpansions THEN
	    Formatter.Begin(Out.out, 2);
	    Formatter.PutText(Out.out, "Expanded type: ");
	    Formatter.UnitedBreak(Out.out);
	    Tree.PrintType(Out.out, actionType, NIL);
	    Formatter.UnitedBreak(Out.out);
	    Formatter.PutText(Out.out, "  To: ");
	    Formatter.UnitedBreak(Out.out);
	    Tree.PrintType(Out.out, instance, NIL);
	    Formatter.End(Out.out);
	    Formatter.NewLine(Out.out);
          END;
      | Tree.Term(actionTerm) => 
	  instance := InstantiateTermPattern(actionTerm, base, NIL, NIL);
          IF showExpansions THEN
	    Formatter.Begin(Out.out, 2);
	    Formatter.PutText(Out.out, "Expanded term: ");
	    Formatter.UnitedBreak(Out.out);
	    Tree.PrintTerm(Out.out, actionTerm, NIL);
	    Formatter.UnitedBreak(Out.out);
	    Formatter.PutText(Out.out, "  To: ");
	    Formatter.UnitedBreak(Out.out);
	    Tree.PrintTerm(Out.out, instance, NIL);
	    Formatter.End(Out.out);
	    Formatter.NewLine(Out.out);
          END;
      ELSE Err.Fault(Out.out, "InstantiatePattern");
      END;
      RETURN instance;
    END InstantiatePattern;

  PROCEDURE InstantiateTermPattern(actionTerm: Tree.Term;
	base: INTEGER; ideEnv,patEnv: Env): Parse.Tree RAISES ANY =
    VAR tree: Parse.Tree; binder: Tree.IdeName;
    BEGIN
      TYPECASE actionTerm OF
      | NULL => Err.Fault(Out.out, "InstantiateTermPattern NIL");
      | Tree.TermIde(node) => 
	  RETURN NEW(Tree.TermIde, location:=node.location,
	    name:=LookupIde(node.name, ideEnv), 
	    index:=0);
      | Tree.TermPatternPosition(node) => 
	tree := FetchItem(node.position, base, patEnv);
	IF ISTYPE(tree, Tree.Term) THEN RETURN tree
	ELSE Err.Fault(Out.out, 
		"Pattern selector was not bound to a \'term\': _"
		& Fmt.Int(node.position));
	END;
      | Tree.TermTop(node) => 
	  RETURN NEW(Tree.TermTop, location:=node.location);
      | Tree.TermFun(node) =>
	  INC(variantCounter);
	  binder :=
	    NEW(Tree.IdeName, location:=node.binder.location,
	      text:=node.binder.text, variant:=variantCounter,
              absoluteEnvIndex:=-1);
          RETURN NEW(Tree.TermFun, location:=node.location,
	    binder:=binder, bound:=
	    InstantiateTypePattern(node.bound, base, ideEnv, patEnv),
	    body:=InstantiateTermPattern(node.body, base,
	      NEW(Env, name:=binder, rest:=ideEnv), patEnv));
      | Tree.TermPatternFun(node) =>
          TYPECASE FetchItem(node.position, base, NIL) OF
          | Tree.TermIde(ide) => 
	      INC(variantCounter);
	      binder :=
	        NEW(Tree.IdeName, location:=ide.location,
	         text:=ide.name.text, variant:=variantCounter,
                 absoluteEnvIndex:=-1);
          ELSE Err.Fault(Out.out, "Pattern binder was not a \'termIde\'");
          END;
          RETURN NEW(Tree.TermFun, location:=node.location,
	    binder:=binder, bound:=
	    InstantiateTypePattern(node.bound, base, ideEnv, patEnv),
	    body:=InstantiateTermPattern(node.body, base, ideEnv, 
	      NEW(Env, name:=binder, rest:=patEnv)));
      | Tree.TermAppl(node) =>
	RETURN NEW(Tree.TermAppl, location:=node.location,
	  fun:=InstantiateTermPattern(node.fun, base, ideEnv, patEnv), 
	  arg:=InstantiateTermPattern(node.arg, base, ideEnv, patEnv));
      | Tree.TermFun2(node) =>
	  INC(variantCounter);
	  binder :=
	    NEW(Tree.IdeName, location:=node.binder.location,
	      text:=node.binder.text, variant:=variantCounter,
              absoluteEnvIndex:=-1);
          RETURN NEW(Tree.TermFun2, location:=node.location,
	    binder:=binder, omit:=node.omit, bound:=
	    InstantiateTypePattern(node.bound, base, ideEnv, patEnv),
	    body:=InstantiateTermPattern(node.body, base,
	      NEW(Env, name:=binder, rest:=ideEnv), patEnv));
      | Tree.TermPatternFun2(node) =>
          TYPECASE FetchItem(node.position, base, NIL) OF
          |  Tree.TypeIde(ide) => 
	      INC(variantCounter);
	      binder :=
	        NEW(Tree.IdeName, location:=ide.location,
	         text:=ide.name.text, variant:=variantCounter,
                 absoluteEnvIndex:=-1);
          ELSE Err.Fault(Out.out, "Pattern binder was not a \'typeIde\'");
          END;
          RETURN NEW(Tree.TermFun2, location:=node.location,
	    binder:=binder, omit:=node.omit, bound:=
	    InstantiateTypePattern(node.bound, base, ideEnv, patEnv),
	    body:=InstantiateTermPattern(node.body, base, ideEnv,
	      NEW(Env, name:=binder, rest:=patEnv)));
      | Tree.TermAppl2(node) =>
	RETURN NEW(Tree.TermAppl2, location:=node.location,
	  fun:=InstantiateTermPattern(node.fun, base, ideEnv, patEnv), 
	  arg:=InstantiateTypePattern(node.arg, base, ideEnv, patEnv));
      | Tree.TermFold(node) =>
	RETURN NEW(Tree.TermFold, location:=node.location,
	  recType:=InstantiateTypePattern(node.recType, base, ideEnv, patEnv), 
	  arg:=InstantiateTermPattern(node.arg, base, ideEnv, patEnv));
      | Tree.TermUnfold(node) =>
	RETURN NEW(Tree.TermUnfold, location:=node.location,
	  arg:=InstantiateTermPattern(node.arg, base, ideEnv, patEnv));
      | Tree.TermRec(node) =>
	  INC(variantCounter);
	  binder :=
	    NEW(Tree.IdeName, location:=node.binder.location,
	      text:=node.binder.text, variant:=variantCounter,
              absoluteEnvIndex:=-1);
          RETURN NEW(Tree.TermRec, location:=node.location,
	    binder:=binder, bound:=
	    InstantiateTypePattern(node.bound, base, ideEnv, patEnv),
	    body:=InstantiateTermPattern(node.body, base,
	      NEW(Env, name:=binder, rest:=ideEnv), patEnv));
      END;
    END InstantiateTermPattern;

  PROCEDURE InstantiateTypePattern(actionType: Tree.Type;
	base: INTEGER; ideEnv, patEnv: Env): Parse.Tree RAISES ANY =
    VAR tree: Parse.Tree; binder: Tree.IdeName;
    BEGIN
      TYPECASE actionType OF
      | NULL => Err.Fault(Out.out, "InstantiateTypePattern NIL");
      | Tree.TypeIde(node) => 
	  RETURN NEW(Tree.TypeIde, location:=node.location,
	    name:=LookupIde(node.name, ideEnv), 
	    index:=0);
      | Tree.TypePatternPosition(node) => 
	  tree := FetchItem(node.position, base, patEnv);
	  IF ISTYPE(tree, Tree.Type) THEN RETURN tree
	  ELSE Err.Fault(Out.out, 
		"Pattern selector was not bound to a \'type\': _"
		& Fmt.Int(node.position));
	  END;
      | Tree.TypeTop => RETURN actionType;
      | Tree.TypeArrow(node) =>
	  RETURN NEW(Tree.TypeArrow, location:=node.location,
	    dom:=InstantiateTypePattern(node.dom, base, ideEnv, patEnv), 
	    rng:=InstantiateTypePattern(node.rng, base, ideEnv, patEnv));
      | Tree.TypeForall(node) =>
	  INC(variantCounter);
	  binder := 
	    NEW(Tree.IdeName, location:=node.binder.location,
	      text:=node.binder.text, variant:=variantCounter,
              absoluteEnvIndex:=-1);
          RETURN NEW(Tree.TypeForall, location:=node.location,
	    binder:=binder, omit:=node.omit, bound:=
	    InstantiateTypePattern(node.bound, base, ideEnv, patEnv),
	    body:=InstantiateTypePattern(node.body, base,
	      NEW(Env, name:=binder, rest:=ideEnv), patEnv));
      | Tree.TypePatternForall(node) =>
          TYPECASE FetchItem(node.position, base, NIL) OF
          | Tree.TypeIde(ide) => 
	      INC(variantCounter);
	      binder :=
	        NEW(Tree.IdeName, location:=ide.location,
	         text:=ide.name.text, variant:=variantCounter,
                 absoluteEnvIndex:=-1);
          ELSE Err.Fault(Out.out, "Pattern binder was not a \'typeIde\'");
          END;
          RETURN NEW(Tree.TypeForall, location:=node.location,
	    binder:=binder, omit:=node.omit, bound:=
	    InstantiateTypePattern(node.bound, base, ideEnv, patEnv),
	    body:=InstantiateTypePattern(node.body, base, ideEnv,
	      NEW(Env, name:=binder, rest:=patEnv)));
      | Tree.TypeRec(node) =>
	  INC(variantCounter);
	  binder := 
	    NEW(Tree.IdeName, location:=node.binder.location,
	      text:=node.binder.text, variant:=variantCounter,
              absoluteEnvIndex:=-1);
          RETURN NEW(Tree.TypeRec, location:=node.location,
	    binder:=binder, 
	    body:=InstantiateTypePattern(node.body, base,
	      NEW(Env, name:=binder, rest:=ideEnv), patEnv));
      END;
    END InstantiateTypePattern;

  VAR showExpansions: BOOLEAN;

PROCEDURE ShowExpansions(self: Command.T; arg: TEXT) RAISES ANY =
    BEGIN
      IF Text.Equal(arg, "?") THEN
	Formatter.PutText(Out.out, self.name & " {On Off} is ");
	IF showExpansions THEN Formatter.PutText(Out.out, "On");
	ELSE Formatter.PutText(Out.out, "Off"); END;
	Formatter.NewLine(Out.out);
      ELSIF Text.Equal(arg, "On") THEN showExpansions:=TRUE;
      ELSIF Text.Equal(arg, "Off") THEN showExpansions:=FALSE;
      ELSE
	Formatter.PutText(Out.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	Formatter.NewLine(Out.out);
      END;
    END ShowExpansions;

  PROCEDURE Setup() RAISES ANY =
  BEGIN
    showExpansions := FALSE;
    Command.Register(
      NEW(Command.T, name:="ShowExpansions", 
	Exec:=ShowExpansions));
  END Setup;

  PROCEDURE GetGrammars(
    VAR (*out*) actionTermGrammar, actionTypeGrammar: Parse.NonTerminal) 
    RAISES ANY =
  BEGIN
    Tree.GetGrammars((*out*)actionTermGrammar,
	(*out*)actionTypeGrammar);
  END GetGrammars;

BEGIN
END Act.
