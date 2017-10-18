(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

MODULE Main;
IMPORT Out, Rd, String, Text, TextRd, Err, Scanner, Formatter, FileStream, Fmt, 
  Parse, Gram, Act, Tree, Scope, Check, Value, Eval, Command, Frame;

CONST
  Version = 1; Enhancement = 5; BugFix = 0;

VAR showAfterParsing: BOOLEAN := FALSE;

PROCEDURE ShowAfterParsing(self: Command.T; arg: TEXT) RAISES ANY =
    BEGIN
      IF Text.Equal(arg, "?") THEN
	Formatter.PutText(Out.out, self.name & " {On Off} is ");
	IF showAfterParsing THEN Formatter.PutText(Out.out, "On");
	ELSE Formatter.PutText(Out.out, "Off"); END;
	Formatter.NewLine(Out.out);
      ELSIF Text.Equal(arg, "On") THEN showAfterParsing:=TRUE;
      ELSIF Text.Equal(arg, "Off") THEN showAfterParsing:=FALSE;
      ELSE
	Formatter.PutText(Out.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	Formatter.NewLine(Out.out);
      END;
    END ShowAfterParsing;

PROCEDURE ShowVersion(self: Command.T; arg: TEXT) RAISES ANY =
    BEGIN
      IF Text.Equal(arg, "?") THEN
	Formatter.PutText(Out.out, self.name & " "
	  & Fmt.Int(Version) & "." 
	  & Fmt.Int(Enhancement) & "."
	  & Fmt.Int(BugFix));
	Formatter.NewLine(Out.out);
      ELSE
	Formatter.PutText(Out.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	Formatter.NewLine(Out.out);
      END;
    END ShowVersion;

PROCEDURE DoIt() RAISES ANY =
  VAR
    phrase: Parse.Tree; type: Tree.Type;
    nfType, nfSubType, nfSuperType: Check.Type; 
    value: Value.Val; 
    scopeEnv: Scope.Env; checkEnv: Check.Env; 
    valueEnv: Value.Env;
    subst: Check.Subst; uniVars: Tree.Env; 
  BEGIN
    Frame.topPhraseGram := Tree.phrase;
    Frame.topPhraseEnv := Tree.env;
    Frame.SaveFrame("");
    Scanner.SetPrompt("- ", "  ");
    Frame.LoadFile("Startup.fsub", FALSE);
    LOOP
      TRY
        Scanner.FirstPrompt();
	phrase:=Parse.Read(Frame.topGram, Frame.topEnv);
	TYPECASE phrase OF
	| Frame.Command(node) =>
	    Command.Exec(node.name, node.arg);
	| Frame.Reload(node) =>
	    Frame.LoadFile(node.name);
	| Frame.Load(node) =>
	    Frame.ImportLoad(node.name);
	| Frame.Module(node) =>
	    Frame.ModuleFrame(node.name, node.imports);
	| Frame.Establish(node) =>
	    Frame.EstablishFrame(node.name);
	| Frame.Save(node) =>
	    Frame.SaveFrame(node.name);
	| Frame.Restore(node) =>
	    Frame.RestoreFrame(node.name);	    
	| Frame.None =>
          phrase:= Parse.Read(Frame.topPhraseGram, Frame.topPhraseEnv);
  	  TYPECASE phrase OF
	  | NULL =>
	  | Tree.Grammar(node) =>
	      IF node.gramInfo.adoptAsTopLevelGrammar THEN
		Frame.topPhraseGram := node.gramInfo.topGram;
		Frame.topPhraseEnv := node.gramInfo.env;
	      END;
	      Frame.SaveGramInfo(node.gramInfo);
	  | Tree.TypeBinding(binding) =>
	      scopeEnv := 
	        Scope.ScopeTypeBinding(binding, Scope.topEnv);
	      IF showAfterParsing THEN
		Formatter.Begin(Out.out, 2);
		Formatter.PutText(Out.out, "Parsed type binding: ");
		Formatter.UnitedBreak(Out.out);
	        Tree.PrintTypeBinding(Out.out, binding, Check.topEnv);
		Formatter.End(Out.out);
		Formatter.NewLine(Out.out);
	      END;
	      subst:=NIL;
	      checkEnv := Check.CheckTypeBinding(binding, Check.topEnv, 
		(*in-out*)subst);
	      valueEnv := Eval.TypeBinding(binding, Value.topEnv);
	      IF Scanner.TopLevel() THEN
		uniVars:=NIL;
	        Value.PrintTypeBinding(Out.out, checkEnv, Check.topEnv,
	          valueEnv, Value.topEnv, (*in-out*)uniVars);
	      END;
	      Scope.topEnv := scopeEnv;
	      Check.topEnv := checkEnv;
	      Value.topEnv := valueEnv;
	  | Tree.TermBinding(binding) =>
	      scopeEnv := 
	        Scope.ScopeTermBinding(binding, Scope.topEnv);
	      IF showAfterParsing THEN
		Formatter.Begin(Out.out, 2);
		Formatter.PutText(Out.out, "Parsed term binding: ");
		Formatter.UnitedBreak(Out.out);
	        Tree.PrintTermBinding(Out.out, binding, Check.topEnv);
		Formatter.End(Out.out);
		Formatter.NewLine(Out.out);
	      END;
	      subst:=NIL;
	      checkEnv := Check.CheckTermBinding(binding, Check.topEnv, 
		(*in-out*)subst);
	      checkEnv := Check.ExpandTermEnv(checkEnv, Check.topEnv, subst);
	      valueEnv := Eval.TermBinding(binding, Value.topEnv);
	      IF Scanner.TopLevel() THEN
		uniVars:=NIL;
	        Value.PrintTermBinding(Out.out, checkEnv, Check.topEnv,
	          valueEnv, Value.topEnv, subst, (*in-out*)uniVars);
	      END;
	      Scope.topEnv := scopeEnv;
	      Check.topEnv := checkEnv;
	      Value.topEnv := valueEnv;
	  | Tree.Type(type) =>
	      type := Scope.ScopeType(type, Scope.topEnv);
	      IF showAfterParsing THEN
		Formatter.Begin(Out.out, 2);
		Formatter.PutText(Out.out, "Parsed type: ");
		Formatter.UnitedBreak(Out.out);
	        Tree.PrintType(Out.out, type, Check.topEnv);
		Formatter.End(Out.out);
		Formatter.NewLine(Out.out);
	      END;
	      subst:=NIL;
	      Check.CheckType(type, Check.topEnv, (*out*)nfType);
	      IF Scanner.TopLevel() THEN
	        Formatter.Begin(Out.out);
	        Formatter.PutText(Out.out, ": ");
		uniVars:=NIL;
	        Check.PrintType(Out.out, nfType, Check.topEnv, subst,
		  (*out*)uniVars);
	        Formatter.End(Out.out);
	      END;
	  | Tree.Term(term) =>
	      Scope.ScopeTerm(term, Scope.topEnv);
	      IF showAfterParsing THEN
		Formatter.Begin(Out.out, 2);
		Formatter.PutText(Out.out, "Parsed term: ");
		Formatter.UnitedBreak(Out.out);
	        Tree.PrintTerm(Out.out, term, Check.topEnv);
		Formatter.End(Out.out);
		Formatter.NewLine(Out.out); 
	      END;
	      subst:=NIL; uniVars:=NIL;
	      nfType := Check.CheckTerm(term, Check.topEnv, (*in-out*)subst);
	      value := Eval.Term(term, Value.topEnv);
	      IF Scanner.TopLevel() THEN
	        Formatter.Begin(Out.out);
	          Value.PrintVal(Out.out, value, Check.topEnv);
	          Formatter.PutChar(Out.out, ' ');
	        Formatter.UnitedBreak(Out.out);
	          Formatter.PutText(Out.out, ": ");
		  uniVars:=NIL;
	          Check.PrintType(Out.out, nfType, Check.topEnv, subst,
		    (*in-out*)uniVars);
	        Formatter.End(Out.out);
	      END;
	  | Tree.JudgeContext(node) =>
	      scopeEnv := Scope.ScopeContext(node.context, Scope.topEnv);
	      subst:=NIL;
	      checkEnv := Check.CheckContext(node.context, Check.topEnv);
	      IF Scanner.TopLevel() THEN Formatter.PutText(Out.out,"ok"); END;
	  | Tree.JudgeType(node) =>
	      scopeEnv := Scope.ScopeContext(node.context, Scope.topEnv);
	      node.type := Scope.ScopeType(node.type, scopeEnv);
	      subst:=NIL;
	      checkEnv := Check.CheckContext(node.context, Check.topEnv);
	      Check.CheckType(node.type, checkEnv, (*out*)nfType);
	      IF Scanner.TopLevel() THEN Formatter.PutText(Out.out,"ok"); END;
	  | Tree.JudgeSubtype(node) =>
	      scopeEnv := Scope.ScopeContext(node.context, Scope.topEnv);
	      node.subType := Scope.ScopeType(node.subType, scopeEnv);
	      node.superType := Scope.ScopeType(node.superType, scopeEnv);
	      subst:=NIL;
	      checkEnv := Check.CheckContext(node.context, Check.topEnv);
	      Check.CheckType(node.subType, checkEnv, (*out*)nfSubType);
	      Check.CheckType(node.superType, checkEnv, (*out*)nfSuperType);
	      Check.SubType(nfSubType, nfSuperType, checkEnv, (*in-out*)subst);
	      IF Scanner.TopLevel() THEN Formatter.PutText(Out.out,"ok"); END;
	  | Tree.JudgeTerm(node) =>
	      scopeEnv := Scope.ScopeContext(node.context, Scope.topEnv);
	      node.type := Scope.ScopeType(node.type, scopeEnv);
	      Scope.ScopeTerm(node.term, scopeEnv);
	      subst:=NIL;
	      checkEnv := Check.CheckContext(node.context, Check.topEnv);
	      Check.CheckType(node.type, checkEnv, (*out*)nfSuperType);
	      nfSubType := 
		Check.CheckTerm(node.term, checkEnv, (*in-out*)subst);
	      Check.SubType(nfSubType, nfSuperType, checkEnv, (*in-out*)subst);
	      IF Scanner.TopLevel() THEN Formatter.PutText(Out.out,"ok"); END;
	  END;
	  Formatter.NewLine(Out.out);
        END;
	Formatter.Flush(Out.out);
      EXCEPT
      | Err.Fail => Formatter.Flush(Out.out);
      | Rd.EndOfFile => EXIT;
      END;
    END;
  END DoIt;

BEGIN
  Out.Setup();
  Err.Setup();
  Command.Setup();
  Scanner.Setup();
  Parse.Setup();
  Gram.Setup();
  Act.Setup();
  Tree.Setup();
  Scope.Setup();
  Check.Setup();
  Value.Setup();
  Eval.Setup();
  Frame.Setup();

    Command.Register(
      NEW(Command.T, name:="Version", 
	Exec:=ShowVersion));
    showAfterParsing := FALSE;
    Command.Register(
      NEW(Command.T, name:="ShowParsing", 
	Exec:=ShowAfterParsing));

  DoIt();

END Main.
