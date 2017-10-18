(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                             *)
(* Created by Luca Cardelli                                    *)
(* Last modified on Sat Aug 15 22:46:18 PDT 1998 by heydon     *)

MODULE Tree;
IMPORT String, Err, Command, Out, Text, Formatter, Fmt, Scanner, Parse, Gram;

  REVEAL
    Parse.Tree =
      Parse.Located BRANDED OBJECT END;

    Env =
      EnvBase BRANDED OBJECT
        decoration: INTEGER;
      END;

  TYPE
    IntegerTemp =
      Parse.Tree BRANDED OBJECT
        int: INTEGER;
      END;
    StringTemp =
      Parse.Tree BRANDED OBJECT
        string: String.T;
      END;
    OmitParam = Parse.Tree BRANDED OBJECT END;

  VAR
    printDeBruijnIndex: BOOLEAN;
    printScopeLevel: BOOLEAN;
  VAR 
    keySet: Scanner.KeywordSet;
    keyHasType, keyHasSubtype,
    keyTypeTop, keyTypeArrow, keyTypeForall, keyTypeRec,
    keyTermTop, keyTermFun, keyTermFold, keyTermUnfold, keyTermRec,
    keyTermLet, keyTypeLet, keyEqDef,
    keyJudge, keyEntail: Scanner.Keyword;
    synTerm, synDecl, 
    phraseEmpty, phraseSyntax, phraseTypeBinding, 
    phraseTermBinding, phraseType, phraseTerm, phraseJudge, 
    phraseJudegeContext,
    phraseJudgeType, phraseJudgeSubtype, phraseJudgeTerm, context,
    binderIde, binderPos, typeBinding, termBinding, type, typeBase, 
    typeOper, typeIde, typePos,
    typeTop, typeArrow, typeForall, typeForallBind, typeRec, typeParen,
    term, termBase, termOper, termAppl, termIde, termPos, termTop, termFun, 
    termFunBind,
    termFunBindIde, termFunBindPos, termFunBindIde1, termFunBindIde2,
    termFunBindPos1, termFunBindPos2, termIterAppl, 
    termFold, termUnfold, termRec, termParen: Parse.NonTerminal;

  PROCEDURE Select1(<*UNUSED*> self: Parse.Action; base: INTEGER;
      <*UNUSED*> READONLY info: Err.LocationInfo): Parse.Tree =
    BEGIN
      RETURN Parse.Stack[base+1];
    END Select1;

  PROCEDURE Select2(<*UNUSED*> self: Parse.Action; base: INTEGER;
      <*UNUSED*> READONLY info: Err.LocationInfo): Parse.Tree =
    BEGIN
      RETURN Parse.Stack[base+2];
    END Select2;

  PROCEDURE Select3(<*UNUSED*> self: Parse.Action; base: INTEGER;
      <*UNUSED*> READONLY info: Err.LocationInfo): Parse.Tree =
    BEGIN
      RETURN Parse.Stack[base+3];
    END Select3;

  <*UNUSED*>
  PROCEDURE Select4(<*UNUSED*> self: Parse.Action; base: INTEGER;
      <*UNUSED*> READONLY info: Err.LocationInfo): Parse.Tree =
    BEGIN
      RETURN Parse.Stack[base+4];
    END Select4;

  PROCEDURE Select5(<*UNUSED*> self: Parse.Action; base: INTEGER;
      <*UNUSED*> READONLY info: Err.LocationInfo): Parse.Tree =
    BEGIN
      RETURN Parse.Stack[base+5];
    END Select5;

  PROCEDURE BuildIdeName(<*UNUSED*> self: Parse.Identifier; text: TEXT;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(IdeName, location:=Err.NewLineLocation(info),
	text:=text, variant:=0, absoluteEnvIndex:=-1);
    END BuildIdeName;

  PROCEDURE BuildInteger(<*UNUSED*> self: Parse.Integer; int: INTEGER;
      <*UNUSED*> READONLY info: Err.LocationInfo): Parse.Tree =
    BEGIN
      RETURN NEW(IntegerTemp, int:=int);
    END BuildInteger;

  <*UNUSED*>
  PROCEDURE BuildString(<*UNUSED*> self: Parse.QuotedString; string: String.T;
      <*UNUSED*> READONLY info: Err.LocationInfo): Parse.Tree =
    BEGIN
      RETURN NEW(StringTemp, string:=string);
    END BuildString;

  PROCEDURE BuildPhraseEmpty(<*UNUSED*> self: Parse.GivenDelimiter;
      <*UNUSED*> READONLY info: Err.LocationInfo): Parse.Tree =
    BEGIN
      RETURN NIL;
    END BuildPhraseEmpty;

  PROCEDURE BuildPhraseSyntax(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(Grammar, location:=Err.NewLineLocation(info),
	gramInfo:=NARROW(Parse.Stack[base+1], Gram.GramInfo));
    END BuildPhraseSyntax;

  PROCEDURE BuildTypeBinding(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    VAR bound: Parse.Tree;
    BEGIN
      bound:=Parse.Stack[base+2];
      IF bound=NIL THEN 
	bound:=NEW(TypeTop, location:=Err.NewLineLocation(info));
      END;
      RETURN 
	NEW(TypeBinding, location:=Err.NewLineLocation(info),
	  binder:=Parse.Stack[base+1], 
	  bound:=bound,
	  type:=Parse.Stack[base+4],
	  rest:=Parse.Stack[base+5]);
    END BuildTypeBinding;

  PROCEDURE BuildTermBinding(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN 
	NEW(TermBinding, location:=Err.NewLineLocation(info),
	  binder:=Parse.Stack[base+1], 
	  bound:=Parse.Stack[base+2],
	  term:=Parse.Stack[base+4],
	  rest:=Parse.Stack[base+5]);
    END BuildTermBinding;

  PROCEDURE BuildTypeArrow(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(TypeArrow, location:=Err.NewLineLocation(info),
	dom:=Parse.Stack[base+1], rng:=Parse.Stack[base+3]);
    END BuildTypeArrow;

   PROCEDURE BuildTypeIde(<*UNUSED*> self: Parse.Identifier; text: TEXT;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(TypeIde, location:=Err.NewLineLocation(info),
	name:=NEW(IdeName, text:=text, variant:=0, absoluteEnvIndex:=-1),
	index:=0);
    END BuildTypeIde;

  PROCEDURE BuildTypePos(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(TypePatternPosition, location:=Err.NewLineLocation(info),
	position:=NARROW(Parse.Stack[base+1], IntegerTemp).int);
    END BuildTypePos;

  PROCEDURE BuildTypeTop(<*UNUSED*> self: Parse.GivenKeyword;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(TypeTop, location:=Err.NewLineLocation(info));
    END BuildTypeTop;

  PROCEDURE BuildTypeForallIdeBinder(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    VAR bound: Parse.Tree;
    BEGIN
      bound := Parse.Stack[base+3];
      IF bound=NIL THEN 
	bound:=NEW(TypeTop, location:=Err.NewLineLocation(info));
      END;
      RETURN NEW(TypeForall, location:=Err.NewLineLocation(info),
	binder:=Parse.Stack[base+2],
	omit:=Parse.Stack[base+6]#NIL,
	bound:=bound,
	body:=Parse.Stack[base+4]);
    END BuildTypeForallIdeBinder;

  PROCEDURE BuildTypeForallPosBinder(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    VAR bound: Parse.Tree;
    BEGIN
      bound := Parse.Stack[base+3];
      IF bound=NIL THEN 
	bound:=NEW(TypeTop, location:=Err.NewLineLocation(info));
      END;
      RETURN NEW(TypePatternForall, location:=Err.NewLineLocation(info),
	position:=NARROW(Parse.Stack[base+2], IntegerTemp).int,
	omit:=Parse.Stack[base+6]#NIL,
	bound:=bound,
	body:=Parse.Stack[base+4]);
    END BuildTypeForallPosBinder;

  PROCEDURE BuildTypeRec(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(TypeRec, location:=Err.NewLineLocation(info),
	binder:=Parse.Stack[base+1],
	body:=Parse.Stack[base+2]);
    END BuildTypeRec;

  PROCEDURE BuildTermIde(<*UNUSED*> self: Parse.Identifier; text: TEXT;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(TermIde, location:=Err.NewLineLocation(info),
	name:=NEW(IdeName, text:=text, variant:=0, absoluteEnvIndex:=-1),
	index:=0);
    END BuildTermIde;

  PROCEDURE BuildTermIdeOmit(<*UNUSED*> self: Parse.Action; base: INTEGER;
      <*UNUSED*> READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      TYPECASE Parse.Stack[base+1] OF
      | TermIde(node) => node.omitArgs:=FALSE;
      ELSE Scanner.Syntax("\'!\' must follow an identifier");
      END;
      RETURN Parse.Stack[base+1];
    END BuildTermIdeOmit;

  PROCEDURE BuildTermPos(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(TermPatternPosition, location:=Err.NewLineLocation(info),
	position:=NARROW(Parse.Stack[base+1], IntegerTemp).int);
    END BuildTermPos;

  PROCEDURE BuildTermTop(<*UNUSED*> self: Parse.GivenKeyword;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(TermTop, location:=Err.NewLineLocation(info));
    END BuildTermTop;

  PROCEDURE BuildOmitParam(<*UNUSED*> self: Parse.GivenDelimiter;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(OmitParam, location:=Err.NewLineLocation(info));
    END BuildOmitParam;

  PROCEDURE BuildTermFunIde1(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      IF Parse.Stack[base+6]#NIL THEN
	Scanner.Syntax("\'?\' cannot appear here");
      END;
      RETURN 
        NEW(TermFun,location:=Err.NewLineLocation(info),
	  binder:=Parse.Stack[base+2],
	  bound:=Parse.Stack[base+4],
	  body:=Parse.Stack[base+5]);
    END BuildTermFunIde1;

  PROCEDURE BuildTermFunIde2NoBound(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN 
        NEW(TermFun2, location:=Err.NewLineLocation(info),
	  binder:=Parse.Stack[base+2], omit:=Parse.Stack[base+6]#NIL,
	  bound:=NEW(TypeTop, location:=Err.NewLineLocation(info)),
	  body:=Parse.Stack[base+5]);
    END BuildTermFunIde2NoBound;

  PROCEDURE BuildTermFunIde2(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN 
        NEW(TermFun2,location:=Err.NewLineLocation(info),
	  binder:=Parse.Stack[base+2], omit:=Parse.Stack[base+6]#NIL,
	  bound:=Parse.Stack[base+4],
	  body:=Parse.Stack[base+5]);
    END BuildTermFunIde2;

  PROCEDURE BuildTermFunPos1(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      IF Parse.Stack[base+6]#NIL THEN
	Scanner.Syntax("\'?\' cannot appear here");
      END;
      RETURN 
        NEW(TermPatternFun,location:=Err.NewLineLocation(info),
	  position:=NARROW(Parse.Stack[base+2], IntegerTemp).int,
	  bound:=Parse.Stack[base+4],
	  body:=Parse.Stack[base+5]);
    END BuildTermFunPos1;

  PROCEDURE BuildTermFunPos2(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN 
        NEW(TermPatternFun2,location:=Err.NewLineLocation(info),
	  position:=NARROW(Parse.Stack[base+2], IntegerTemp).int,
	  omit:=Parse.Stack[base+6]#NIL,
	  bound:=Parse.Stack[base+4],
	  body:=Parse.Stack[base+5]);
    END BuildTermFunPos2;

 PROCEDURE BuildTermFunPos2NoBound(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN 
        NEW(TermPatternFun2, location:=Err.NewLineLocation(info),
	  position:=NARROW(Parse.Stack[base+2], IntegerTemp).int,
	  omit:=Parse.Stack[base+6]#NIL,
	  bound:=NEW(TypeTop, location:=Err.NewLineLocation(info)),
	  body:=Parse.Stack[base+5]);
    END BuildTermFunPos2NoBound;

  PROCEDURE BuildTermAppl1(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(TermAppl, location:=Err.NewLineLocation(info),
	fun:=Parse.Stack[base+1], arg:=Parse.Stack[base+3]);
    END BuildTermAppl1;

  PROCEDURE BuildTermAppl2(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(TermAppl2, location:=Err.NewLineLocation(info),
	fun:=Parse.Stack[base+1], arg:=Parse.Stack[base+3]);
    END BuildTermAppl2;

  PROCEDURE BuildTermFold(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(TermFold, location:=Err.NewLineLocation(info),
	recType:=Parse.Stack[base+1], arg:=Parse.Stack[base+2]);
    END BuildTermFold;

  PROCEDURE BuildTermUnfold(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(TermUnfold, location:=Err.NewLineLocation(info),
	arg:=Parse.Stack[base+1]);
    END BuildTermUnfold;

  PROCEDURE BuildTermRec(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(TermRec, location:=Err.NewLineLocation(info),
	binder:=Parse.Stack[base+1], bound:=Parse.Stack[base+2],
	body:=Parse.Stack[base+3]);
    END BuildTermRec;

  PROCEDURE BuildPhraseJudgeContext(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(JudgeContext, location:=Err.NewLineLocation(info),
	context:=Parse.Stack[base+1]);
    END BuildPhraseJudgeContext;

  PROCEDURE BuildPhraseJudgeType(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(JudgeType, location:=Err.NewLineLocation(info),
	context:=Parse.Stack[base+1], type:=Parse.Stack[base+2]);
    END BuildPhraseJudgeType;

  PROCEDURE BuildPhraseJudgeSubtype(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(JudgeSubtype, location:=Err.NewLineLocation(info),
	context:=Parse.Stack[base+1], subType:=Parse.Stack[base+2],
	superType:=Parse.Stack[base+3]);
    END BuildPhraseJudgeSubtype;

  PROCEDURE BuildPhraseJudgeTerm(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(JudgeTerm, location:=Err.NewLineLocation(info),
	context:=Parse.Stack[base+1], term:=Parse.Stack[base+2],
	type:=Parse.Stack[base+3]);
    END BuildPhraseJudgeTerm;

  PROCEDURE BuildContextType(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(ContextType, location:=Err.NewLineLocation(info),
	binder:=Parse.Stack[base+1], 
        bound:=Parse.Stack[base+3], rest:=Parse.Stack[base+4]);
    END BuildContextType;

  PROCEDURE BuildContextTerm(<*UNUSED*> self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(ContextTerm, location:=Err.NewLineLocation(info),
	binder:=Parse.Stack[base+1], type:=Parse.Stack[base+3],
	rest:=Parse.Stack[base+4]);
    END BuildContextTerm;

  PROCEDURE SameIdeName(name1, name2: IdeName): BOOLEAN =
    BEGIN
      RETURN Text.Equal(name1.text, name2.text) AND 
        (name1.variant=name2.variant);
        (* ignoring  absoluteEnvIndex *)
    END SameIdeName;

  PROCEDURE BeEnv(env: Env; name: IdeName; rest: Env) =
  BEGIN
    env.name := name;
    env.decoration := FreshDecoration(name, rest);
    env.rest := rest;
  END BeEnv;

  PROCEDURE NewEnv(name: IdeName; rest: Env): Env =
  VAR env: Env;
  BEGIN
    env := NEW(Env);
    BeEnv(env, name, rest);
    RETURN env;
  END NewEnv;

  PROCEDURE FreshDecoration(name: IdeName; env: Env): INTEGER =
  BEGIN
    LOOP
      IF env=NIL THEN RETURN 0 END;
      IF Text.Equal(env.name.text, name.text) THEN RETURN env.decoration+1 END;
      env := env.rest;
    END;
  END FreshDecoration;

  PROCEDURE FetchDecoration(name: IdeName; env: Env): INTEGER =
  BEGIN
    LOOP
      IF env=NIL THEN RETURN -1 END;
      IF SameIdeName(env.name, name) THEN RETURN env.decoration END;
      env := env.rest;
    END;
  END FetchDecoration;

  PROCEDURE PrintDecoration(fmt: Formatter.T; decoration: INTEGER) RAISES ANY =
  BEGIN
    Formatter.PutText(fmt, FmtDecoration(decoration));
  END PrintDecoration;

  PROCEDURE PrintAbsoluteEnvIndex(fmt: Formatter.T; index: INTEGER) RAISES ANY =
  BEGIN
    Formatter.PutText(fmt, FmtAbsoluteEnvIndex(index));
  END PrintAbsoluteEnvIndex;

  PROCEDURE PrintIdeName(fmt: Formatter.T; name: IdeName; env:Env) RAISES ANY =
  BEGIN
    Formatter.Begin(fmt);
    Formatter.PutText(fmt, name.text);
    PrintDecoration(fmt, FetchDecoration(name, env));
    PrintAbsoluteEnvIndex(fmt, name.absoluteEnvIndex);
    IF printScopeLevel THEN
      IF name.variant#0 THEN
	Formatter.PutChar(fmt, '%');
	Formatter.PutText(fmt, Fmt.Int(name.variant));
      END;
    END;
    Formatter.End(fmt);
  END PrintIdeName;

  PROCEDURE PrintIde(fmt: Formatter.T; name: IdeName; index: INTEGER; 
    env: Env) RAISES ANY =
  BEGIN
    Formatter.Begin(fmt);
    PrintIdeName(fmt, name, env);
    IF printDeBruijnIndex THEN
      Formatter.PutChar(fmt, '_');
      Formatter.PutText(fmt, Fmt.Int(index));
    END;
    Formatter.End(fmt);
  END PrintIde;

  PROCEDURE FmtDecoration(decoration: INTEGER): TEXT =
  VAR res: TEXT;
  BEGIN
    IF decoration=0 THEN RETURN "" END;
    IF decoration<0 THEN RETURN "@" END;
    res := "";
    LOOP
      CASE decoration MOD 4 OF <*NOWARN*>
      | 1 => res := "\'" & res;
      | 2 => res := "\"" & res;
      | 3 => res := "^" & res;
      | 0 => res := "~" & res;
      END;
      decoration := (decoration-1) DIV 4;
      IF decoration = 0 THEN EXIT END;
    END;
    RETURN res;
  END FmtDecoration;

  PROCEDURE FmtAbsoluteEnvIndex(index: INTEGER): TEXT =
  BEGIN
    IF index >=0 THEN RETURN "##" & Fmt.Int(index) ELSE RETURN "" END;
  END FmtAbsoluteEnvIndex;

  PROCEDURE FmtIdeName(name: IdeName; env: Env): TEXT =
  VAR text: TEXT;
  BEGIN
    text := name.text & FmtDecoration(FreshDecoration(name, env))
      & FmtAbsoluteEnvIndex(name.absoluteEnvIndex);
    IF printScopeLevel THEN
      IF name.variant>0 THEN
	text:=text & "%";
	text := text & Fmt.Int(name.variant);
      END;
    END;
    RETURN text;
  END FmtIdeName;

  PROCEDURE FmtIde(name: IdeName; index: INTEGER; env: Env): TEXT =
  VAR text: TEXT;
  BEGIN
    text := FmtIdeName(name, env);
    IF printDeBruijnIndex THEN
      text := text & "_" & Fmt.Int(index);
    END;
    RETURN text;
  END FmtIde;

  PROCEDURE PrintTypeBinding(fmt: Formatter.T; 
	binding: TypeBinding; env: Env) RAISES ANY =
  BEGIN
    Formatter.Begin(fmt, 2);
    Formatter.PutText(Out.out, "Let ");
    PrintTypeBinding1(fmt, binding, env);
    Formatter.End(fmt);
  END PrintTypeBinding;

  PROCEDURE PrintTypeBinding1(fmt: Formatter.T; 
	binding: TypeBinding; env: Env) RAISES ANY =
  VAR newEnv: Env;
  BEGIN
    TYPECASE binding OF
    | NULL =>
    | TypeBinding(node) =>
	Formatter.UnitedBreak(fmt);
	Formatter.Begin(fmt, 2);
	  Formatter.Begin(fmt, 4);
	    newEnv := NewEnv(node.binder, env);
	    PrintIdeName(fmt, node.binder, newEnv);
	    Formatter.PutText(fmt, " <: ");
	  Formatter.UnitedBreak(fmt);
	    PrintType(fmt, node.bound, env);
	    Formatter.PutText(fmt, " = ");
	  Formatter.End(fmt);
	Formatter.UnitedBreak(fmt);
	  PrintType(fmt, node.type, env);
	  Formatter.PutText(fmt, " ");
	Formatter.End(fmt);
	PrintTypeBinding1(fmt, node.rest, newEnv);
    END
  END PrintTypeBinding1;

  PROCEDURE PrintTermBinding(fmt: Formatter.T; 
	binding: TermBinding; env: Env) RAISES ANY =
  BEGIN
    Formatter.Begin(fmt, 2);
    Formatter.PutText(Out.out, "let ");
    PrintTermBinding1(fmt, binding, env);
    Formatter.End(fmt);
  END PrintTermBinding;

  PROCEDURE PrintTermBinding1(fmt: Formatter.T; 
	binding: TermBinding; env: Env) RAISES ANY =
  VAR newEnv: Env;
  BEGIN
    TYPECASE binding OF
    | NULL =>
    | TermBinding(node) =>
        Formatter.UnitedBreak(Out.out);
	Formatter.Begin(fmt, 2);
	  Formatter.Begin(fmt, 4);
	    newEnv := NewEnv(node.binder, env);
	    PrintIdeName(fmt, node.binder, newEnv);
	    IF node.bound # NIL THEN
  	      Formatter.PutText(fmt, " : ");
	  Formatter.UnitedBreak(fmt);
	      PrintType(fmt, node.bound, env);
	    END;
	    Formatter.PutText(fmt, " = ");
	  Formatter.End(fmt);
	Formatter.UnitedBreak(fmt);
	  PrintTerm(fmt, node.term, env);
	  Formatter.PutText(fmt, " ");
	Formatter.End(fmt);
	PrintTermBinding1(fmt, node.rest, newEnv);
    END
  END PrintTermBinding1;

  PROCEDURE PrintTag(fmt: Formatter.T; name: IdeName) RAISES ANY =
  BEGIN
    Formatter.PutChar(fmt, '<');
    Formatter.PutText(fmt, name.text);
    Formatter.PutText(fmt, ">=");
  END PrintTag;

  PROCEDURE PrintType(fmt: Formatter.T; type: Type; env: Env) RAISES ANY =
  VAR newEnv: Env;
  BEGIN
    IF type.tag#NIL THEN PrintTag(fmt, type.tag); END;
    TYPECASE type OF
    | NULL => Formatter.PutChar(fmt, '_');
    | TypeIde(node) =>
	PrintIde(fmt, node.name, node.index, env);
    | TypePatternPosition(node) =>
	Formatter.PutText(fmt, "_" & Fmt.Int(node.position));
    | TypeTop =>
	Formatter.PutText(fmt, "Top");
    | TypeArrow(node) =>
	Formatter.Begin(fmt, 2);
          Formatter.PutChar(fmt, '{');
  	  PrintType(fmt, node.dom, env);
	  Formatter.PutText(fmt, "->");
	Formatter.UnitedBreak(fmt);
	  PrintType(fmt, node.rng, env);
          Formatter.PutChar(fmt, '}');
	Formatter.End(fmt);
    | TypeForall(node) =>
	Formatter.Begin(fmt, 2);
          Formatter.PutText(fmt, "{All(");	
  	  Formatter.Begin(fmt, 2);
	    newEnv := NewEnv(node.binder, env);
	    PrintIdeName(fmt, node.binder, newEnv);
	    IF node.omit THEN Formatter.PutChar(fmt, '?') END;
	    IF NOT ISTYPE(node.bound, TypeTop) THEN
	      Formatter.PutText(fmt, "<:");
	  Formatter.UnitedBreak(fmt);
	      PrintType(fmt, node.bound, env);
	    END;
	  Formatter.End(fmt);
          Formatter.PutChar(fmt, ')');
	Formatter.UnitedBreak(fmt);
	  PrintType(fmt, node.body, newEnv);
          Formatter.PutChar(fmt, '}');
	Formatter.End(fmt);
    | TypePatternForall(node) =>
	Formatter.Begin(fmt, 2);
          Formatter.PutText(fmt, "{All(");
  	  Formatter.Begin(fmt, 2);
	    Formatter.PutText(fmt, "_" & Fmt.Int(node.position));
	    IF node.omit THEN Formatter.PutChar(fmt, '?') END;
	    IF NOT ISTYPE(node.bound, TypeTop) THEN
	      Formatter.PutText(fmt, "<:");
	  Formatter.UnitedBreak(fmt);
	      PrintType(fmt, node.bound, env);
	    END;
	  Formatter.End(fmt);
          Formatter.PutChar(fmt, ')');
	Formatter.UnitedBreak(fmt);
	  PrintType(fmt, node.body, env);
          Formatter.PutChar(fmt, '}');
	Formatter.End(fmt);
    | TypeRec(node) =>
	Formatter.Begin(fmt, 2);
          Formatter.PutText(fmt, "{Rec(");	
	  newEnv := NewEnv(node.binder, env);
	  PrintIdeName(fmt, node.binder, newEnv);
          Formatter.PutChar(fmt, ')');
	Formatter.UnitedBreak(fmt);
	  PrintType(fmt, node.body, newEnv);
          Formatter.PutChar(fmt, '}');
	Formatter.End(fmt);
    ELSE
	Formatter.PutText(fmt, "<?>");
    END;
  END PrintType;

  PROCEDURE PrintTerm(fmt: Formatter.T; term: Term; env: Env) RAISES ANY =
  VAR newEnv: Env;
  BEGIN
    TYPECASE term OF
    | NULL => Formatter.PutChar(fmt, '_');
    | TermIde(node) =>
        IF node.omitArgs THEN
	  PrintIde(fmt, node.name, node.index, env);
	ELSE
  	  Formatter.Begin(fmt);
	  PrintIde(fmt, node.name, node.index, env);
	  Formatter.PutChar(fmt, '!');
  	  Formatter.Begin(fmt);
	END;
    | TermPatternPosition(node) =>
	Formatter.PutText(fmt, "_" & Fmt.Int(node.position));
    | TermTop =>
	Formatter.PutText(fmt, "top");
    | TermFun(node) =>
	Formatter.Begin(fmt, 2);
	  Formatter.Begin(fmt, 4);
            Formatter.PutText(fmt, "{fun(");
	  Formatter.UnitedBreak(fmt);
	    Formatter.Begin(fmt, 2);
	      newEnv := NewEnv(node.binder, env);
	      PrintIdeName(fmt, node.binder, newEnv);
	      Formatter.PutText(fmt, ":");
	    Formatter.UnitedBreak(fmt);
	      PrintType(fmt, node.bound, env);
	    Formatter.End(fmt);
            Formatter.PutChar(fmt, ')');
	  Formatter.End(fmt);
	Formatter.UnitedBreak(fmt);
	  PrintTerm(fmt, node.body, newEnv);
          Formatter.PutChar(fmt, '}');
	Formatter.End(fmt);
    | TermPatternFun(node) =>
	Formatter.Begin(fmt, 2);
	  Formatter.Begin(fmt, 4);
            Formatter.PutText(fmt, "{fun(");
	  Formatter.UnitedBreak(fmt);
	    Formatter.Begin(fmt, 2);
	      Formatter.PutText(fmt, "_" & Fmt.Int(node.position) & ":");
	    Formatter.UnitedBreak(fmt);
	      PrintType(fmt, node.bound, env);
	    Formatter.End(fmt);
            Formatter.PutChar(fmt, ')');
	  Formatter.End(fmt);
	Formatter.UnitedBreak(fmt);
	  PrintTerm(fmt, node.body, env);
          Formatter.PutChar(fmt, '}');
	Formatter.End(fmt);
    | TermAppl(node) =>
	Formatter.Begin(fmt, 2);
	  PrintTerm(fmt, node.fun, env);
	  Formatter.PutChar(fmt, '(');
	Formatter.UnitedBreak(fmt);
	  PrintTerm(fmt, node.arg, env);
	  Formatter.PutChar(fmt, ')');
	Formatter.End(fmt);
    | TermFun2(node) =>
	Formatter.Begin(fmt, 2);
	  Formatter.Begin(fmt, 4);
            Formatter.PutText(fmt, "{fun(");
	  Formatter.UnitedBreak(fmt);
	    Formatter.Begin(fmt, 2);
	      newEnv := NewEnv(node.binder, env);
	      PrintIdeName(fmt, node.binder, newEnv);
	      IF node.omit THEN Formatter.PutChar(fmt, '?') END;
	      IF NOT ISTYPE(node.bound, TypeTop) THEN
	        Formatter.PutText(fmt, "<:");
	    Formatter.UnitedBreak(fmt);
	        PrintType(fmt, node.bound, env);
	      END;
	    Formatter.End(fmt);
            Formatter.PutChar(fmt, ')');
	  Formatter.End(fmt);
	Formatter.UnitedBreak(fmt);
	  PrintTerm(fmt, node.body, newEnv);
          Formatter.PutChar(fmt, '}');
	Formatter.End(fmt);
    | TermPatternFun2(node) =>
	Formatter.Begin(fmt, 2);
	  Formatter.Begin(fmt, 4);
        Formatter.PutText(fmt, "{fun(");
	  Formatter.UnitedBreak(fmt);
	    Formatter.Begin(fmt, 2);
	      Formatter.PutText(fmt, "_" & Fmt.Int(node.position));
	      IF node.omit THEN Formatter.PutChar(fmt, '?') END;
	      IF NOT ISTYPE(node.bound, TypeTop) THEN
	        Formatter.PutText(fmt, "<:");
	    Formatter.UnitedBreak(fmt);
	        PrintType(fmt, node.bound, env);
	      END;
	    Formatter.End(fmt);
            Formatter.PutChar(fmt, ')');
	  Formatter.End(fmt);
	Formatter.UnitedBreak(fmt);
	  PrintTerm(fmt, node.body, env);
          Formatter.PutChar(fmt, '}');
	Formatter.End(fmt);
    | TermAppl2(node) =>
	Formatter.Begin(fmt, 2);
	  PrintTerm(fmt, node.fun, env);
	  Formatter.PutChar(fmt, '(');
	Formatter.UnitedBreak(fmt);
	  Formatter.PutChar(fmt, ':');
	  PrintType(fmt, node.arg, env);
	  Formatter.PutChar(fmt, ')');
	Formatter.End(fmt);
    | TermFold(node) =>
	Formatter.Begin(fmt, 2);
	  Formatter.Begin(fmt, 4);
            Formatter.PutText(fmt, "fold(");
	  Formatter.UnitedBreak(fmt);
	    Formatter.PutText(fmt, ":");
	    PrintType(fmt, node.recType, env);
            Formatter.PutChar(fmt, ')');
	  Formatter.End(fmt);
	Formatter.UnitedBreak(fmt);
	  Formatter.Begin(fmt, 4);
            Formatter.PutChar(fmt, '(');
	  Formatter.UnitedBreak(fmt);
	    PrintTerm(fmt, node.arg, env);
            Formatter.PutChar(fmt, ')');
	  Formatter.End(fmt);
	Formatter.End(fmt);
    | TermUnfold(node) =>
	Formatter.Begin(fmt, 2);
          Formatter.PutText(fmt, "unfold(");
	Formatter.UnitedBreak(fmt);
	  PrintTerm(fmt, node.arg, env);
          Formatter.PutChar(fmt, ')');
	Formatter.End(fmt);
    | TermRec(node) =>
	Formatter.Begin(fmt, 2);
	  Formatter.Begin(fmt, 4);
            Formatter.PutText(fmt, "{rec(");
	  Formatter.UnitedBreak(fmt);
	    Formatter.Begin(fmt, 2);
	      newEnv := NewEnv(node.binder, env);
	      PrintIdeName(fmt, node.binder, newEnv);
	      Formatter.PutText(fmt, ":");
	    Formatter.UnitedBreak(fmt);
	      PrintType(fmt, node.bound, env);
	    Formatter.End(fmt);
            Formatter.PutChar(fmt, ')');
	  Formatter.End(fmt);
	Formatter.UnitedBreak(fmt);
	  PrintTerm(fmt, node.body, newEnv);
          Formatter.PutChar(fmt, '}');
	Formatter.End(fmt);
     ELSE
	Formatter.PutText(fmt, "<?>");
    END;
  END PrintTerm; 

PROCEDURE Copy(tree: Parse.Tree): Parse.Tree RAISES ANY =
  VAR res: Parse.Tree;
  BEGIN
    TYPECASE tree OF
    | NULL => res := NIL;
    | IdeName(node) =>
        res := NEW(IdeName, text:=node.text, variant:=node.variant,
          absoluteEnvIndex:=node.absoluteEnvIndex);
    | TypeBinding(node) =>
	res := NEW(TypeBinding, binder:=Copy(node.binder), 
	  bound:=Copy(node.bound), type:=Copy(node.type),
	  rest:=Copy(node.rest));
    | TermBinding(node) =>
	res := NEW(TermBinding, binder:=Copy(node.binder), 
	  bound:=Copy(node.bound), term:=Copy(node.term),
	  rest:=Copy(node.rest));
    | TypeIde(node) =>
	res := NEW(TypeIde, tag:=node.tag, name:=Copy(node.name), 
	  index:=node.index);
    | TypeTop(node) => res := NEW(TypeTop, tag:=node.tag);
    | TypeArrow(node) =>
	res := NEW(TypeArrow, tag:=node.tag, 
	  dom:=Copy(node.dom), rng:=Copy(node.rng));
    | TypeForall(node) =>
	res := NEW(TypeForall, tag:=node.tag, binder:=Copy(node.binder),
	  omit:=node.omit, bound:=Copy(node.bound), body:=Copy(node.body));
    | TypeRec(node) =>
	res := NEW(TypeRec, tag:=node.tag, binder:=Copy(node.binder),
	  body:=Copy(node.body));
    | TermIde(node) =>
	res := NEW(TermIde, name:=Copy(node.name), index:=node.index,
	  omitArgs:=node.omitArgs, omitCount:=node.omitCount);
    | TermTop => res := NEW(TermTop);
    | TermFun(node) =>
	res := NEW(TermFun, binder:=Copy(node.binder),
	  bound:=Copy(node.bound), body:=Copy(node.body));
    | TermAppl(node) =>
	res := NEW(TermAppl, fun:=Copy(node.fun), arg:=Copy(node.arg));
    | TermFun2(node) =>
	res := NEW(TermFun2, binder:=Copy(node.binder), omit:=node.omit,
	  bound:=Copy(node.bound), body:=Copy(node.body));
    | TermAppl2(node) =>
	res := NEW(TermAppl2, fun:=Copy(node.fun), arg:=Copy(node.arg));
    | TermFold(node) =>
	res := NEW(TermFold, recType:=Copy(node.recType),
	  arg:=Copy(node.arg));
    | TermUnfold(node) =>
	res := NEW(TermUnfold, arg:=Copy(node.arg));
    | TermRec(node) =>
	res := NEW(TermRec, binder:=Copy(node.binder),
	  bound:=Copy(node.bound), body:=Copy(node.body));
    ELSE Err.Fault(Out.out, "Tree.Copy");
    END;
    IF res#NIL THEN res.location := tree.location END;
    RETURN res;
  END Copy;

  PROCEDURE GetGrammars(
    VAR (*out*) actionTermGrammar, actionTypeGrammar: Parse.NonTerminal) =
  BEGIN
    actionTermGrammar := term; (* for use by Gram *)
    actionTypeGrammar := type; (* for use by Gram *)
  END GetGrammars;

  PROCEDURE PrintDeBruijnIndex(self: Command.T; arg: TEXT) RAISES ANY =
    BEGIN
      IF Text.Equal(arg, "?") THEN
	Formatter.PutText(Out.out, self.name & " {On Off} is ");
	IF printDeBruijnIndex THEN Formatter.PutText(Out.out, "On");
	ELSE Formatter.PutText(Out.out, "Off"); END;
	Formatter.NewLine(Out.out);
      ELSIF Text.Equal(arg, "On") THEN printDeBruijnIndex:=TRUE;
      ELSIF Text.Equal(arg, "Off") THEN printDeBruijnIndex:=FALSE;
      ELSE
	Formatter.PutText(Out.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	Formatter.NewLine(Out.out);
      END;
    END PrintDeBruijnIndex;

  PROCEDURE PrintScopeLevel(self: Command.T; arg: TEXT) RAISES ANY =
    BEGIN
      IF Text.Equal(arg, "?") THEN
	Formatter.PutText(Out.out, self.name & " {On Off} is ");
	IF printScopeLevel THEN Formatter.PutText(Out.out, "On");
	ELSE Formatter.PutText(Out.out, "Off"); END;
	Formatter.NewLine(Out.out);	
      ELSIF Text.Equal(arg, "On") THEN printScopeLevel:=TRUE;
      ELSIF Text.Equal(arg, "Off") THEN printScopeLevel:=FALSE;
      ELSE
	Formatter.PutText(Out.out, "Command " & self.name 
	  & ": bad argument: " & arg);
	Formatter.NewLine(Out.out);
      END;
    END PrintScopeLevel;

  PROCEDURE Setup() RAISES ANY =
  BEGIN

    printDeBruijnIndex := FALSE;
    Command.Register(
      NEW(Command.T, name:="ShowVarIndex", 
	Exec:=PrintDeBruijnIndex));

    printScopeLevel := FALSE;
    Command.Register(
      NEW(Command.T, name:="ShowVarLevel",
	Exec:=PrintScopeLevel));

    noName := NEW(IdeName, text:="", variant:=-1, absoluteEnvIndex:=-1);

    keySet := Gram.keySet;
    keyHasType := Scanner.BeKeyword(":", keySet);
    keyHasSubtype := Scanner.BeKeyword("<:", keySet);
    keyTypeTop := Scanner.BeKeyword("Top", keySet);
    keyTypeArrow := Scanner.BeKeyword("->", keySet);
    keyTypeForall := Scanner.BeKeyword("All", keySet);
    keyTypeRec := Scanner.BeKeyword("Rec", keySet);
    keyTermTop := Scanner.BeKeyword("top", keySet);
    keyTermFun := Scanner.BeKeyword("fun", keySet);
    keyTermFold := Scanner.BeKeyword("fold", keySet);
    keyTermUnfold := Scanner.BeKeyword("unfold", keySet);
    keyTermRec := Scanner.BeKeyword("rec", keySet);
    keyTermLet := Scanner.BeKeyword("let", keySet);
    keyTypeLet := Scanner.BeKeyword("Let", keySet);
    keyEqDef := Scanner.BeKeyword("=", keySet);
    keyJudge := Scanner.BeKeyword("judge", keySet);
    keyEntail := Scanner.BeKeyword("|-", keySet);

    env := Gram.env;

    phrase := NEW(Parse.NonTerminal, name:="phrase"); (* public *)
    phraseEmpty := NEW(Parse.NonTerminal, name:="*phraseEmpty");
    phraseSyntax := NEW(Parse.NonTerminal, name:="*phraseSyntax");
    phraseTypeBinding := NEW(Parse.NonTerminal, name:="*phraseTypeBinding");
    phraseTermBinding := NEW(Parse.NonTerminal, name:="*phraseTermBinding");
    phraseType := NEW(Parse.NonTerminal, name:="*phraseType");
    phraseTerm := NEW(Parse.NonTerminal, name:="*phraseTerm");
    phraseJudge := NEW(Parse.NonTerminal, name:="*phraseJudge");
    phraseJudegeContext := NEW(Parse.NonTerminal, name:="*phraseJudegeContext");
    phraseJudgeType := NEW(Parse.NonTerminal, name:="*phraseJudgeType");
    phraseJudgeSubtype := NEW(Parse.NonTerminal, name:="*phraseJudgeSubtype");
    phraseJudgeTerm := NEW(Parse.NonTerminal, name:="*phraseJudgeTerm");
    context := NEW(Parse.NonTerminal, name:="*context");

    binderIde := NEW(Parse.NonTerminal, name:="*binderIde");
    binderPos := NEW(Parse.NonTerminal, name:="*binderPos");

    typeBinding := NEW(Parse.NonTerminal, name:="*typeBinding");
    termBinding := NEW(Parse.NonTerminal, name:="*termBinding");

    type := NEW(Parse.NonTerminal, name:="type"); (* public *)
    typeBase := NEW(Parse.NonTerminal, name:="typeBase"); (* public *)
    typeOper := NEW(Parse.NonTerminal, name:="typeOper"); (* public *)
    typeIde := NEW(Parse.NonTerminal, name:="typeIde"); (* public *)
    typePos := NEW(Parse.NonTerminal, name:="*typePos");
    typeTop := NEW(Parse.NonTerminal, name:="*typeTop");
    typeArrow := NEW(Parse.NonTerminal, name:="*typeArrow");
    typeForall := NEW(Parse.NonTerminal, name:="*typeForall");
    typeForallBind := NEW(Parse.NonTerminal, name:="*typeForallBind");
    typeRec := NEW(Parse.NonTerminal, name:="*typeRec");
    typeParen := NEW(Parse.NonTerminal, name:="*typeParen");

    term := NEW(Parse.NonTerminal, name:="term"); (* public *)
    termBase := NEW(Parse.NonTerminal, name:="termBase"); (* public *)
    termOper := NEW(Parse.NonTerminal, name:="termOper"); (* public *)
    termAppl := NEW(Parse.NonTerminal, name:="termAppl"); (* public *)
    termIde := NEW(Parse.NonTerminal, name:="termIde"); (* public *)
    termPos := NEW(Parse.NonTerminal, name:="*termPos");
    termTop:= NEW(Parse.NonTerminal, name:="*termTop");
    termFun := NEW(Parse.NonTerminal ,name:="*termFun");
    termFunBind := NEW(Parse.NonTerminal ,name:="*termFunBind");
    termFunBindIde := NEW(Parse.NonTerminal ,name:="*termFunBindIde");
    termFunBindPos := NEW(Parse.NonTerminal ,name:="*termFunBindPos");
    termFunBindIde1 := NEW(Parse.NonTerminal ,name:="*termFunBindIde1");
    termFunBindIde2 := NEW(Parse.NonTerminal ,name:="*termFunBindIde2");
    termFunBindPos1 := NEW(Parse.NonTerminal ,name:="*termFunBindPos1");
    termFunBindPos2 := NEW(Parse.NonTerminal ,name:="*termFunBindPos2");

    termIterAppl := NEW(Parse.NonTerminal, name:="*termIterAppl");
    termFold := NEW(Parse.NonTerminal, name:="*termFold");
    termUnfold := NEW(Parse.NonTerminal, name:="*termUnfold");
    termRec := NEW(Parse.NonTerminal, name:="*termRec");
    termParen := NEW(Parse.NonTerminal, name:="*termParen");

    Gram.InitGrammars((*out*)synTerm, (*out*)synDecl);
    (* This ends up calling back GetGrammars. *)

(* phrase ::=
	{ phraseEmpty phraseSyntax phraseTypeBinding
	  phraseTermBinding phraseType phraseTerm phraseJudge }
*)
    env.Add(phrase.name,
      NEW(Parse.Choice, choice:=Parse.List(
	phraseEmpty,
	phraseSyntax,
	phraseTypeBinding,
	phraseTermBinding,
	phraseType,
        phraseTerm,
	phraseJudge)));

(* phraseEmpty ::=
	";"
*)
    env.Add(phraseEmpty.name,
      NEW(Parse.GivenDelimiter, delim:=';', Build:=BuildPhraseEmpty));

(* phraseSyntax ::=
	[ syntaxDecl ";" ]
*)
    env.Add(phraseSyntax.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
	  Parse.Store(1, synDecl),
	  NEW(Parse.GivenDelimiter, delim:=';'))),
        Build:=BuildPhraseSyntax));

(* phraseTypeBinding ::=
	[ "Let" typeBinding ";" ]
*)
    env.Add(phraseTypeBinding.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
	  NEW(Parse.GivenKeyword, key:=keyTypeLet),
	  Parse.Store(1, typeBinding),
	  NEW(Parse.GivenDelimiter, delim:=';'))),
	Build:=Select1));

(* phraseTermBinding ::=
	[ "let" termBinding ";" ]
*)
    env.Add(phraseTermBinding.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
	  NEW(Parse.GivenKeyword, key:=keyTermLet),
	  Parse.Store(1, termBinding),
	  NEW(Parse.GivenDelimiter, delim:=';'))),
	Build:=Select1));

(* phraseType ::=
	[ ":" type ";" ]
*)
    env.Add(phraseType.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
          NEW(Parse.GivenKeyword, key:=keyHasType),
          Parse.Store(1, type),
	  NEW(Parse.GivenDelimiter, delim:=';'))),
        Build:=Select1));

(* phraseTerm ::=
	[ term ";" ]
*)
    env.Add(phraseTerm.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
          Parse.Store(1, term),
	  NEW(Parse.GivenDelimiter, delim:=';'))),
        Build:=Select1));

    env.Add(phraseJudge.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
	  NEW(Parse.GivenKeyword, key:=keyJudge),
          Parse.Store(1,
	    NEW(Parse.Choice, choice:=Parse.List(
	      phraseJudegeContext,
	      phraseJudgeType,
	      phraseJudgeSubtype,
	      phraseJudgeTerm))),
	  NEW(Parse.GivenDelimiter, delim:=';'))),
	Build:=Select1));

    env.Add(phraseJudegeContext.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
          NEW(Parse.GivenName, text:="env"),
	  Parse.Store(1, context))),
	Build:=BuildPhraseJudgeContext));

    env.Add(phraseJudgeType.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
          NEW(Parse.GivenName, text:="type"),
	  Parse.Store(1, context),
	  NEW(Parse.GivenKeyword, key:=keyEntail),
	  Parse.Store(2, type))),
	Build:=BuildPhraseJudgeType));

    env.Add(phraseJudgeSubtype.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
          NEW(Parse.GivenName, text:="subtype"),
	  Parse.Store(1, context),
	  NEW(Parse.GivenKeyword, key:=keyEntail),
	  Parse.Store(2, type),
	  NEW(Parse.GivenKeyword, key:=keyHasSubtype),
	  Parse.Store(3, type))),
	Build:=BuildPhraseJudgeSubtype));

    env.Add(phraseJudgeTerm.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
          NEW(Parse.GivenName, text:="term"),
	  Parse.Store(1, context),
	  NEW(Parse.GivenKeyword, key:=keyEntail),
	  Parse.Store(2, term),
	  NEW(Parse.GivenKeyword, key:=keyHasType),
	  Parse.Store(3, type))),
	Build:=BuildPhraseJudgeTerm));

(* context ::=
	{ [ ide { [ "<:" type context] [ ":" type context] }]
	  [] }

*)
    env.Add(context.name,
      NEW(Parse.Choice, choice:=Parse.List(
        NEW(Parse.Action, grammar:=
          NEW(Parse.Sequence, items:=Parse.List(
	    Parse.Store(1, NEW(Parse.Identifier, Build:=BuildIdeName)),
	    Parse.Store(2, 
	      NEW(Parse.Choice, choice:=Parse.List(
		NEW(Parse.Action, grammar:=
		  NEW(Parse.Sequence, items:=Parse.List(
		    NEW(Parse.GivenKeyword, key:=keyHasSubtype),
		    Parse.Store(3, type),
		    Parse.Store(4, context))),
		  Build:=BuildContextType),
		NEW(Parse.Action, grammar:=
		  NEW(Parse.Sequence, items:=Parse.List(
		    NEW(Parse.GivenKeyword, key:=keyHasType),
		    Parse.Store(3, type),
		    Parse.Store(4, context))),
		  Build:=BuildContextTerm)))))),
	  Build:=Select2),
        NEW(Parse.Sequence, items:=NIL))));

    env.Add(binderIde.name,
      NEW(Parse.Identifier, Build:=BuildIdeName));

    env.Add(binderPos.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
      	  NEW(Parse.GivenDelimiter, delim:='_'),
	  Parse.Store(1, NEW(Parse.Integer, Build:=BuildInteger)))),
	Build:=Select1));

(* typeBinding ::=
	{ [ ide { [ "<:" type ] [] } "=" type typeBinding ] [] }
*)
    env.Add(typeBinding.name,
      NEW(Parse.Choice,	choice:=Parse.List(
	NEW(Parse.Action, grammar:=
          NEW(Parse.Sequence, items:=Parse.List(
	    Parse.Store(1, NEW(Parse.Identifier, Build:=BuildIdeName)),
            Parse.Store(2,
	      NEW(Parse.Choice, choice:=Parse.List(
	        NEW(Parse.Action, grammar:=
		  NEW(Parse.Sequence, items:=Parse.List(
		    NEW(Parse.GivenKeyword, key:=keyHasSubtype),
		    Parse.Store(3, type))),
		  Build:=Select3),
		NEW(Parse.Sequence, items:=NIL)))),
	    NEW(Parse.GivenKeyword, key:=keyEqDef),
	    Parse.Store(4, type),
	    Parse.Store(5, typeBinding))),
	  Build:=BuildTypeBinding),
        NEW(Parse.Sequence, items:=NIL))));

(* termBinding ::=
	{ [ ide { [ ":" type ] [] } "=" term termBinding ] [] }
*)
    env.Add(termBinding.name,
      NEW(Parse.Choice,	choice:=Parse.List(
	NEW(Parse.Action, grammar:=
          NEW(Parse.Sequence, items:=Parse.List(
	    Parse.Store(1, NEW(Parse.Identifier, Build:=BuildIdeName)),
            Parse.Store(2,
	      NEW(Parse.Choice, choice:=Parse.List(
	        NEW(Parse.Action, grammar:=
		  NEW(Parse.Sequence, items:=Parse.List(
		    NEW(Parse.GivenKeyword, key:=keyHasType),
		    Parse.Store(3, type))),
		  Build:=Select3),
		NEW(Parse.Sequence, items:=NIL)))),
	    NEW(Parse.GivenKeyword, key:=keyEqDef),
	    Parse.Store(4, term),
	    Parse.Store(5, termBinding))),
	  Build:=BuildTermBinding),
        NEW(Parse.Sequence, items:=NIL))));

(* type ::=
	[ typeOper_1
	  { [ "->" type_3 ]	=> BuildTypeArrow(_1,_3)
	    []			=> Select1(_1)
	  }_2
	]			=> Fetch _2
*)
    env.Add(type.name,
      NEW(Parse.Action, grammar :=
        NEW(Parse.Sequence, items:=Parse.List(
	  Parse.Store(1, typeOper),
	  Parse.Store(2,
	    NEW(Parse.Choice, choice :=Parse.List(
	      NEW(Parse.Action, grammar:=
		NEW(Parse.Sequence, items:=Parse.List(
		  NEW(Parse.GivenKeyword, key:=keyTypeArrow),
		  Parse.Store(3, type))),
		Build:=BuildTypeArrow),
	      NEW(Parse.Action, grammar:=
	        NEW(Parse.Sequence, items:=NIL),
		Build:=Select1)))))),
	Build:=Select2));

(* typeOper ::=			(hook for client infixes)
	( typeBase *_1 {} )
*)
    env.Add(typeOper.name,
      NEW(Parse.Iter,
	accum:=TRUE,
	accumPosition:=1,
	base:=typeBase,
	iter:=NEW(Parse.Choice, choice:=NIL)));

(* typeBase ::=
	{ typeIde typePos typeTop typeForall typeRec typeParen }
*)
    env.Add(typeBase.name,
      NEW(Parse.Choice, choice:=Parse.List(
	typeIde,
	typePos,
	typeTop,
        typeForall,
	typeRec,
        typeParen)));

(* typeIde ::=
	ide
*)
    env.Add(typeIde.name,
      NEW(Parse.Identifier, 
        Build:=BuildTypeIde));

(* typePos ::=
	[ "_" int ]
*)
    env.Add(typePos.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
      	  NEW(Parse.GivenDelimiter, delim:='_'),
	  Parse.Store(1, NEW(Parse.Integer, Build:=BuildInteger)))),
	Build:=BuildTypePos));

(* typeTop ::=
	"Top"
*)
    env.Add(typeTop.name,
      NEW(Parse.GivenKeyword, key:=keyTypeTop, Build:=BuildTypeTop));

(* typeForall ::=
	[ "All" "(" 
	  { [ binderIde { "?" [] } { [ "<:" type ] [] } ")" type ]
	    [ binderPos { "?" [] } { [ "<:" type ] [] } ")" type ] } ]
*)
    env.Add(typeForall.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
          NEW(Parse.GivenKeyword, key:=keyTypeForall),
          NEW(Parse.GivenDelimiter, delim:='('),
	  Parse.Store(1,
	    NEW(Parse.Choice, choice:=Parse.List(
	      NEW(Parse.Action, grammar:=
                NEW(Parse.Sequence, items:=Parse.List(
                  Parse.Store(2, binderIde),
		  Parse.Store(6,
		    NEW(Parse.Choice, choice:=Parse.List(
		      NEW(Parse.GivenDelimiter, delim:='?',
			Build:=BuildOmitParam),
		      NEW(Parse.Sequence, items:=NIL)))),
		  Parse.Store(3,
		    NEW(Parse.Choice, choice:=Parse.List(
		      NEW(Parse.Action, grammar:=
		        NEW(Parse.Sequence, items:=Parse.List(
                          NEW(Parse.GivenKeyword, key:=keyHasSubtype),
	                  Parse.Store(5, type))),
			Build:=Select5),
		      NEW(Parse.Sequence, items:=NIL)))),
		  NEW(Parse.GivenDelimiter, delim:=')'),
		  Parse.Store(4, type))),
                Build:=BuildTypeForallIdeBinder),
	      NEW(Parse.Action, grammar:=
                NEW(Parse.Sequence, items:=Parse.List(
                  Parse.Store(2, binderPos),
		  Parse.Store(6,
		    NEW(Parse.Choice, choice:=Parse.List(
		      NEW(Parse.GivenDelimiter, delim:='?',
			Build:=BuildOmitParam),
		      NEW(Parse.Sequence, items:=NIL)))),
		  Parse.Store(3,
		    NEW(Parse.Choice, choice:=Parse.List(
		      NEW(Parse.Action, grammar:=
		        NEW(Parse.Sequence, items:=Parse.List(
                          NEW(Parse.GivenKeyword, key:=keyHasSubtype),
	                  Parse.Store(5, type))),
			Build:=Select5),
		      NEW(Parse.Sequence, items:=NIL)))),
		  NEW(Parse.GivenDelimiter, delim:=')'),
		  Parse.Store(4, type))),
                Build:=BuildTypeForallPosBinder)))))),
        Build:=Select1));

(* typeRec ::=
	[ "Rec" "(" binderIde ")" type ]
*)
    env.Add(typeRec.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
          NEW(Parse.GivenKeyword, key:=keyTypeRec),
          NEW(Parse.GivenDelimiter, delim:='('),
          Parse.Store(1, binderIde),
	  NEW(Parse.GivenDelimiter, delim:=')'),
	  Parse.Store(2, type))),
        Build:=BuildTypeRec));

(* typeParen ::=
	[ "{" type "}" ]
*)
    env.Add(typeParen.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
          NEW(Parse.GivenDelimiter, delim:='{'),
          Parse.Store(1, type),
          NEW(Parse.GivenDelimiter, delim:='}'))),
        Build:=Select1));

(* term ::= 
	termOper

   termOper ::=
	(termAppl *_1 {} )

   termAppl ::=
	( termBase *_1 
	  { [ "(" 
	      { [ ":" type_3 ] 	=> BuildTermAppl2(_1,_3)
	        term_3 		=> BuildTermAppl1(_1,_3)
	      }_2 
	      ")"
	    ] 			=> Fetch _2
	    "!"
          }
	)
*)
    env.Add(term.name, termOper);

    env.Add(termOper.name,
      NEW(Parse.Iter,
	accum:=TRUE,
	accumPosition:=1,
	base:=termAppl,
	iter:=NEW(Parse.Choice, choice:=NIL)));

    env.Add(termAppl.name,
      NEW(Parse.Iter,
	accum:=TRUE,
	accumPosition:=1,
	base:=termBase,
	iter:=
	NEW(Parse.Choice, choice:=Parse.List(
	  NEW(Parse.Action, grammar:=
            NEW(Parse.Sequence, items:=Parse.List(
              NEW(Parse.GivenDelimiter, delim:='('),
	      Parse.Store(2,
		NEW(Parse.Choice, choice:=Parse.List(
	          NEW(Parse.Action, grammar:=
		    NEW(Parse.Sequence, items:=Parse.List(
		      NEW(Parse.GivenKeyword, key:=keyHasType),
		      Parse.Store(3, type))),
		    Build:=BuildTermAppl2),
	          NEW(Parse.Action, grammar:=
		    Parse.Store(3, term),
		    Build:=BuildTermAppl1)))),
              NEW(Parse.GivenDelimiter, delim:=')'))), 
            Build:=Select2),
	  NEW(Parse.Action, grammar:=
	    NEW(Parse.GivenDelimiter, delim:='!'),
	    Build:=BuildTermIdeOmit)))));

(* termBase ::=
	{ termIde termPos termTop termFun termFold termUnfold termRec
	  termParen synTerm }
*)
    env.Add(termBase.name,
      NEW(Parse.Choice, choice:=Parse.List(
	termIde,
	termPos,
	termTop,
        termFun,
        termFold,
        termUnfold,
        termRec,
        termParen,
	synTerm)));

(* termIde ::=
	ide
*)
    env.Add(termIde.name,
      NEW(Parse.Identifier, 
        Build:=BuildTermIde));

(* termPos ::=
	[ "_" int ]
*)
    env.Add(termPos.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
      	  NEW(Parse.GivenDelimiter, delim:='_'),
	  Parse.Store(1, NEW(Parse.Integer, Build:=BuildInteger)))),
	Build:=BuildTermPos));

(* termTop ::=
	"top"
*)
    env.Add(termTop.name,
      NEW(Parse.GivenKeyword, key:=keyTermTop, Build:=BuildTermTop));

(* termFun ::=
	[ "fun" "("
	  { [ binderIde { "?" [] }
	      { [ ":" type ")" ] term ]
	        [ ")" term ]
	        [ "<:" type ")" term ] }
	    [ binderPos { "?" [] }
	      { [ ":" type ")" ] term ]
	        [ ")" term ]
	        [ "<:" type ")" term ] } } ]
*)	    
    env.Add(termFun.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
          NEW(Parse.GivenKeyword, key:=keyTermFun),
          NEW(Parse.GivenDelimiter, delim:='('),
	  Parse.Store(1,
	    NEW(Parse.Choice, choice:=Parse.List(
	      NEW(Parse.Action, grammar:=
	        NEW(Parse.Sequence, items:=Parse.List(
		  Parse.Store(2, binderIde),
		  Parse.Store(6,
		    NEW(Parse.Choice, choice:=Parse.List(
		      NEW(Parse.GivenDelimiter, delim:='?',
			Build:=BuildOmitParam),
		      NEW(Parse.Sequence, items:=NIL)))),
		  Parse.Store(3,
		    NEW(Parse.Choice, choice:=Parse.List(
		      NEW(Parse.Action, grammar:=
			NEW(Parse.Sequence, items:=Parse.List(
			  NEW(Parse.GivenKeyword, key:=keyHasType),
			  Parse.Store(4, type),
			  NEW(Parse.GivenDelimiter, delim:=')'),
			  Parse.Store(5, term))),
			Build:=BuildTermFunIde1),
		      NEW(Parse.Action, grammar:=
			NEW(Parse.Sequence, items:=Parse.List(
			  NEW(Parse.GivenDelimiter, delim:=')'),
			  Parse.Store(5, term))),
			Build:=BuildTermFunIde2NoBound),
		      NEW(Parse.Action, grammar:=
			NEW(Parse.Sequence, items:=Parse.List(
			  NEW(Parse.GivenKeyword, key:=keyHasSubtype),
			  Parse.Store(4, type),
			  NEW(Parse.GivenDelimiter, delim:=')'),
			  Parse.Store(5, term))),
			Build:=BuildTermFunIde2)))))),
		Build:=Select3),
	      NEW(Parse.Action, grammar:=
	        NEW(Parse.Sequence, items:=Parse.List(
		  Parse.Store(2, binderPos),
		  Parse.Store(6,
		    NEW(Parse.Choice, choice:=Parse.List(
		      NEW(Parse.GivenDelimiter, delim:='?',
			Build:=BuildOmitParam),
		      NEW(Parse.Sequence, items:=NIL)))),
		  Parse.Store(3,
		    NEW(Parse.Choice, choice:=Parse.List(
		      NEW(Parse.Action, grammar:=
			NEW(Parse.Sequence, items:=Parse.List(
			  NEW(Parse.GivenKeyword, key:=keyHasType),
			  Parse.Store(4, type),
			  NEW(Parse.GivenDelimiter, delim:=')'),
			  Parse.Store(5, term))),
			Build:=BuildTermFunPos1),
		      NEW(Parse.Action, grammar:=
			NEW(Parse.Sequence, items:=Parse.List(
			  NEW(Parse.GivenDelimiter, delim:=')'),
			  Parse.Store(5, term))),
			Build:=BuildTermFunPos2NoBound),
		      NEW(Parse.Action, grammar:=
			NEW(Parse.Sequence, items:=Parse.List(
			  NEW(Parse.GivenKeyword, key:=keyHasSubtype),
			  Parse.Store(4, type),
			  NEW(Parse.GivenDelimiter, delim:=')'),
			  Parse.Store(5, term))),
			Build:=BuildTermFunPos2)))))),
		Build:=Select3)))))),
	Build:=Select1));

(* termFold ::=
	[ "fold" "(" ":" type ")" "(" term ")" ]
*)	    
    env.Add(termFold.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
          NEW(Parse.GivenKeyword, key:=keyTermFold),
          NEW(Parse.GivenDelimiter, delim:='('),
          NEW(Parse.GivenKeyword, key:=keyHasType),
	  Parse.Store(1, type),
          NEW(Parse.GivenDelimiter, delim:=')'),
          NEW(Parse.GivenDelimiter, delim:='('),
	  Parse.Store(2, term),
          NEW(Parse.GivenDelimiter, delim:=')'))),
	Build:=BuildTermFold));

(* termUnfold ::=
	[ "unfold" "(" term ")" ]
*)	    
    env.Add(termUnfold.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
          NEW(Parse.GivenKeyword, key:=keyTermUnfold),
          NEW(Parse.GivenDelimiter, delim:='('),
	  Parse.Store(1, term),
          NEW(Parse.GivenDelimiter, delim:=')'))),
	Build:=BuildTermUnfold));

(* termRec ::=
	[ "rec" "(" binderIde ":" type ")" term ]
*)	    
    env.Add(termRec.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
          NEW(Parse.GivenKeyword, key:=keyTermRec),
          NEW(Parse.GivenDelimiter, delim:='('),
	  Parse.Store(1, binderIde),
          NEW(Parse.GivenKeyword, key:=keyHasType),
	  Parse.Store(2, type),
          NEW(Parse.GivenDelimiter, delim:=')'),
	  Parse.Store(3, term))),
	Build:=BuildTermRec));

(* termParen ::=
	[ "{" term "}" ]
*)
    env.Add(termParen.name,
       NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
          NEW(Parse.GivenDelimiter, delim:='{'),
          Parse.Store(1, term),
          NEW(Parse.GivenDelimiter, delim:='}'))),
        Build:=Select1));
     
  END Setup;

BEGIN
END Tree.
