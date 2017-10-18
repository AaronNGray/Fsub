(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(*                                                             *)
(* Created by Luca Cardelli                                    *)
(* Last modified on Sat Aug 15 20:22:08 PDT 1998 by heydon     *)

MODULE Gram;
IMPORT String, Err, Text, Out, Formatter, Scanner, Parse, Act;

  REVEAL
    Parse.Grammar = Parse.Tree BRANDED OBJECT END;
    Parse.GrammarListRoot = Parse.Tree BRANDED OBJECT END;
    Parse.GivenKeyword =
      Parse.GivenKeywordBase BRANDED OBJECT
	ide: TEXT;
      END;
    Parse.Action =
      Parse.ActionBase BRANDED OBJECT
        action: Parse.Tree;
      END;

    GramInfo =
      GramInfoBase BRANDED OBJECT
	clauseList: ClauseList;
	oldKeySet, newKeySet: Scanner.KeywordSet;
      END;

TYPE
  TempFlag =
    Parse.Tree BRANDED OBJECT END;

  IntegerTemp =
    Parse.Tree BRANDED OBJECT
      int: INTEGER;
    END;

  IdeNode =
    Parse.Tree BRANDED OBJECT
      ide: TEXT;
    END;

  ClauseList =
    Parse.Tree BRANDED OBJECT
      ide: IdeNode;
      extend, extendIter, iterPosPresent: BOOLEAN; iterPos: INTEGER;
      gram: Parse.Grammar;
      inserted: BOOLEAN:=FALSE;
      rest:ClauseList;
    END;

  ClauseExtends =
    Parse.Tree BRANDED OBJECT
      extend, iter, iterPosPresent: BOOLEAN;
      iterPos: INTEGER;
    END;

  PROCEDURE MergeEnv(list: ClauseList; env: Parse.GrammarEnv) RAISES ANY =
    BEGIN
      LOOP
        IF list=NIL THEN RETURN END;
	IF list.extend THEN
	  IF list.extendIter THEN
	    env.ExtendIter(list.ide.ide, list.iterPosPresent, list.iterPos,
		list.gram);
	  ELSE
	    env.Extend(list.ide.ide, list.gram);
	  END;
	ELSE
	  env.Add(list.ide.ide, list.gram);
	END;
	list.inserted := TRUE;
        list := list.rest;
      END;
    END MergeEnv;

  PROCEDURE UndoMergeEnv(list: ClauseList; env: Parse.GrammarEnv) RAISES ANY =
    BEGIN
      LOOP
        IF list=NIL THEN RETURN END;
	IF list.inserted THEN
	  IF list.extend THEN
	    IF list.extendIter THEN
	      env.UndoExtendIter(list.ide.ide, list.gram);
	    ELSE
	      env.UndoExtend(list.ide.ide, list.gram);
	    END;
	  ELSE
	    env.UndoAdd(list.ide.ide);
	  END;
	END;
	list.inserted := FALSE;
        list := list.rest;
      END;
    END UndoMergeEnv;

  PROCEDURE BeKeywords(list: ClauseList; keySet: Scanner.KeywordSet) RAISES ANY =
    BEGIN
      LOOP
        IF list=NIL THEN RETURN END;
	BeKeywordsOfGram(list.gram, keySet);
        list := list.rest;
      END;
    END BeKeywords;

  PROCEDURE BeKeywordsOfGramList(list: Parse.GrammarList; keySet: Scanner.KeywordSet) RAISES ANY =
    BEGIN
      LOOP
        IF list=NIL THEN RETURN END;
	BeKeywordsOfGram(list.first, keySet);
        list := list.rest;
      END;
    END BeKeywordsOfGramList;

  PROCEDURE BeKeywordsOfGram(gram: Parse.Grammar; keySet: Scanner.KeywordSet) RAISES ANY =
    BEGIN
      TYPECASE gram OF <*NOWARN*>
      | NULL =>
      | Parse.NonTerminal =>
      | Parse.Storage(node) => BeKeywordsOfGram(node.item, keySet);
      | Parse.Action(node) => BeKeywordsOfGram(node.grammar, keySet);
      | Parse.EnvCapture(node) => BeKeywordsOfGram(node.grammar, keySet);
      | Parse.GivenKeyword(node) => 
	  node.key := Scanner.BeKeyword(node.ide, keySet);
      | Parse.Identifier, Parse.QuotedChar, Parse.Integer, Parse.Real,
	Parse.QuotedString, Parse.GivenDelimiter =>
      | Parse.Sequence(node) => 
	  BeKeywordsOfGramList(node.items, keySet);
      | Parse.Choice(node) => BeKeywordsOfGramList(node.choice, keySet);
      | Parse.Iter(node) =>
	  BeKeywordsOfGram(node.base, keySet); 
	  BeKeywordsOfGram(node.iter, keySet);
      END;
    END BeKeywordsOfGram;

  PROCEDURE PrintClauseList(fmt: Formatter.T; list: ClauseList) RAISES ANY =
  BEGIN
    Formatter.PutText(fmt, "syntax for:");
    WHILE list#NIL DO
      Formatter.PutText(fmt, " " & list.ide.ide);
      (* IF list.extend OR list.extendIter THEN
         Formatter.PutText(fmt, "(extended)");
      END; *)
      list:=list.rest;
    END;
    Formatter.PutChar(fmt, '\n');
  END PrintClauseList;

  PROCEDURE BuildInteger(<*UNUSED*> self: Parse.Integer; int: INTEGER;
	<*UNUSED*> READONLY info: Err.LocationInfo): Parse.Tree =
    BEGIN
      RETURN NEW(IntegerTemp, int:=int);
    END BuildInteger;

  PROCEDURE BuildIde(<*UNUSED*> self: Parse.Identifier; name: TEXT;
	<*UNUSED*> READONLY info: Err.LocationInfo): Parse.Tree =
    BEGIN
      RETURN NEW(IdeNode, ide:=name);
    END BuildIde;

  PROCEDURE BuildName(<*UNUSED*> self: Parse.GivenName;
	<*UNUSED*> READONLY info: Err.LocationInfo): Parse.Tree =
    BEGIN
      RETURN NEW(TempFlag);
    END BuildName;

  PROCEDURE BuildSyntaxDecl(<*UNUSED*> self: Parse.EnvCapture; 
	base: INTEGER; env: Parse.GrammarEnv;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    VAR clauseList: ClauseList; topGram: Parse.Grammar;
      oldKeySet, newKeySet: Scanner.KeywordSet;
    BEGIN
      clauseList := NARROW(Parse.Stack[base+1], ClauseList);
      oldKeySet := Scanner.GetKeywordSet();
      TRY
        newKeySet := Scanner.CopyKeywordSet(oldKeySet);
	(* -- or: newKeySet := Scanner.NewKeywordSet(); *)
	(* -- there should be a way, automatic or explicity
	  to decide whether the old keyword should be imported
	  in the new lexical environment *)
        Scanner.UseKeywordSet(newKeySet);
        BeKeywords(clauseList, newKeySet);
        MergeEnv(clauseList, env);
	IF Scanner.TopLevel() THEN PrintClauseList(Out.out, clauseList) END;
        topGram := NEW(Parse.NonTerminal,
	  location:=Err.NewLineLocation(info),
	  name:=clauseList.ide.ide);
      EXCEPT Err.Fail => 
	UndoMergeEnv(clauseList, env); 
	Scanner.UseKeywordSet(oldKeySet);
	RAISE Err.Fail;
      END;
      RETURN 
	NEW(GramInfo, topGram:=topGram, env:=env,
	  adoptAsTopLevelGrammar:=Parse.Stack[base+2]#NIL,
          clauseList:=clauseList, 
	  oldKeySet:=oldKeySet, newKeySet:=newKeySet);
    END BuildSyntaxDecl;

  PROCEDURE UndoSyntaxDecl(info: GramInfo) RAISES ANY =
    BEGIN
      UndoMergeEnv(info.clauseList, info.env);
      Scanner.UseKeywordSet(info.oldKeySet);
    END UndoSyntaxDecl;

  PROCEDURE BuildSyntax(<*UNUSED*> self: Parse.EnvCapture; 
	base: INTEGER; env: Parse.GrammarEnv;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    VAR tree: Parse.Tree; clauseList: ClauseList;
      topGram: Parse.Grammar; oldKeySet, newKeySet: Scanner.KeywordSet;
      ide: TEXT; 
    BEGIN
      clauseList := NARROW(Parse.Stack[base+1], ClauseList);
      oldKeySet := Scanner.GetKeywordSet();
      TRY
	newKeySet := Scanner.CopyKeywordSet(oldKeySet);
	(* -- or: newKeySet := Scanner.NewKeywordSet(); *)
	(* -- there should be a way, automatic or explicity
	  to decide whether the old keyword should be imported
	  in the new lexical environment *)
	Scanner.UseKeywordSet(newKeySet);
	BeKeywords(clauseList, newKeySet);
        MergeEnv(clauseList, env);
	topGram := NEW(Parse.NonTerminal, 
	  location:=Err.NewLineLocation(info),
	  name:=clauseList.ide.ide);
        tree := Parse.Read(topGram, env, base+2);
	Scanner.UseKeywordSet(oldKeySet);
	IF Scanner.HaveTokenKey(keyEnd) THEN
	ELSIF Scanner.GetTokenIde((*out*) ide) AND Text.Equal(ide, "end") THEN
	ELSE Scanner.Syntax("Parse failed", " at \'end\' of \'syntax\'") 
	END;
      FINALLY
	UndoMergeEnv(clauseList, env);
	Scanner.UseKeywordSet(oldKeySet);
      END;
      IF tree=NIL THEN RETURN Act.DefaultTree(Err.NewLineLocation(info));
      ELSE RETURN tree;
      END;
    END BuildSyntax;

  PROCEDURE BuildGrammar(<*UNUSED*> self:Parse.Action; base: INTEGER;
	<*UNUSED*> READONLY info: Err.LocationInfo): Parse.Tree =
    VAR list: ClauseList;
    BEGIN
      list := NARROW(Parse.Stack[base+1], ClauseList);
(* -- check that names in list are unique, whether extensions or not. *)
      RETURN list;
    END BuildGrammar;

  PROCEDURE BuildClauseList(<*UNUSED*> self: Parse.Action; base: INTEGER;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    VAR clauseExtends: ClauseExtends;
    BEGIN
      clauseExtends:=NARROW(Parse.Stack[base+2], ClauseExtends);
      RETURN 
        NEW(ClauseList, location:=Err.NewLineLocation(info),
	  ide:=NARROW(Parse.Stack[base+1], IdeNode),
	  extend:=clauseExtends.extend,
	  extendIter:=clauseExtends.iter,
	  iterPosPresent:=clauseExtends.iterPosPresent,
	  iterPos:=clauseExtends.iterPos,
	  gram:=NARROW(Parse.Stack[base+3], Parse.Grammar),
	  rest:=NARROW(Parse.Stack[base+4], ClauseList)); 
    END BuildClauseList;

  PROCEDURE BuildClauseExtendsChoice(<*UNUSED*> self: Parse.Action;
        <*UNUSED*> base: INTEGER; <*UNUSED*> READONLY info: Err.LocationInfo)
        : Parse.Tree =
    BEGIN
      RETURN NEW(ClauseExtends, extend:=TRUE, iter:=FALSE,
	iterPosPresent:=FALSE, iterPos:=0);
    END BuildClauseExtendsChoice;

  PROCEDURE BuildClauseExtendsIterPos(<*UNUSED*> self: Parse.Action; 
        base: INTEGER; <*UNUSED*> READONLY info: Err.LocationInfo)
        : Parse.Tree =
    BEGIN
      RETURN NEW(ClauseExtends, extend:=TRUE, iter:=TRUE,
	iterPosPresent:=TRUE, 
	iterPos:=NARROW(Parse.Stack[base+3], IntegerTemp).int);
    END BuildClauseExtendsIterPos;

  PROCEDURE BuildClauseExtendsIterNoPos(<*UNUSED*> self: Parse.Action;
        <*UNUSED*> base: INTEGER; <*UNUSED*> READONLY info: Err.LocationInfo)
        : Parse.Tree =
    BEGIN
      RETURN NEW(ClauseExtends, extend:=TRUE, iter:=TRUE,
	iterPosPresent:=FALSE, iterPos:=0);
    END BuildClauseExtendsIterNoPos;

  PROCEDURE BuildClauseExtendsIter(<*UNUSED*> self: Parse.Action;
        base: INTEGER; <*UNUSED*> READONLY info: Err.LocationInfo)
        : Parse.Tree =
    BEGIN
      RETURN Parse.Stack[base+2];
    END BuildClauseExtendsIter;

  PROCEDURE BuildClauseExtendsNo(<*UNUSED*> self: Parse.Action;
        <*UNUSED*> base: INTEGER; <*UNUSED*> READONLY info: Err.LocationInfo)
        : Parse.Tree =
    BEGIN
      RETURN NEW(ClauseExtends, extend:=FALSE, iter:=FALSE);
    END BuildClauseExtendsNo;

  PROCEDURE BuildClauseExtendsYes(<*UNUSED*> self: Parse.Action; base: INTEGER;
	<*UNUSED*> READONLY info: Err.LocationInfo): Parse.Tree =
    BEGIN
      RETURN Parse.Stack[base+1];
    END BuildClauseExtendsYes;

  PROCEDURE BuildGramIde(<*UNUSED*> self: Parse.Identifier; name: TEXT;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(Parse.NonTerminal, location:=Err.NewLineLocation(info),
 	name:=name);
    END BuildGramIde;

  PROCEDURE BuildGramString(<*UNUSED*> self: Parse.QuotedString;
        string: String.T; READONLY info: Err.LocationInfo)
      : Parse.Tree RAISES ANY =
    BEGIN
      IF String.Length(string)=0 THEN 
	Err.Fault(Out.out, "Invalid token: \"\"") 
      END;
      IF (String.Length(string)=1) AND Scanner.IsDelimiter(string[0]) THEN
	RETURN 
	  NEW(Parse.GivenDelimiter, location:=Err.NewLineLocation(info),
	    delim:=string[0], 
	    Build:=Act.BuildActionDelimiter);
      ELSIF Scanner.IsIdentifier(string) THEN
        RETURN 
	    (* Fill the key field later; store it in ide for now. *)
	    NEW(Parse.GivenKeyword, location:=Err.NewLineLocation(info),
	      ide:=String.ToText(string), key:=NIL,
	      Build:=Act.BuildActionKeyword);
      ELSE
        <*NOWARN*> Err.Fault(Out.out, "Invalid token: "&String.ToText(string));
      END;
    END BuildGramString;

  PROCEDURE BuildGramKeyIde(<*UNUSED*> self: Parse.GivenKeyword;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(Parse.Identifier, location:=Err.NewLineLocation(info),
	Build:=Act.BuildActionIdentifier);
    END BuildGramKeyIde;

  PROCEDURE BuildGramKeyInt(<*UNUSED*> self: Parse.GivenKeyword;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(Parse.Integer, location:=Err.NewLineLocation(info),
	Build:=Act.BuildActionInteger);
    END BuildGramKeyInt;

  PROCEDURE BuildGramKeyReal(<*UNUSED*> self: Parse.GivenKeyword;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(Parse.Real, location:=Err.NewLineLocation(info),
	Build:=Act.BuildActionReal);
    END BuildGramKeyReal;

  PROCEDURE BuildGramKeyChar(<*UNUSED*> self: Parse.GivenKeyword;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(Parse.QuotedChar, location:=Err.NewLineLocation(info),
	Build:=Act.BuildActionChar);
    END BuildGramKeyChar;

  PROCEDURE BuildGramKeyString(<*UNUSED*> self: Parse.GivenKeyword;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(Parse.QuotedString, location:=Err.NewLineLocation(info),
	Build:=Act.BuildActionString);
    END BuildGramKeyString;

  PROCEDURE BuildGramList(<*UNUSED*> self: Parse.Action; base: INTEGER;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN 
        NEW(Parse.GrammarList, location:=Err.NewLineLocation(info),
	  first:=Parse.Stack[base+1],
	  rest:=Parse.Stack[base+2]);
    END BuildGramList;

  PROCEDURE BuildActionPattern(self: Parse.Action; base: INTEGER;
	<*UNUSED*> READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN Act.InstantiatePattern(self.action, base);
    END BuildActionPattern;

  PROCEDURE BuildStorage(<*UNUSED*> self: Parse.Action; base: INTEGER;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(Parse.Storage, location:=Err.NewLineLocation(info),
	position:=NARROW(Parse.Stack[base+3], IntegerTemp).int,
	item:=Parse.Stack[base+1]);
    END BuildStorage;

  PROCEDURE BuildGramExpSequence(<*UNUSED*> self: Parse.Action; base: INTEGER;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(Parse.Sequence, location:=Err.NewLineLocation(info),
	items:=Parse.Stack[base+1]);
    END BuildGramExpSequence;

  PROCEDURE BuildGramExpChoice(<*UNUSED*> self: Parse.Action; base: INTEGER;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(Parse.Choice, location:=Err.NewLineLocation(info),
	choice:=Parse.Stack[base+1]);
    END BuildGramExpChoice;

  PROCEDURE BuildGramExpParens(<*UNUSED*> self: Parse.Action; base: INTEGER;
	<*UNUSED*> READONLY info: Err.LocationInfo): Parse.Tree =
    BEGIN
      RETURN Parse.Stack[base+6];
    END BuildGramExpParens;

  PROCEDURE BuildGramExpBase(<*UNUSED*> self: Parse.Action; base: INTEGER;
	<*UNUSED*> READONLY info: Err.LocationInfo): Parse.Tree =
    BEGIN
      RETURN Parse.Stack[base+1];
    END BuildGramExpBase;

  PROCEDURE BuildGramExpIter(<*UNUSED*> self: Parse.Action; base: INTEGER;
	<*UNUSED*> READONLY info: Err.LocationInfo): Parse.Tree =
    BEGIN
      RETURN Parse.Stack[base+5];
    END BuildGramExpIter;

  PROCEDURE BuildGramExpIterNoPos(<*UNUSED*> self: Parse.Action; base: INTEGER;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(Parse.Iter, location:=Err.NewLineLocation(info),
	base:=Parse.Stack[base+1], 
	iter:=Parse.Stack[base+3],
	accum:=FALSE,
	accumPosition:=0);
    END BuildGramExpIterNoPos;

  PROCEDURE BuildGramExpIterPos(<*UNUSED*> self: Parse.Action; base: INTEGER;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      RETURN NEW(Parse.Iter, location:=Err.NewLineLocation(info),
	base:=Parse.Stack[base+1], 
	iter:=Parse.Stack[base+3],
	accum:=TRUE,
        accumPosition:=NARROW(Parse.Stack[base+4], IntegerTemp).int);
    END BuildGramExpIterPos;

  PROCEDURE BuildTermAction(<*UNUSED*> self: Parse.Action; base: INTEGER;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      Act.CheckPattern(Parse.Stack[base+3]);
      RETURN NEW(Parse.Action, location:=Err.NewLineLocation(info),
	grammar:=Parse.Stack[base+1],
	action:=Parse.Stack[base+3],
	Build:=BuildActionPattern);
    END BuildTermAction;

  PROCEDURE BuildTypeAction(<*UNUSED*> self: Parse.Action; base: INTEGER;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
    BEGIN
      Act.CheckPattern(Parse.Stack[base+3]);
      RETURN NEW(Parse.Action, location:=Err.NewLineLocation(info),
	grammar:=Parse.Stack[base+1],
	action:=Parse.Stack[base+3],
	Build:=BuildActionPattern);
    END BuildTypeAction;

  PROCEDURE BuildSingle(<*UNUSED*> self: Parse.Action; base: INTEGER;
	<*UNUSED*> READONLY info: Err.LocationInfo): Parse.Tree =
    BEGIN
      RETURN Parse.Stack[base+1];
    END BuildSingle;

  PROCEDURE BuildGramExp(<*UNUSED*> self: Parse.Action; base: INTEGER;
	<*UNUSED*> READONLY info: Err.LocationInfo): Parse.Tree =
    BEGIN
      RETURN Parse.Stack[base+2];
    END BuildGramExp;

  VAR 
    keySyntax, keyIn, keyDefSyn, keyIde, keyInt, keyReal, 
      keyChar, keyString, keyTermSem, keyTypeSem, keyStar, keyEq: Scanner.Keyword;
    synDecl, synTerm, grammar, clauseRest, clauseSeq, clauseExtends,
      gramExp, gramExpBase, gramExpIde, gramExpSequence, 
      gramExpChoice, gramExpParens, gramExpIter, gramExpList, 
      action, actionTermExp, actionTypeExp: Parse.NonTerminal;

  PROCEDURE Setup() RAISES ANY =
  BEGIN
    keySet := Scanner.NewKeywordSet();
    Scanner.UseKeywordSet(keySet);
    env := Parse.NewEnv();

    keySyntax := Scanner.BeKeyword("syntax", keySet);
    keyIn := Scanner.BeKeyword("in", keySet);
    keyDefSyn := Scanner.BeKeyword("::=", keySet);    
    keyIde := Scanner.BeKeyword("ide", keySet);
    keyInt := Scanner.BeKeyword("int", keySet);
    keyReal := Scanner.BeKeyword("real", keySet);
    keyChar := Scanner.BeKeyword("char", keySet);
    keyString := Scanner.BeKeyword("string", keySet);
    keyTermSem := Scanner.BeKeyword("=>", keySet);
    keyTypeSem := Scanner.BeKeyword(":>", keySet);
    keyStar := Scanner.BeKeyword("*", keySet);
    keyEq := Scanner.BeKeyword("=", keySet);
    keyEnd := Scanner.BeKeyword("end", keySet);

    synDecl := NEW(Parse.NonTerminal, name:="*synDecl");
    synTerm := NEW(Parse.NonTerminal, name:="*synTerm");
    grammar := NEW(Parse.NonTerminal, name:="*grammar");
    clauseSeq := NEW(Parse.NonTerminal, name:="*clauseSeq");
    clauseRest := NEW(Parse.NonTerminal, name:="*clauseRest");
    clauseExtends := NEW(Parse.NonTerminal, name:="*clauseExtends");
    gramExp := NEW(Parse.NonTerminal, name:="*gram");
    gramExpBase := NEW(Parse.NonTerminal, name:="*gramBase");
    gramExpIde := NEW(Parse.NonTerminal, name:="*gramIdent");
    gramExpSequence := NEW(Parse.NonTerminal, name:="*gramSequence");
    gramExpChoice := NEW(Parse.NonTerminal, name:="*gramChoice");
    gramExpParens := NEW(Parse.NonTerminal, name:="*gramParens");
    gramExpIter := NEW(Parse.NonTerminal, name:="*gramIter");
    gramExpList := NEW(Parse.NonTerminal, name:="*gramList");
    action := NEW(Parse.NonTerminal, name:="*action");
  END Setup;

  VAR keyEnd: Scanner.Keyword;

  PROCEDURE InitGrammars(
	VAR (*out*) syntaxTerm, syntaxDecl: Parse.NonTerminal) RAISES ANY =
  BEGIN

    Act.GetGrammars((*out*) actionTermExp, (*out*) actionTypeExp);

    syntaxTerm := synTerm;
    syntaxDecl := synDecl;

(* synDecl ::=
	[ "syntax" grammar ]
*)
    env.Add(synDecl.name,
      NEW(Parse.EnvCapture, grammar :=
	NEW(Parse.Sequence, items:=
	  Parse.List(
	    NEW(Parse.GivenKeyword, key:=keySyntax),
	    Parse.Store(2,
	      NEW(Parse.Choice, choice:=Parse.List(
		NEW(Parse.GivenName, text:="toplevel", Build:=BuildName),
		NEW(Parse.Sequence, items:=NIL)))),
	    Parse.Store(1, grammar))),
	Build:=BuildSyntaxDecl));

(* synTerm ::=
	[ "syntax" grammar "in" ... "end" ]
*)
    env.Add(synTerm.name,
      NEW(Parse.EnvCapture, grammar:=
        NEW(Parse.Sequence, items:=
	  Parse.List(
            NEW(Parse.GivenKeyword, key:=keySyntax), 
            Parse.Store(1, grammar),
            NEW(Parse.GivenKeyword, key:=keyIn))), 
        Build:=BuildSyntax));

(* grammar ::=
	clauseSeq
*)
    env.Add(grammar.name,
      NEW(Parse.Action,
	grammar:=Parse.Store(1, clauseSeq),
	Build:=BuildGrammar));

(* clauseSeq ::=
	[ gramExpIde "::=" clauseExtends gramExp clauseRest ]
*)
    env.Add(clauseSeq.name,
      NEW(Parse.Action, grammar :=
        NEW(Parse.Sequence, items:=Parse.List(
	  Parse.Store(1, gramExpIde),
	  NEW(Parse.GivenKeyword, key:=keyDefSyn),
	  Parse.Store(2, clauseExtends),
	  Parse.Store(3, gramExp),
	  Parse.Store(4, clauseRest))),
	Build:=BuildClauseList));

(* clauseRest ::=
	{ clauseSeq [] }
*)
    env.Add(clauseRest.name,
      NEW(Parse.Choice,	choice:=Parse.List(
	clauseSeq,
	NEW(Parse.Sequence, items:=NIL))));

(* clauseExtends ::=
	{ [ "." "." "." { [ "*" { [ "_" int ] [] } ] [] } [] }
*)
    env.Add(clauseExtends.name,
      NEW(Parse.Choice, choice:=Parse.List(
	NEW(Parse.Action, grammar:=
	  NEW(Parse.Sequence, items:=Parse.List(
	    NEW(Parse.GivenDelimiter, delim:='.'),
	    NEW(Parse.GivenDelimiter, delim:='.'),
	    NEW(Parse.GivenDelimiter, delim:='.'),
	    Parse.Store(1,
	      NEW(Parse.Choice, choice:=Parse.List(
	        NEW(Parse.Action, grammar:=
		  NEW(Parse.Sequence, items:=Parse.List(
		    NEW(Parse.GivenKeyword, key:=keyStar),
		    Parse.Store(2,
		      NEW(Parse.Choice, choice:=Parse.List(
		        NEW(Parse.Action, grammar:=
			  NEW(Parse.Sequence, items:=Parse.List(
			    NEW(Parse.GivenDelimiter, delim:='_'),
			    Parse.Store(3, 
			      NEW(Parse.Integer, Build:=BuildInteger)))),
			  Build:=BuildClauseExtendsIterPos),
		        NEW(Parse.Action, grammar:=
			  NEW(Parse.Sequence, items:=NIL),
			  Build:=BuildClauseExtendsIterNoPos)))))),
		  Build:=BuildClauseExtendsIter),
	        NEW(Parse.Action, grammar:=
		  NEW(Parse.Sequence, items:=NIL),
		  Build:=BuildClauseExtendsChoice)))))),
	  Build:=BuildClauseExtendsYes),
	NEW(Parse.Action,
	  grammar := NEW(Parse.Sequence, items:=NIL),
	  Build:=BuildClauseExtendsNo))));

(* gramExpIde ::=
	ide
*)
    env.Add(gramExpIde.name,
      NEW(Parse.Identifier, 
        Build:=BuildIde));

(* gramExp ::=
	[ gramExpBase 
	  { [ "=>" actionTermExp ] 
	    [ ":> actionTypeExp ]
	    [ "_" int ]
	    [] } ]
*)
    env.Add(gramExp.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
	  Parse.Store(1, gramExpBase),
          Parse.Store(2,
	    NEW(Parse.Choice, choice :=Parse.List(
	      NEW(Parse.Action, grammar:=
	        NEW(Parse.Sequence, items:=Parse.List(
	          NEW(Parse.GivenKeyword, key:=keyTermSem),
	          Parse.Store(3, actionTermExp))),
	        Build:=BuildTermAction),
	      NEW(Parse.Action, grammar:=
	        NEW(Parse.Sequence, items:=Parse.List(
	          NEW(Parse.GivenKeyword, key:=keyTypeSem),
	          Parse.Store(3, actionTypeExp))),
	        Build:=BuildTypeAction),
      	      NEW(Parse.Action, grammar:=
		NEW(Parse.Sequence, items:=Parse.List(
	 	  NEW(Parse.GivenDelimiter, delim:='_'),
		  Parse.Store(3, NEW(Parse.Integer, Build:=BuildInteger)))),
		Build:=BuildStorage),
	      NEW(Parse.Action, grammar:=
	        NEW(Parse.Sequence, items:=NIL),
		Build:=BuildSingle)))))),
	Build:=BuildGramExp));

(* gramExpBase ::=
	{ ide string "ide" "int" "real" "char" "string"
	  gramExpSequence gramExpChoice gramExpParens }
*)
    env.Add(gramExpBase.name,
      NEW(Parse.Choice, choice:=
	Parse.List(
	  NEW(Parse.Identifier, Build:=BuildGramIde),
	  NEW(Parse.QuotedString, Build:=BuildGramString),
	  NEW(Parse.GivenKeyword, key:=keyIde, Build:=BuildGramKeyIde),
	  NEW(Parse.GivenKeyword, key:=keyInt, Build:=BuildGramKeyInt),
	  NEW(Parse.GivenKeyword, key:=keyReal, Build:=BuildGramKeyReal),
	  NEW(Parse.GivenKeyword, key:=keyChar, Build:=BuildGramKeyChar),
	  NEW(Parse.GivenKeyword, key:=keyString, Build:=BuildGramKeyString),
	  gramExpSequence,
	  gramExpChoice,
	  gramExpParens)));

(* gramExpSequence ::=
	[ "[" gramExpList "]" ]
*)
    env.Add(gramExpSequence.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
	  NEW(Parse.GivenDelimiter, delim:='['),
	  Parse.Store(1, gramExpList),
	  NEW(Parse.GivenDelimiter, delim:=']'))),
	Build:=BuildGramExpSequence));

(* gramExpChoice ::=
	[ "{" gramExpList "}" ]
*)
    env.Add(gramExpChoice.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=
	  Parse.List(
	    NEW(Parse.GivenDelimiter, delim:='{'),
	    Parse.Store(1, gramExpList),
	    NEW(Parse.GivenDelimiter, delim:='}'))),
	Build:=BuildGramExpChoice));

(* gramExpParens ::=
	[ "(" gramExp
	  { [ "*" { [ "_" int gramExp ] gramExp } ] [] } 
	  ")" ]
*)
    env.Add(gramExpParens.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
	  NEW(Parse.GivenDelimiter, delim:='('),
	  Parse.Store(1, gramExp),
	  Parse.Store(6,
      	    NEW(Parse.Choice, choice :=Parse.List(
              NEW(Parse.Action, grammar:=
	        NEW(Parse.Sequence, items:=Parse.List(
	          NEW(Parse.GivenKeyword, key:=keyStar),
		  Parse.Store(5,
		    NEW(Parse.Choice, choice:=Parse.List(
		      NEW(Parse.Action, grammar:=
		        NEW(Parse.Sequence, items:=Parse.List(
			  NEW(Parse.GivenDelimiter, delim:='_'),
			  Parse.Store(4, 
			    NEW(Parse.Integer, Build:=BuildInteger)),
			  Parse.Store(3, gramExp))),
		        Build:= BuildGramExpIterPos),
		      NEW(Parse.Action, grammar:=
		        Parse.Store(3, gramExp),
		        Build:= BuildGramExpIterNoPos)))))),
		Build:=BuildGramExpIter),
	      NEW(Parse.Action, 
		grammar:=NEW(Parse.Sequence, items:=NIL),
		Build:=BuildGramExpBase)))),
  	  NEW(Parse.GivenDelimiter, delim:=')'))),
	Build:=BuildGramExpParens));

(* gramExpList ::=
	{ [ gramExp gramExpList ] [] }
*)
    env.Add(gramExpList.name,
      NEW(Parse.Choice, choice :=
	Parse.List(
	  NEW(Parse.Action, grammar:=
	    NEW(Parse.Sequence, items:=
	      Parse.List(
	        Parse.Store(1, gramExp),
		Parse.Store(2, gramExpList))),
	    Build:=BuildGramList),
	  NEW(Parse.Sequence, items:=NIL))));

  END InitGrammars;

BEGIN
END Gram.

