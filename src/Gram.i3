(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE Gram;
IMPORT Scanner, Parse;

  TYPE
    GramInfo <: GramInfoBase;
    GramInfoBase = 
      Parse.Tree BRANDED OBJECT
	topGram: Parse.Grammar;
	env: Parse.GrammarEnv;
	adoptAsTopLevelGrammar: BOOLEAN;
      END;

  VAR 
    keySet: Scanner.KeywordSet;
    env: Parse.GrammarEnv;

  PROCEDURE Setup() RAISES ANY;
  (* To be called before any other use of this module. *)

  PROCEDURE InitGrammars(
    VAR (*out*) syntaxTerm, syntaxDecl: Parse.NonTerminal) RAISES ANY;
  (* Calls Act.GetGrammars, so they had better be initialized. *)
  (* When parsed, the Parse.Tree produced by syntaxDecl is a GramInfo. *)

  PROCEDURE UndoSyntaxDecl(info: GramInfo) RAISES ANY;
  (* Undo the effect of parsing a syntaxDecl; to be called in the
     order inverse to the nesting of syntaxDecls. *)

(*
	grammar ::=
		clauseList

	clauseList ::=
		{ clause [] }

	clause ::=
		[ ide "::=" { ["." "." "."] [] } gramExp clauseList ]

	gramExp ::=
		{ ide string "ide" "int" "real" "char" "string"
		  [ "[" gramExpList "]" action ]
		  [ "{" gramExpList "}" ]
		  [ "(" gramExp { [ gramExp "*" action ] [] } ")" ] }

	gramExpList ::=
		{ [ gramExp gramExpList ] [] }

	action ::=
		{ [ "=>" "{" actionTermExp "}" ] 
		  [ ":>" "{" actionTypeExp "}" ] 
		  [] }

*)

END Gram.
