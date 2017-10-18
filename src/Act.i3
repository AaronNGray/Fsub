(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)

INTERFACE Act;
IMPORT Err, String, Parse;

  PROCEDURE Setup() RAISES ANY;
  (* To be called before any other use of this module. *)

  PROCEDURE GetGrammars(VAR (*out*) actionTermGrammar, 
    actionTypeGrammar: Parse.NonTerminal) RAISES ANY;

  PROCEDURE BuildActionIdentifier(self: Parse.Identifier; name: TEXT;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY;
  PROCEDURE BuildActionDelimiter(self: Parse.GivenDelimiter;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY;
  PROCEDURE BuildActionKeyword(self: Parse.GivenKeyword;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY;
  PROCEDURE BuildActionInteger(self: Parse.Integer; int: INTEGER;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY;
  PROCEDURE BuildActionReal(self: Parse.Real; real: REAL;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY;
  PROCEDURE BuildActionChar(self: Parse.QuotedChar; char: CHAR;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY;
  PROCEDURE BuildActionString(self: Parse.QuotedString; string: String.T;
	READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY;

  PROCEDURE DefaultTree(location: Err.Location): Parse.Tree RAISES ANY;

  PROCEDURE CheckPattern(action: Parse.Tree) RAISES ANY;

  PROCEDURE InstantiatePattern(action: Parse.Tree;
	base: INTEGER): Parse.Tree RAISES ANY;

END Act.
