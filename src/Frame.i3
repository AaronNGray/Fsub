(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
INTERFACE Frame;
IMPORT Parse, Gram;

TYPE

  Env <: ROOT;

  Name =
    Parse.Tree BRANDED OBJECT name: TEXT END;
  NameList =
    Parse.Tree BRANDED OBJECT first: TEXT; rest: NameList END;

  Command =
    Parse.Tree BRANDED OBJECT
      name, arg: TEXT;
    END;
  Reload =
    Parse.Tree BRANDED OBJECT
      name: TEXT;
    END;
  Load =
    Parse.Tree BRANDED OBJECT
      name: TEXT;
    END;
  Module =
    Parse.Tree BRANDED OBJECT
      name: TEXT; imports: NameList;
    END;
  Establish =
    Parse.Tree BRANDED OBJECT
      name: TEXT;
    END;
  Save =
    Parse.Tree BRANDED OBJECT
      name: TEXT;
    END;
  Restore =
    Parse.Tree BRANDED OBJECT
      name: TEXT;
    END;
  None =
    Parse.Tree BRANDED OBJECT END;

VAR topFrame: Env:=NIL;
    topPhraseGram: Parse.Grammar;
    topPhraseEnv: Parse.GrammarEnv;

VAR topGram: Parse.Grammar;
    topEnv: Parse.GrammarEnv;
    

PROCEDURE Setup() RAISES ANY;

PROCEDURE LoadFile(fileName: TEXT; complain: BOOLEAN:=TRUE) RAISES ANY;

PROCEDURE ModuleFrame(name: TEXT; imports: NameList) RAISES ANY;

PROCEDURE ImportLoad(name: TEXT) RAISES ANY;

PROCEDURE EstablishFrame(name: TEXT) RAISES ANY;

PROCEDURE SaveGramInfo(gramInfo: Gram.GramInfo) RAISES ANY;

PROCEDURE SaveFrame(name: TEXT) RAISES ANY;

PROCEDURE RestoreFrame(name: TEXT) RAISES ANY;

END Frame.

