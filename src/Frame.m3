(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
MODULE Frame;
IMPORT Parse, Err, Out, String, Scanner, Scope, Check, Value, Gram, 
Rd, TextRd, FileStream, Formatter, Text;

REVEAL
  Env = BRANDED OBJECT
      name: TEXT;
      topPhraseGram: Parse.Grammar;
      topPhraseEnv: Parse.GrammarEnv;
      gramInfos: GramInfos;
      scopeEnv: Scope.Env;
      checkEnv: Check.Env;
      valueEnv: Value.Env;
      rest: Env;
    END;

TYPE
  GramInfos = BRANDED OBJECT
      first: Gram.GramInfo;
      rest: GramInfos;
    END;

PROCEDURE LoadFile(fileName: TEXT; complain: BOOLEAN:=TRUE) RAISES ANY =
  VAR rd: Rd.T;
  BEGIN
    TRY 
      rd:= FileStream.OpenRead(fileName);
      Formatter.PutText(Out.out, "Loading " & fileName);
      Formatter.NewLine(Out.out);
      Scanner.PushInput(fileName, rd);
    EXCEPT 
    | Rd.Failure => 
	IF complain THEN 
	  Err.Fault(Out.out, "Could not open file: " & fileName) 
	END;
    END;
  END LoadFile;

PROCEDURE ModuleFrame(name: TEXT; imports: NameList) RAISES ANY =
(* Push scanner inputs so it will first load the imports first
   to last, then establish a frame for this module, and then
   finish reading this module. *)
  VAR scan: Env;
  BEGIN
    Scanner.PushInput("<none>", TextRd.New("establish " & name & ";\n"));
    LoadImports(imports);
  END ModuleFrame;

PROCEDURE LoadImports(imports: NameList) RAISES ANY =
(* last to first, so the scanner will see them first to last *)
  BEGIN
    IF imports#NIL THEN
      LoadImports(imports.rest);
      ImportLoad(imports.first);
    END;
  END LoadImports;

PROCEDURE ImportLoad(name: TEXT) RAISES ANY =
  VAR scan: Env;
  BEGIN
    scan:=FindFrame(name);
    IF scan=NIL THEN LoadFile(name & ".fsub") END;
  END ImportLoad;

PROCEDURE EstablishFrame(name: TEXT) RAISES ANY =
  VAR scan: Env;
  BEGIN
    Formatter.PutText(Out.out, "Establishing " & name);
    Formatter.NewLine(Out.out);
    scan:=FindFrame(name);
    IF scan=NIL THEN 
      SaveFrame(name);
    ELSE 
      RestoreFrame(name);
    END;
  END EstablishFrame;

PROCEDURE SaveGramInfo(gramInfo: Gram.GramInfo) RAISES ANY =
  BEGIN
    topFrame.gramInfos:=
      NEW(GramInfos, first:=gramInfo, 
	rest:=topFrame.gramInfos);
  END SaveGramInfo;

PROCEDURE SaveFrame(name: TEXT) RAISES ANY =
  BEGIN
    topFrame :=
	NEW(Env, name:=name, 
		topPhraseGram:=topPhraseGram, topPhraseEnv:=topPhraseEnv,
		gramInfos:=NIL,
		scopeEnv:=Scope.topEnv,
		checkEnv:=Check.topEnv, valueEnv:=Value.topEnv,
		rest:=topFrame);  
  END SaveFrame;

PROCEDURE RestoreFrame(name: TEXT) RAISES ANY =
  VAR scan: Env; gramInfos: GramInfos;
  BEGIN
    scan:=FindFrame(name);
    IF scan=NIL THEN 
      Err.Msg(Out.out, "Frame not found: " & name);
    ELSE
      LOOP
        gramInfos := topFrame.gramInfos;
        WHILE gramInfos#NIL DO 
	  Gram.UndoSyntaxDecl(gramInfos.first);
	  gramInfos := gramInfos.rest;
        END;
        topPhraseGram := topFrame.topPhraseGram;
	topPhraseEnv := topFrame.topPhraseEnv;
        Scope.topEnv := topFrame.scopeEnv;
        Check.topEnv := topFrame.checkEnv;
        Value.topEnv:= topFrame.valueEnv;
	IF topFrame=scan THEN EXIT END;
	topFrame:=topFrame.rest;
      END;
    END;
  END RestoreFrame;

PROCEDURE FindFrame(name: TEXT): Env RAISES ANY =
  VAR scan: Env;
  BEGIN
    scan:=topFrame;
    LOOP
      IF scan=NIL THEN EXIT END;
      IF Text.Equal(scan.name, name) THEN EXIT END;
      scan := scan.rest;
    END;
    RETURN scan;
  END FindFrame;

  PROCEDURE BuildName(self: Parse.Name; text: TEXT;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
  BEGIN
    RETURN 
      NEW(Name, location:=Err.NewLineLocation(info),
        name:=text);
  END BuildName;

  PROCEDURE BuildCommand(self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
  VAR name, arg: TEXT;
  BEGIN
    IF Parse.Stack[base+1]=NIL THEN name:="?"; arg:="?";
    ELSE
      name := NARROW(Parse.Stack[base+1], Name).name;
      IF Parse.Stack[base+2]=NIL THEN arg:="?";
      ELSE
	arg:=NARROW(Parse.Stack[base+2], Name).name;
      END;
    END;
    RETURN 
      NEW(Command, location:=Err.NewLineLocation(info),
        name:=name, arg:=arg);
  END BuildCommand;

  PROCEDURE BuildReloadName(self: Parse.Name; text: TEXT;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
  BEGIN
    RETURN 
      NEW(Reload, location:=Err.NewLineLocation(info),
        name:=text & ".fsub");
  END BuildReloadName;

  PROCEDURE BuildReloadString(self: Parse.QuotedString; string: String.T;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
  BEGIN
    RETURN 
      NEW(Reload, location:=Err.NewLineLocation(info),
        name:=String.ToText(string));
  END BuildReloadString;

  PROCEDURE BuildLoad(self: Parse.Name; text: TEXT;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
  BEGIN
    RETURN 
      NEW(Load, location:=Err.NewLineLocation(info),
        name:=text);
  END BuildLoad;

  PROCEDURE BuildModuleFrame(self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
  BEGIN
    RETURN 
      NEW(Module, location:=Err.NewLineLocation(info),
        name:=NARROW(Parse.Stack[base+1], Name).name,
	imports:=Parse.Stack[base+2]);
  END BuildModuleFrame;

  PROCEDURE BuildImportList(self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
  BEGIN
    RETURN 
      NEW(NameList, location:=Err.NewLineLocation(info),
        first:=NARROW(Parse.Stack[base+1], Name).name,
	rest:=Parse.Stack[base+2]);
  END BuildImportList;

  PROCEDURE BuildEstablishFrame(self: Parse.Name; text: TEXT;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
  BEGIN
    RETURN 
      NEW(Establish, location:=Err.NewLineLocation(info),
        name:=text);
  END BuildEstablishFrame;

  PROCEDURE BuildSaveFrame(self: Parse.Name; text: TEXT;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
  BEGIN
    RETURN 
      NEW(Save, location:=Err.NewLineLocation(info),
        name:=text);
  END BuildSaveFrame;

  PROCEDURE BuildRestoreFrame(self: Parse.Name; text: TEXT;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
  BEGIN
    RETURN 
      NEW(Restore, location:=Err.NewLineLocation(info),
        name:=text);
  END BuildRestoreFrame;

  PROCEDURE BuildRestoreFirstFrame(self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
  BEGIN
    RETURN 
      NEW(Restore, location:=Err.NewLineLocation(info),
        name:="");
  END BuildRestoreFirstFrame;

  PROCEDURE BuildNoFrame(self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
  BEGIN
    RETURN 
      NEW(None, location:=Err.NewLineLocation(info));
  END BuildNoFrame;

  PROCEDURE Select1(self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
  BEGIN
    RETURN Parse.Stack[base+1];
  END Select1;

  PROCEDURE Select3(self: Parse.Action; base: INTEGER;
      READONLY info: Err.LocationInfo): Parse.Tree RAISES ANY =
  BEGIN
    RETURN Parse.Stack[base+3];
  END Select3;

VAR topFrames, topFrameCommand, topFrameReload, topFrameLoad, topFrameModule, 
    importList, topFrameEstablish, topFrameSave, 
    topFrameRestore, topFrameNone: Parse.NonTerminal;

PROCEDURE Setup() RAISES ANY =
  BEGIN

    topFrames := NEW(Parse.NonTerminal, name:="*topFrames");
    topFrameCommand := NEW(Parse.NonTerminal, name:="*topFrameCommand");
    topFrameReload := NEW(Parse.NonTerminal, name:="*topFrameReload");
    topFrameLoad := NEW(Parse.NonTerminal, name:="*topFrameLoad");
    topFrameModule := NEW(Parse.NonTerminal, name:="*topFrameModule");
    importList := NEW(Parse.NonTerminal, name:="*importList");
    topFrameEstablish := NEW(Parse.NonTerminal, name:="*topFrameEstablish");
    topFrameSave := NEW(Parse.NonTerminal, name:="*topFrameSave");
    topFrameRestore := NEW(Parse.NonTerminal, name:="*topFrameRestore");
    topFrameNone := NEW(Parse.NonTerminal, name:="*topFrameNone");

    topGram := topFrames;
    topEnv := Parse.NewEnv();

    topEnv.Add(topFrames.name,
      NEW(Parse.Choice, choice:=Parse.List(
	topFrameCommand,
	topFrameReload,
	topFrameLoad,
	topFrameModule,
	topFrameEstablish,
	topFrameSave,
	topFrameRestore,
	topFrameNone)));
    topEnv.Add(topFrameCommand.name,
      NEW(Parse.Action, grammar:=
	NEW(Parse.Sequence, items:=Parse.List(
          NEW(Parse.GivenName, text:="do"),
          Parse.Store(1, 
	    NEW(Parse.Choice, choice:=Parse.List(
	      NEW(Parse.Name, Build:=BuildName),
	      NEW(Parse.Sequence, items:=NIL)))),
          Parse.Store(2, 
	    NEW(Parse.Choice, choice:=Parse.List(
	      NEW(Parse.Name, Build:=BuildName),
	      NEW(Parse.Sequence, items:=NIL)))),
	  NEW(Parse.GivenDelimiter, delim:=';'))),
	Build:=BuildCommand));
    topEnv.Add(topFrameReload.name,
      NEW(Parse.Action, grammar:=
	NEW(Parse.Sequence, items:=Parse.List(
          NEW(Parse.GivenName, text:="reload"),
	  Parse.Store(1, 
	    NEW(Parse.Choice, choice:=Parse.List(
	      NEW(Parse.Name, Build:=BuildReloadName),
	      NEW(Parse.QuotedString, Build:=BuildReloadString)))),
	  NEW(Parse.GivenDelimiter, delim:=';'))),
	Build:=Select1));
    topEnv.Add(topFrameLoad.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
	  NEW(Parse.GivenName, text:="load"),
	  Parse.Store(1, NEW(Parse.Name, Build:=BuildLoad)),
	  NEW(Parse.GivenDelimiter, delim:=';'))),
	Build:=Select1));
    topEnv.Add(topFrameModule.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
	  NEW(Parse.GivenName, text:="module"),
	  Parse.Store(1, NEW(Parse.Name, Build:=BuildName)),
	  Parse.Store(2,
	    NEW(Parse.Choice, choice:=Parse.List(
	      NEW(Parse.Action, grammar:=
	        NEW(Parse.Sequence, items:=Parse.List(
		  NEW(Parse.GivenName, text:="import"),
		  Parse.Store(3, importList))),
		Build:=Select3),
	      NEW(Parse.Sequence, items:=NIL)))),
	  NEW(Parse.GivenDelimiter, delim:=';'))),
	Build:=BuildModuleFrame));
    topEnv.Add(importList.name,
      NEW(Parse.Choice, choice:=Parse.List(
	NEW(Parse.Action, grammar:=
	  NEW(Parse.Sequence, items:=Parse.List(
	    Parse.Store(1, NEW(Parse.Name, Build:=BuildName)),
	    Parse.Store(2, importList))),
	  Build:=BuildImportList),
	NEW(Parse.Sequence, items:=NIL))));
    topEnv.Add(topFrameEstablish.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
	  NEW(Parse.GivenName, text:="establish"),
	  Parse.Store(1, NEW(Parse.Name, Build:=BuildEstablishFrame)),
	  NEW(Parse.GivenDelimiter, delim:=';'))),
	Build:=Select1));
    topEnv.Add(topFrameSave.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
	  NEW(Parse.GivenName, text:="save"),
	  Parse.Store(1, NEW(Parse.Name, Build:=BuildSaveFrame)),
	  NEW(Parse.GivenDelimiter, delim:=';'))),
	Build:=Select1));
    topEnv.Add(topFrameRestore.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=Parse.List(
	  NEW(Parse.GivenName, text:="restore"),
	  Parse.Store(1, 
	    NEW(Parse.Choice, choice:=Parse.List(
	      NEW(Parse.Name, Build:=BuildRestoreFrame),
	      NEW(Parse.Action, grammar:=NEW(Parse.Sequence, items:=NIL),
		Build:=BuildRestoreFirstFrame)))),
	  NEW(Parse.GivenDelimiter, delim:=';'))),
	Build:=Select1));
    topEnv.Add(topFrameNone.name,
      NEW(Parse.Action, grammar:=
        NEW(Parse.Sequence, items:=NIL),
        Build:=BuildNoFrame));
  END Setup;

BEGIN
END Frame.
