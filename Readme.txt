(* Copyright 1991 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
File: README

	   _______
	  |       |          __
	  |   ____|___ __ __|  |__
	  |     |   __|  |  |     |
	  |   __|__   |     |  |  |
	  |___| |_____|_____|_____|


Fsub is a Modula-3 [3] implementation of the F<: calculus [1],[2],[4].
This is the "smallest possible" calculus integrating subtyping with 
polymorphism. The type structure consists of type variables, "Top", 
function spaces, bounded quantification, and recursive types. 
The implementation supports type inference ("argument synthesis"), a 
simple modularization mechanism, and the introduction of arbitrary 
notation on-the-fly.

The system can be obtained by anonymous ftp from gatekeeper.pa.dec.com,
in the "DEC" directory. (To unpack, say: "uncompress <dir>.tar.Z; 
tar xvf <dir>.tar".) The distribution includes DECstation (mips/Fsub)
binaries; it can be ported to other architectures that support Modula-3 
by recompilation (say "m3make" in e.g. in the mips directory).

The Fsub licence is covered by the Modula-3 licence (included);
there is nothing to sign. If needed, Modula-3 can be obtained by 
anonymous ftp from gatekeeper.pa.dec.com.

A manual "F-sub, the system" is included in postscript format. It will 
prints on Apple LaserWriter II and HP LaserJet postscript printers. 
Hardcopies can be obtained from:

	Luca Cardelli (luca@src.dec.com)
	DEC SRC
	130 Lytton Ave
	Palo Alto CA 94301
	USA

To install Fsub, move the appropriate binary (e.g mips/Fsub)
to some convenient location, and invoke it from your working directory.
There are no shell-line parameters. A local file called
"Startup.fsub", if found, is loaded at startup.

------
[1] L.Cardelli, J.C.Mitchell, S.Martini, A.Scedrov: "An extension
    of system F with subtyping", Proc. TACS'91 & SRC Report #80.
[2] P.-L.Curien, G.Ghelli: "Subyping + extensionaliy: confluence
    of beta-eta reductions in F<=", Proc. TACS'91.
[3] G.Nelson(ed.): Systems Programming in Modula-3. 
    Prentice Hall, ISBN 0-13-590464-1, 1991.
[4] B.C.Pierce: Bounded quantification is undecidable, Proc. POPL'92.
 