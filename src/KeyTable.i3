(* Copyright 1990 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Created by Jorge Stolfi   on Thu Sep  6  1:38:51 PDT 1990   *)
(* Based on Table.def by John Ellis                            *)
(* Last modified on Tue Feb  4 06:02:41 1992 by luca           *)
(*      modified on Tue May 14 13:22:44 PDT 1991 by stolfi     *)

INTERFACE KeyTable;

(********************************************************************)
(* WARNING: DO NOT EDIT THIS FILE, IT WAS GENERATED MECHANICALLY.   *)
(* See the  Makefile for more details.                              *)
(********************************************************************)

(* Dynamic tables with TEXT keys and REFANY values.

This module implements hash tables whose keys are TEXT's and whose
values are REFANY's.

The tables grow and shrink automatically so as to provide reasonable
access time without wasting too much space.

All operations on a given table are internally serialized by a private
MUTEX.

Index: hash table, table, dictionary, TEXT, REFANY. 

*)

IMPORT List;

TYPE
  Key = TEXT;

  Value = REFANY;

  CHARS = ARRAY OF CHAR;

EXCEPTION NotFound;

TYPE
  T = OBJECT METHODS 

      in (key: Key; VAR (*OUT*) value: Value): BOOLEAN RAISES {}; 
        (*
          Looks for an entry in the table with the given /key/.  
          If /key/ is found, its value is assigned to /value/ and 
          TRUE is returned.  If the key isn't found, /value/ is set to 
          /Refany.Default/ and FALSE returned. *)

      get (key: Key): Value RAISES {NotFound};
        (*
          Looks for an entry in the table with the given /key/, and
          returns the corresponding value.  Raises an exception if the
          key isn't found.  *)

      inChars (READONLY key: CHARS; VAR (*OUT*) value: Value): BOOLEAN RAISES {}; 
      getChars (READONLY key: CHARS): Value RAISES {NotFound}; 
        (*
          Same as /in/ and /get/, but take an ARRAY OF CHAR instead of a TEXT. *)

      put (key: Key; READONLY value: Value): BOOLEAN RAISES {};
        (*
          Enters a key/value pair into the table.  If there is already
          an entry with the given key, its value is replaced by the new one
          and TRUE is returned. Otherwise, the pair /key,value/
          is added to the table, and FALSE is returned. *)

      delete (key: Key; VAR (*OUT*) value: Value): BOOLEAN RAISES {};
        (*
          Removes a key/value pair from the table.  If the key was
          present, /value/ is set to the deleted value and TRUE is
          returned.  If the key wasn't present, the table isn't changed,
          /value/ is set to /Refany.Default/, and FALSE is returned.  *)

      clear () RAISES {};
        (*
          Removes all entries from the table. *)

      copy (): T RAISES {};
        (*
          Makes a copy of the table and its contents. *)

      toKeyList (): List.T RAISES {};
        (*
          Returns a list of all the keys in the table, as TEXT, in no
          particular order.  *)

      toValueList (): List.T RAISES {};
        (*
          Returns a list of all the values in the table, as REFANY, in no
          particular order.  *)

      toAssocList (): List.T RAISES {};
        (*
          Returns an association list (see List.Assoc) of entries in the
          table, that is, a list of pairs (key value). *)

      enumerate  (
          proc: EnumerateProc; 
          data: REFANY; 
          VAR (*OUT*) key: Key; 
          VAR (*OUT*) value: Value;
        ): BOOLEAN RAISES ANY;
        (*
          Invokes /proc(data, key, value)/ for each pair /(key,value)/ in the
          table.  If /proc/ returns TRUE, the enumeration is terminated, the
          terminating entry is stored into /key/ and /value/, and TRUE is
          returned.  If /proc/ never returns TRUE, /key/ and /value/ are set
          to /""/ and /Refany.Default/, and FALSE is returned.

          The client procedure /proc/ is called with /T.lock/ held.
          Therefore, all other operations on the same table will block
          until Enumerate returns (or is aborted by the /proc/ raising an 
	  exception).
          Also, the /proc/ itself sshould not attempt to perform any
          operation on the same table, or a deadlock will result.

          The client procedure /proc/ is allowed to modify the /value/ field
          of visited entries.

          *)

    END;

PROCEDURE New(

    initialSize: CARDINAL := 1;
  ): T RAISES {};
(*
  Constructs a new table, initially containing /initialSize/ buckets.

  *)

TYPE

  EnumerateProc = PROCEDURE(data: REFANY; key: Key; VAR value: Value)
    : BOOLEAN RAISES ANY;
  (*
    A client-defined procedure called by Enumerate on each key/value
    pair in the table.

    The /data/ parameter is the client data passed to
    Enumerate.  An EnumerateProc should return true to halt the
    enumeration, false to continue it.  *)

END KeyTable.