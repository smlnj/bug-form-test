(* make-issues.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This program converts the CSV bug data from the gforge server to
 * the GitHub issue format.
 *
 * usage:
 *      make-issues [ -dir <dir> ] [ -cmd ] <csv-file> <bug-no> ...
 *
 * to build:
 *      ml-build sources.cm MakeIssues.main make-issues
 *)

structure MakeIssues : sig

    val main : string * string list -> OS.Process.status

  end = struct

    structure E = Entry

    fun prErr msg = TextIO.output (TextIO.stdErr, msg)

    fun usage () = (
          prErr "** usage: make-issues [ -dir <dir> ] [ -cmd ] <csv-file> <bug-no> ...\n";
          OS.Process.exit OS.Process.failure)

    fun main (cmd, args) = let
          val (outDir, csvFile, bugs) = (case args
                 of "-dir"::dir::file::(bugs as _::_) => (dir, file, bugs)
                  | file::(bugs as _::_) =>  (".", file, bugs)
                  | _ => usage()
                (* end case *))
(* TODO: load features too! *)
          val db = DB.readFile (true, csvFile, DB.empty)
          fun find id = (case Int.fromString id
                 of SOME id => DB.find (db, id)
                  | NONE => NONE
                (* end case *))
          fun gen id = (case find id
                 of (SOME ent) => let
                      val prefix = if E.isBug ent then "bug-" else "feature-"
                      val issueName = concat[
                              prefix, StringCvt.padLeft #"0" 3 (Int.toString(E.id ent)), ".md"
                            ]
                      val path = OS.Path.concat(outDir, issueName)
                      in
                        MakeMarkdown.gen (path, ent);
(* TODO: include open/close and other labels in output *)
                        prErr (concat[issueName, ":    ", "\n"])
                      end
                  | NONE => prErr (concat["** bug ", id, " does not exist\n"])
                (* end case *))
          in
            List.app gen bugs;
            OS.Process.success
          end
            handle ex => (
              prErr (concat [
                  "uncaught exception ", General.exnName ex,
                  " [", General.exnMessage ex, "]\n"
                ]);
              List.app (fn s => prErr (concat ["  raised at ", s, "\n"])) (SMLofNJ.exnHistory ex);
              OS.Process.failure)

  end
