(* make-issues.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This program converts the CSV bug data from the gforge server to
 * the GitHub issue format.
 *
 * usage:
 *      make-issues [ options ] all
 *      make-issues [ options ] <bug-no> ...
 * options:
 *      -dir <dir>              -- specify output directory\n";
 *      -bugs <csv-file>        -- specify bug-report database\n";
 *      -features <csv-file>    -- specify feature-request database\n";
 *      -cmd                    -- print command to upload issue\n";
 *
 * to build:
 *      ml-build sources.cm MakeIssues.main make-issues
 *)

structure MakeIssues : sig

    val main : string * string list -> OS.Process.status

  end = struct

    structure E = Entry

    fun prErr msg = TextIO.output (TextIO.stdErr, msg)

    fun usage sts = (
          prErr "usage: make-issues [ options ] all\n";
          prErr "       make-issues [ options ] <bug-no> ...\n";
          prErr "options:\n";
          prErr "       -dir <dir>              -- specify output directory\n";
          prErr "       -bugs <csv-file>        -- specify bug-report database\n";
          prErr "       -features <csv-file>    -- specify feature-request database\n";
          prErr "       -cmd                    -- print command to upload issue\n";
          OS.Process.exit sts)

    (* print the `upload` command (see upload-cmd.sh) needed to create the issue on
     * GitHub (including setting its state, labels, and assignees).
     *)
    fun uploadCmd (ent, outFile) = let
          val state = (case E.status ent
                 of Status.Open => "open"
                  | Status.Closed => "closed"
                  | _ => raise Fail "unexpected deleted report"
                (* end case *))
          val labels = " -l " ^ Labels.get ent
          val assignee = (case NameMap.find (E.assignedTo ent)
                 of SOME id => " -a " ^ id
                  | NONE => ""
                (* end case *))
          in
            concat ["upload ", state, " ", outFile, labels, assignee, "\n"]
          end

    fun processArgs args = let
          fun doArgs ([], _, _, _, _) = usage OS.Process.failure
            | doArgs ("-bugs"::file::rest, outDir, _, featureFile, prCmd) =
                doArgs (rest, outDir, file, featureFile, prCmd)
            | doArgs ("-cmd"::rest, outDir, bugFile, featureFile, _) =
                doArgs (rest, outDir, bugFile, featureFile, true)
            | doArgs ("-dir"::dir::rest, _, bugFile, featureFile, prCmd) =
                doArgs (rest, dir, bugFile, featureFile, prCmd)
            | doArgs ("-features"::file::rest, outDir, bugFile, _, prCmd) =
                doArgs (rest, outDir, bugFile, file, prCmd)
            | doArgs (["all"], outDir, bugFile, featureFile, prCmd) =
                ({bugs=bugFile, features=featureFile}, outDir, prCmd, [])
            | doArgs (bugs, outDir, bugFile, featureFile, prCmd) =
                if List.all (isSome o Int.fromString) bugs
                  then ({bugs=bugFile, features=featureFile}, outDir, prCmd, bugs)
                  else usage OS.Process.failure
          in
            doArgs (args, ".", "bug-db.csv", "feature-db.csv", false)
          end

    fun main (cmd, args) = let
          val (dbFiles, outDir, prCmd, bugs) = processArgs args
          val db = DB.load dbFiles
          val bugIds = if null bugs
                then let (* all bugs *)
                  fun toId ent = if E.status ent <> Status.Deleted
                        then SOME(E.id ent)
                        else NONE
                  in
                    DB.mapPartial toId db
                  end
                else List.map (valOf o Int.fromString) bugs
          fun gen id = (case DB.find (db, id)
                 of (SOME ent) => let
                      val prefix = if E.isBug ent then "bug-" else "feature-"
                      val issueName = concat[
                              prefix, StringCvt.padLeft #"0" 3 (Int.toString(E.id ent)), ".md"
                            ]
                      val path = OS.Path.concat(outDir, issueName)
                      in
                        MakeMarkdown.gen (path, ent);
                        if prCmd
                          then TextIO.output (TextIO.stdOut, uploadCmd (ent, path))
                          else prErr (concat[
                              issueName, ": ", E.summary ent,
                              if E.isOpen ent then " [open]\n" else " [closed]"
                            ])
                      end
                  | NONE => prErr (concat["** bug ", Int.toString id, " does not exist\n"])
                (* end case *))
          in
            List.app gen bugIds;
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
