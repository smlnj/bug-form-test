(* make-issues.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This program converts the CSV bug data from the gforge server to
 * the GitHub issue format.
 *
 * usage:
 *      make-issues [ -dir <dir> ] <csv-file> <bug-no> ...
 *)

(* generate a markdown file for an bug report *)
structure Gen : sig

    val gen : string * Entry.t -> unit

  end = struct

    structure E = Entry

  (* the output has the following format:
   *
   *    ================
   *    <title line>
   *
   *    ### Version
   *
   *    <version>
   *
   *    ### What operating system(s) exhibit the bug?
   *
   *    ================
   *)

    fun gen (out, entry) = let
          val outS = TextIO.openOut out
          fun pr s = TextIO.output (outS, s)
          fun nl () = pr "\n"
          fun prl l = pr(concat l)
          fun prHdr s = prl ["### ", s, "\n\n"]
          (* output a check list *)
          fun prCheckList toS (checked, items) = let
                fun prItem item = if checked item
                      then prl ["- [X] ", toS item, "\n"]
                      else prl ["- [ ] ", toS item, "\n"]
                in
                  List.app prItem items
                end
          (* output a code block *)
          fun prCodeBlock (title, lang, content) = (
                prHdr title;
                if lang <> "" then prl ["``` ", lang, "\n"] else pr "```\n";
                List.app (fn ln => prl [ln, "\n"]) content;
                pr "```\n\n")
          in
            prl [E.summary entry, "\n"];
            prHdr "Version";
            prHdr "What operating system(s) exhibit the bug?";
            prCheckList
            prHdr "What OS version?";
            prHdr "What processor exhibits the bug?";
            prHdr "Component";
            prHdr "Severity";
            prHdr "Description of the problem";
            prCodeBlock ("Transcript", "", (E.transcript entry));
            prHdr "Expected Behavior";
            prHdr "Steps to Reproduce";
            prHdr "Additional Information";
            prHdr "Email address";
            prHdr "Comments:";
            TextIO.closeOut outS
          end

  end;
