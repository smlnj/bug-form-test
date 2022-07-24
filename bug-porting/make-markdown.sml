(* make-markdown.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(* generate a markdown file for an bug report *)
structure MakeMarkdown : sig

    (* generate a GitHub issue in Markdown format.   The first argument is the
     * name of the output file and the second is the database entry for the
     * bug.
     *)
    val gen : string * Entry.t -> unit

  end = struct

    structure E = Entry

  (* the output for a "bug" has the following format:
   *
   * ================
   *    <title line>
   *
   *    ### Version
   *
   *    <version>
   *
   *    ### Operating System
   *
   *    - [ ] Any
   *    - [ ] Linux
   *    - [ ] macOS
   *    - [ ] Windows
   *    - [ ] Other Unix
   *
   *    ### OS Version
   *
   *    <os-version>
   *
   *    ### Processor
   *
   *    ### System Component
   *
   *    ### Severity
   *
   *    ### Description
   *
   *    <text>
   *
   *    ### Transcript
   *
   *    ```
   *    <transcript>
   *    ```
   *
   *    ### Expected Behavior
   *
   *    <text>
   *
   *    ### Steps to Reproduce
   *
   *    <text>
   *
   *    ### Additional Information
   *
   *    <text>
   *
   *    ### Email address
   *
   *    <email address of submitter>
   *
   *    ## Comments
   *
   *    <comments>
   *
   * ================
   *
   * The format for a feature request is
   *
   *    <title line>
   *
   *    ### Description
   *
   *    <text>
   *
   *    ## Comments
   *
   *    <comments>
   *
   *)

    val noResponse = "_No response_\n\n"

    fun gen (out, entry) = let
          val outS = TextIO.openOut out
          fun pr s = TextIO.output (outS, s)
          fun nl () = pr "\n"
          fun prl l = pr(concat l)
          fun prHdr s = prl ["### ", s, "\n\n"]
          (* output a check list *)
          fun prCheckList toS (hdr, checked, items) = let
                fun prItem item = if checked item
                      then prl ["- [X] ", toS item, "\n"]
                      else prl ["- [ ] ", toS item, "\n"]
                in
                  prHdr hdr;
                  List.app prItem items
                end
          (* output a code block *)
          fun prCodeBlock (title, lang, content) = (
                prHdr title;
                if null content
                  then pr noResponse
                  else (
                    if lang <> "" then prl ["``` ", lang, "\n"] else pr "```\n";
                    List.app (fn ln => prl [ln, "\n"]) content;
                    pr "```\n\n"))
          (* print a possibly empty response *)
          fun prResponse (hdr, response) = (
                prHdr hdr;
                if (response = "")
                  then pr noResponse
                  else prl [response, "\n\n"])
          fun prTextBlock (title, content) = (
                prHdr title;
                if null content
                  then pr noResponse
                  else (
                    List.app (fn ln => prl [ln, "\n"]) content;
                    nl()))
          (* print a dropdown response *)
          fun prChoice toS (hdr, item) = (
                prHdr hdr;
                prl [toS item, "\n\n"])
          (* operating-system info *)
          val {os, version=osVersion} = E.os entry
          fun isOS os' = (os = os')
          (* print a comment *)
          fun prComment {date, name, content} = (
                nl();
                prl [
                    "#### comment by ", NameMap.map name, " on ",
                    Date.fmt "%Y-%m-%d %H:%M:%S +000 UTC" date, "\n\n"
                  ];
                List.app (fn ln => prl [ln, "\n"]) content)
          in
            prl [E.summary entry, "\n\n"];
            if (E.isBug entry)
              then (
                prResponse ("Version", E.smlnjVersion entry);
                prCheckList
                  OperatingSystem.toString
                    ("Operating System", isOS, OperatingSystem.values);
                nl();
                prResponse ("OS Version", osVersion);
                prChoice Architecture.toString ("Processor", E.arch entry);
                prChoice Component.toString ("Component", E.component entry);
                prChoice Severity.toString ("Severity", E.severity entry);
                prTextBlock ("Description of the problem", E.description entry);
                prCodeBlock ("Transcript", "", E.transcript entry);
                prTextBlock ("Expected Behavior", []);
                prTextBlock ("Steps to Reproduce", E.source entry);
                prTextBlock ("Additional Information", []);
                prResponse ("Email address", #email(E.submitter entry)))
              else (
                prTextBlock ("Description of the problem", E.description entry));
            (* include a standard comment that records the connection to the old gforge bug *)
            pr "### Comments from smlnj-gforge\n\n";
            prl ["#### Original smlnj-gforge bug number ", Int.toString(E.id entry), "\n"];
            let
            val content = (case E.keywords entry
                   of "" => []
                    | kws => ["**Keywords:** " ^ E.keywords entry]
                  (* end case *))
            val date = Date.fmt " on %Y-%m-%d at %H:%M:%S" (E.openDate entry)
            val content = (case E.submitMsg entry
                   of NONE => "Submitted " ^ date :: content
                    | SOME msg => msg ^ date :: content
                  (* end case *))
            in
              List.app (fn ln => prl ["\n", ln, "\n"]) content
            end;
            List.app prComment (E.comments entry);
            TextIO.closeOut outS
          end

  end;
