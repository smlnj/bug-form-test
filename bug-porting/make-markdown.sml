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

  (* mapping from gforge names to GitHub names *)
    local
      structure SMap = ListMapFn (
        struct
          type ord_key = string
          val compare = String.compare
        end)
    in
    val nameMap = let
          val nMap = List.foldl SMap.insert' SMap.empty [
                  ("John Reppy", "@JohnReppy"),
                  ("David MacQueen", "@dmacqueen"),
                  ("Matthias Blume", "@mathias-blume"),
                  ("Matthew Fluet", "@MatthewFluet")
                ]
          in
            fn name => (case SMap.find (nMap, name) of SOME n => n | NONE => name)
          end
    end

  (* the output has the following format:
   *
   *    ================
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
   *    ================
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
          fun prComment {date, name, content} = let
                val hdr = concat [
                        "comment by ", nameMap name, " on ",
                        Date.fmt "%Y-%M-%d %H:%M%S +000 UTC" date
                      ]
                in
                  nl();
                  prTextBlock (hdr, content)
                end
          in
            prl [E.summary entry, "\n\n"];
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
            prResponse ("Email address", #email(E.submitter entry));
            if not (null (E.comments entry))
              then (
                pr "## Comments\n";
                List.app prComment (E.comments entry))
              else ();
            TextIO.closeOut outS
          end

  end;
