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

structure RE = RegExpFn(
    structure P = AwkSyntax
    structure E = BackTrackEngine)

structure Util =
  struct

    val toLower = String.map Char.toLower

    val lines = String.fields (fn #"\n" => true | _ => false)

    fun reFind : RE.regexp -> substring -> (substring
  end

structure Status =
  struct

    datatype t = Open | Closed | Deleted

    fun fromString s = (case toLower s
           of "any" => Open
            | "open" => Open
            | "closed" => Closed
            | "deleted" => Deleted
            | _ raise Fail(concat["bogus status \"", String.toString s, "\""])
          (* end case *))

  end

structure OS =
  struct

    datatype t = None | All | Linux | MacOS | Windows | OtherUnix

    fun fromString s = let
          val s = toLower s
          fun return (os, v) = { os = os, version = v }
          in
            if String.isPrefix "windows " s
              then return (Windows, String.extract(s, 8, NONE))
            else if String.isPrefix "mac system " s
              then raise Fail(concat["bogus os \"", String.toString s, "\""])
            else (case toLower s
              of "none" => return (None, "")
               | "all" => return (All, "")
               | "macos x" => return (MacOS, "")
               | "linux" => return (Linux, "")
               | "generic unix" => return (OtherUnix, "")
               | _ => return (OtherUnix, s)
             (* end case *))

  end

structure Component =
  struct

    datatype t
      = None | Installer | Compiler | Basis | CM | MLLex | MLYacc
      | MLBurg | SMLNJLib | CML | EXene | FFI | Other | MLULex | MLAntlr

    fun fromString sts = (case toLower sts
           of "none" => None
            | "installer" => Installer
            | "compiler" => Compiler
            | "basis library" => Basis
            | "cm" => CM
            | "ml-lex" => MLLex
            | "ml-yacc" => MLYacc
            | "ml-burg" => MLBurg
            | "sml/nj library" => SMLNJLib
            | "cml" => CML
            | "exene" => EXene
            | "c interface library" => FFI
            | "other" => Other
            | "ml-ulex" => MLULex
            | "ml-antlr" => MLAntlr
            | _ raise Fail(concat["bogus componenta \"", String.toString s, "\""])
          (* end case *))

  end

structure Resolution =
  struct

    datatype t = None | Accepted | Fixed | Won'tFix | Invalid | WaitingResponse | Works

    fun fromString sts = (case toLower sts
           of "none" => None
            | "accepted as bug" => Accepted
            | "fixed" => Fixed
            | "won't fix" => Won'tFix
            | "invalid" => Invalid
            | "awaiting response" => WaitingResponse
            | "works for me" => Works
            | _ raise Fail(concat["bogus resolution \"", String.toString s, "\""])
          (* end case *))

  end

structure Severity = struct

    datatype t = None | Critical | Major | Minor | Cosmetic

    fun fromString s = (case toLower s
           of "none" => None
            | "critical" => Critical
            | "major" => Major
            | "minor" => Minor
            | "cosmetic" => Cosmetic
            | _ raise Fail(concat["bogus severity \"", String.toString s, "\""])
          (* end case *))

  end

structure Comment =
  struct

    type comment = {
        date : Date.date,       (* date of comment *)
        name : string,          (* name of commentor *)
        content : string list   (* text of comment *)
     }

    (* each comment is proceeded text of the form
     * " *** 2022-05-20 04:28 --- John Reppy --- "
     *)
    fun fromString s = let
          in
          end

  end

structure Entry : sig

    val readFile : string -> t list

    type t

    val id : t -> int
    val summary : t -> string
    val status : t -> Status.t
    val assignedTo : t -> string
    val openDate : t -> Date.date
    val closeDate : t -> Date.date option
    val os : r -> { os : OS.t, version : string }
    val arch : t -> Architecture.t
    val smlnjVersion : string,
    val component : t -> Component.t
    val resolution : t -> Resolution.t
    val severity : t -> Severity.t
    val description : t -> string list
    val transcript : t -> string list
    val source : t -> string list
    val comments : t -> Comment.t list

  end = struct

    type row = string vector

    fun readFile csvFile = let


    type t = {
        artifact_id,            (* 0: original bug number *)
(*
        status_id,              (* 1: "Open" = 1, "Closed" = 2 *)
*)
        status_name,            (* 2: "Open" or "Closed" *)
(*
        priority : int,
        submitter_id,
        submitter_name,         (* 5: "Bug Submitter *)
        assigned_to_id,
*)
        assigned_to_name,       (* 7: "John Reppy", ... *)
        open_date,              (* 8: YYY-MM-DD hh:mm" *)
        close_date,             (* 9: YYY-MM-DD hh:mm" *)
        last_modified_date,     (* 10: YYY-MM-DD hh:mm" *)
        summary : string,       (* 11: title of bug *)
        details,
(*
        _votes,
        _voters,
        _votage,
*)
        architecture : string,  (* 15: "None", "All", "DEC Alpha", "HPPA", "PPC", "x86",
                                 * "MIPS", "SPARC", "Other"
                                 *)
        os : string,            (* 16: ... *)
        component : string,     (* 17: "Installer", "Compiler", "Basis Library", "CM",
                                 * "ML-Lex", "ML_Yacc", "ML-Burg", "SML/NJ Library",
                                 * CML", "eXene", "C Interface Library", "Other",
                                 * ML-Ulex", "ML-ANTLR" *)
        resolution : string,    (* 18: "None', "Accepted As Bug", "Fixed", "Won't Fix",
                                 * "Invalid", "Awaiting Response", "Works for Me" *)
        severity : string,      (* 19: "None", "Critical", "Major", "Minor", "Cosmetic",
                                 * "Feature"
                                 *)
        osVersion : string,     (* 20: *)
        smlnjVersion : string,  (* 21: *)
(*
        keywords : string,      (* 22: *)
        url : string,           (* 23: *)
*)
        transcript : string list, (* 24: *)
        source : string list,   (* 25: *)
        comments                (* 26: each comment is proceeded text of the form
                                 * " *** 2022-05-20 04:28 --- John Reppy --- "
                                 *)
      }

    fun process (row : string vector) = let
          in {
            bugNum = valOf (Int.fromString (Vector.sub (row, 0))),
            summary = Vector.sub(row, 11),
            osVersion = Vector.sub(row, 20),     (* 20: *)
            smlnjVersion = Vector.sub(row, 21)   (* 21: *)
          } end

    fun loadFile file = if OS.FileSys.access (file, [OS.FileSys.A_READ])
          then let
            val inS = TextIO.openIn file
            fun cleanup () = TextIO.closeIn inS
            fun lp acc = (case CSVReadVector.getRow inS
                   of SOME row => lp (process row :: acc)
                    | NONE => List.rev acc
                  (* end case *))
            in
            (* consumer the header *)
              ignore (TextIO.inputLine inS);
              (lp []) handle ex => (cleanup(); raise ex)
              before cleanup()
            end
          else (* error *)

    fun id (x : t) = #bugNum x
    fun summary (x : t) =
    fun status (x : t) =
    fun assignedTo (x : t) =
    fun openDate (x : t) =
    fun closeDate (x : t) =
    fun os (x : t) =
    fun arch (x : t) =
    fun osVersion (x : t) =
    fun smlnjVersion (x : t) =
    fun component (x : t) =
    fun resolution (x : t) =
    fun severity (x : t) =
    fun description (x : t) =
    fun transcript (x : t) =
    fun source (x : t) =
    fun comments (x : t) =

  end

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
