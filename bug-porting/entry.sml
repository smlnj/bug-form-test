(* entry.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Status =
  struct

    datatype t = Open | Closed | Deleted

    fun fromString s = (case Util.toLower s
           of "any" => Open
            | "open" => Open
            | "closed" => Closed
            | "deleted" => Deleted
            | _ => raise Fail(concat["bogus status \"", String.toString s, "\""])
          (* end case *))

  end;

structure OperatingSystem =
  struct

    datatype t = None | All | Linux | MacOS | Windows | OtherUnix

    fun fromString s = let
          val s = Util.toLower s
          fun return (os, v) = { os = os, version = v }
          in
            if String.isPrefix "windows " s
              then return (Windows, String.extract(s, 8, NONE))
            else if String.isPrefix "mac system " s
              then raise Fail(concat["bogus os \"", String.toString s, "\""])
            else (case s
              of "none" => return (None, "")
               | "all" => return (All, "")
               | "macos x" => return (MacOS, "")
               | "linux" => return (Linux, "")
               | "generic unix" => return (OtherUnix, "")
               | _ => return (OtherUnix, s)
             (* end case *))
          end

  end;

structure Architecture =
  struct
  end

structure Component =
  struct

    datatype t
      = None | Installer | Compiler | Basis | CM | MLLex | MLYacc
      | MLBurg | SMLNJLib | CML | EXene | FFI | Other | MLULex | MLAntlr

    fun fromString sts = (case Util.toLower sts
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
            | _ => raise Fail(concat["bogus component \"", String.toString sts, "\""])
          (* end case *))

  end;

structure Resolution =
  struct

    datatype t = None | Accepted | Fixed | Won'tFix | Invalid | WaitingResponse | Works

    fun fromString r = (case Util.toLower r
           of "none" => None
            | "accepted as bug" => Accepted
            | "fixed" => Fixed
            | "won't fix" => Won'tFix
            | "invalid" => Invalid
            | "awaiting response" => WaitingResponse
            | "works for me" => Works
            | _ => raise Fail(concat["bogus resolution \"", String.toString r, "\""])
          (* end case *))

  end;

structure Severity = struct

    datatype t = None | Critical | Major | Minor | Cosmetic

    fun fromString s = (case Util.toLower s
           of "none" => None
            | "critical" => Critical
            | "major" => Major
            | "minor" => Minor
            | "cosmetic" => Cosmetic
            | _ => raise Fail(concat["bogus severity \"", String.toString s, "\""])
          (* end case *))

  end;

structure Comment =
  struct

    type t = {
        date : Date.date,       (* date of comment *)
        name : string,          (* name of commentor *)
        content : string list   (* text of comment *)
     }

    (* each comment is proceeded text of the form
     * " *** 2022-05-20 04:28 --- John Reppy --- "
     *)
(*
    fun fromString s = let
          in
          end
*)

  end;

structure Entry : sig

    type t

    val readFile : string -> t list

    val id : t -> int
    val summary : t -> string
    val status : t -> Status.t
    val assignedTo : t -> string
    val openDate : t -> Date.date
    val closeDate : t -> Date.date option
    val os : t -> { os : OperatingSystem.t, version : string }
    val arch : t -> Architecture.t
    val smlnjVersion : string
    val component : t -> Component.t
    val resolution : t -> Resolution.t
    val severity : t -> Severity.t
    val description : t -> string list
    val transcript : t -> string list
    val source : t -> string list
    val comments : t -> Comment.t list

  end = struct

    type row = string vector

    type t = {
        artifact_id : int,              (* 0: original bug number *)
(*
        status_id,                      (* 1: "Open" = 1, "Closed" = 2 *)
*)
        status_name : Status.t,         (* 2: "Open" or "Closed" *)
(*
        priority : int,
        submitter_id,
        submitter_name,                 (* 5: "Bug Submitter *)
        assigned_to_id,
*)
        assigned_to_name : string,      (* 7: "John Reppy", ... *)
        open_date : Date.date,          (* 8: YYY-MM-DD hh:mm" *)
        close_date : Date.date,         (* 9: YYY-MM-DD hh:mm" *)
        last_modified_date : Date.date, (* 10: YYY-MM-DD hh:mm" *)
        summary : string,               (* 11: title of bug *)
        details : string list,          (* 12: *)
(*
        _votes,
        _voters,
        _votage,
*)
        architecture : string,          (* 16: "None", "All", "DEC Alpha", "HPPA",
                                         *  "PPC", "x86", "MIPS", "SPARC", "Other"
                                         *)
        os : OperatingSystem.t,         (* 17: ... *)
        component : Component.t,        (* 18: "Installer", "Compiler", "Basis Library",
                                         * "CM", "ML-Lex", "ML_Yacc", "ML-Burg",
                                         * "SML/NJ Library", CML", "eXene",
                                         * "C Interface Library", "Other", ML-Ulex",
                                         * "ML-ANTLR"
                                         *)
        resolution : Resolution.t,      (* 19: "None', "Accepted As Bug", "Fixed",
                                         * "Won't Fix", "Invalid", "Awaiting Response",
                                         * "Works for Me"
                                         *)
        severity : Severity.t,          (* 20: "None", "Critical", "Major", "Minor",
                                         *  "Cosmetic", "Feature"
                                         *)
        osVersion : string,             (* 21: *)
        smlnjVersion : string,          (* 22: *)
(*
        keywords : string,              (* 23: *)
        url : string,                   (* 24: *)
*)
        transcript : string list,       (* 25: *)
        source : string list,           (* 26: *)
        comments : Comment.t list       (* 27: each comment is proceeded text of the form
                                         * " *** 2022-05-20 04:28 --- John Reppy --- "
                                         *)
      }

    fun process (row : string vector) = let
          in {
            bugNum = valOf (Int.fromString (Vector.sub (row, 0))),
            summary = Vector.sub(row, 11),
            osVersion = Vector.sub(row, 21),     (* 21: *)
            smlnjVersion = Vector.sub(row, 22)   (* 22: *)
          } end

    fun readFile file = if OS.FileSys.access (file, [OS.FileSys.A_READ])
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
          else ?? (* error *)

    fun id (x : t) = #bugNum x
(*
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
*)

  end;
