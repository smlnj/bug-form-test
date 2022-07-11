(* entry.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The fields of the exported CSV file are:
 *
 *      artifact_id
 *      status_id
 *      status_name
 *      priority
 *      submitter_id
 *      submitter_name
 *      assigned_to_id
 *      assigned_to_name
 *      open_date
 *      close_date
 *      last_modified_date
 *      summary
 *      details
 *      _votes
 *      _voters
 *      _votage
 *      "Machine Architecture"
 *      "Operating System"
 *      "Component"
 *      "Resolution"
 *      "Severity"
 *      "OS Version"
 *      "SML/NJ Version"
 *      "Keywords"
 *      "URL"
 *      "Transcript (of reproduction)"
 *      "Source (for reproduction)"
 *      comments
 *
 * The last line of the `details` field may be have the form:
 *
 *      Submitted via web form by Jane Doe <jane.doe@email-server.com>
 *
 * in which case, the submitter_name field should be overwritten with "Jane Doe"
 *)

structure RE = RegExpFn(
    structure P = AwkSyntax
    structure E = BackTrackEngine)

structure CSVReadVector = CSVReadFn (
  struct

    type row = string vector

    val fromList = Vector.fromList
    val toList = Vector.toList

  end)

structure Util =
  struct

    val toLower = String.map Char.toLower

    val lines = String.fields (fn #"\n" => true | _ => false)

  end

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

    val values = [All, Linux, MacOS, Windows, OtherUnix]

    fun fromString (os, v) = let
          val os = Util.toLower os
          fun return (os, v) = { os = os, version = v }
          in
            if String.isPrefix "windows " os
              then return (Windows, String.extract(os, 8, NONE))
            else if String.isPrefix "mac system " os
              then raise Fail(concat["bogus os \"", String.toString os, "\""])
            else (case os
              of "none" => if v = "" then return (All, "") else return (OtherUnix, v)
               | "all" => return (All, "")
               | "macos x" => return (MacOS, v)
               | "linux" => return (Linux, v)
               | "generic unix" => return (OtherUnix, v)
               | _ => if v = ""
                  then return (OtherUnix, os)
                  else return (OtherUnix, concat[os, " ", v])
             (* end case *))
          end

    fun toString None = "_No response_"
      | toString All = "All"
      | toString Linux = "Linux"
      | toString MacOS = "macOS"
      | toString Windows = "Windows"
      | toString OtherUnix = "Other Unix"

  end;

structure Architecture =
  struct

    datatype t = None | All | Arm | PowerPC | Sparc | X86 | AMD64 | Other

    val values = [All, Arm, PowerPC, Sparc, X86, AMD64, Other]

    fun fromString a = (case Util.toLower a
           of "none" => None
            | "all" => All
            | "dec alpha" => Other
            | "hppa" => Other
            | "ppc" => PowerPC
            | "x86" => X86
            | "mips" => Other
            | "sparc" => Sparc
            | "other" => Other
            | _ => raise Fail(concat["bogus architecture \"", String.toString a, "\""])
          (* end case *))

    fun toString None = "_No response_"
      | toString All = "Any"
      | toString Arm = "Arm (using Rosetta)"
      | toString PowerPC = "PowerPC"
      | toString Sparc = "Sparc"
      | toString X86 = "x86 (32-bit)"
      | toString AMD64 = "x86-64 (64-bit)"
      | toString Other = "Other"

  end

structure Component =
  struct

    datatype t
      = None | Installer | Compiler | Basis | CM | MLLex | MLYacc
      | MLBurg | SMLNJLib | CML | EXene | FFI | Other | MLULex | MLAntlr

    fun fromString c = (case Util.toLower c
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
            | _ => raise Fail(concat["bogus component \"", String.toString c, "\""])
          (* end case *))

    fun toString None = "_No response_"
      | toString Installer = "Installation"
      | toString Compiler = "Core system"
      | toString Basis = "Basis Library"
      | toString CM = "Compilation manager (CM)"
      | toString MLLex = "Command-line tool"
      | toString MLYacc = "Command-line tool"
      | toString MLBurg = "Command-line tool"
      | toString SMLNJLib = "SML/NJ Library"
      | toString CML = "Concurrent ML (CML)"
      | toString EXene = "Other"
      | toString FFI = "Foreign-Function Interface (FFI)"
      | toString Other = "Other"
      | toString MLULex = "Command-line tool"
      | toString MLAntlr = "Command-line tool"

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

    fun toString None = "_No response_"
      | toString Critical = "Critical"
      | toString Major = "Major"
      | toString Minor = "Minor"
      | toString Cosmetic = "Cosmetic"

  end;

structure Timestamp =
  struct

    type t = Date.date

  (* timestamps are given using the syntax "YYYY-MM-DD hh:mm"; we assume that these
   * are in Chicago (local) time, so we convert them to dates in UTC time.
   *)
    local
      structure MT = MatchTree
      val prefix = RE.prefix
            (RE.compileString "([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2})")
              Substring.getc
      val months = #[
              Date.Jan, Date.Feb, Date.Mar, Date.Apr, Date.May, Date.Jun,
              Date.Jul, Date.Aug, Date.Sep, Date.Oct, Date.Nov, Date.Dec
            ]
    in
    fun fromString s = let
          val SOME(MT.Match(_, [year, month, day, hour, minute]), _) = prefix (Substring.full s)
          fun toInt (MT.Match({pos, len}, _)) =
                #1(valOf(Int.scan StringCvt.DEC Substring.getc pos))
          val d = Date.date{
                  year = toInt year,
                  month = Vector.sub(months, (toInt month)-1),
                  day = toInt day,
                  hour = toInt hour,
                  minute = toInt minute,
                  second = 0,
                  offset = NONE
                }
          in
            Date.fromTimeUniv(Date.toTime d)
          end
    end

  end

structure Comment =
  struct

    type t = {
        date : Date.date,       (* date of comment *)
        name : string,          (* name of commentor *)
        content : string list   (* text of comment *)
     }

    (* each comment is proceeded text of the form
     * " *** 2022-05-20 04:28 --- Jane Doe --- "
     *)
    local
      structure SS = Substring
      structure MT = MatchTree
      (* match strings of the form " *** 2022-05-20 04:28 --- Jane Doe --- " *)
      val re = RE.compileString
            " \\*\\*\\* ([0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}) ---([^-]+)--- "
    in
    fun fromString "" = []
      | fromString (s : string) : t list = let
          (* get the next header in the subtring `ss` *)
          fun getHeader ss = (case RE.find re SS.getc ss
                 of SOME(m as MT.Match({pos, ...}, _), rest) => let
                      val offset = SS.size ss - SS.size pos
                      val MT.Match(_, [MT.Match(dateSS, _), MT.Match(nameSS, _)]) =
                            MT.map (fn {pos, len} => SS.slice(pos, 0, SOME len)) m
                      val date = Timestamp.fromString (SS.string dateSS)
                      val name = SS.string (SS.dropl Char.isSpace (SS.dropr Char.isSpace nameSS))
                      in
                        SOME(offset, date, name, rest)
                      end
                  | NONE => NONE
                (* end case *))
          (* get the list of comments *)
          fun get (date, name, rest, comments) = (case getHeader rest
                 of SOME(offset, date', name', rest') => let
                      val comment = {
                              date = date, name = name,
                              content = Util.lines (SS.string (SS.slice (rest, 0, SOME offset)))
                            }
                      in
                        get (date', name', rest', comment::comments)
                      end
                  | NONE => let
                      val comment = {
                              date = date, name = name,
                              content = Util.lines (SS.string rest)
                            }
                      in
                        List.rev (comment::comments)
                      end
                (* end case *))
          val SOME(_, date, name, rest) = getHeader (Substring.full s)
          in
            get (date, name, rest, [])
          end

    end

(*
    fun fromString s = let
          in
          end
*)

  end;

structure Entry :> sig

    type t

    val readFile : string -> t list

    val id : t -> int
    val summary : t -> string
    val submitter : t -> { name : string, email : string }
    val status : t -> Status.t
    val assignedTo : t -> string
    val openDate : t -> Date.date
    val closeDate : t -> Date.date option
    val os : t -> { os : OperatingSystem.t, version : string }
    val arch : t -> Architecture.t
    val smlnjVersion : t -> string
    val component : t -> Component.t
    val resolution : t -> Resolution.t
    val severity : t -> Severity.t
    val description : t -> string list
    val transcript : t -> string list
    val source : t -> string list
    val comments : t -> Comment.t list

  end = struct

    type t = {
        bugNum : int,                   (* 0: original bug number *)
        status : Status.t,              (* 2: "Open" or "Closed" *)
        submitter : string,             (* 5: "Bug Submitter *)
        email : string,                 (* submitter email extracted from details [12] *)
        assignedTo : string,            (* 7: "John Reppy", ... *)
        openDate : Date.date,           (* 8: "YYYY-MM-DD hh:mm" *)
        closeDate : Date.date option,   (* 9: "" or "YYYY-MM-DD hh:mm" *)
        modifiedDate : Date.date,       (* 10: "YYYY-MM-DD hh:mm" *)
        summary : string,               (* 11: title of bug *)
        details : string list,          (* 12: *)
        architecture : Architecture.t,  (* 16: "None", "All", "DEC Alpha", "HPPA",
                                         *  "PPC", "x86", "MIPS", "SPARC", "Other"
                                         *)
        os : OperatingSystem.t,         (* 17 & 21 *)
        osVersion : string,             (* 17 & 21 *)
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
        smlnjVersion : string,          (* 22: *)
        transcript : string list,       (* 25: *)
        source : string list,           (* 26: *)
        comments : Comment.t list       (* 27: each comment is proceeded text of the form
                                         * " *** 2022-05-20 04:28 --- John Reppy --- "
                                         *)
      }

    fun process (row : string vector) = let
          fun field i = Vector.sub (row, i)
          fun multiLine i = Util.lines (field i)
          (* convert a date field that has the format "YYYY-MM-DD hh:mm" *)
          fun dateField i = Timestamp.fromString (field i)
          (* get the submitter and details *)
          val (submitter, email, details) = let
                val submitter = field 5
                val details = multiLine 12
                val submitter = if submitter = "Bug Submitter" then "" else submitter
                val (submitter, email) = if not(null details)
                      then let
                        val lastLn = List.last details
                        in
                          if String.isPrefix "Submitted via web form by " lastLn
                            then let
                              val s = String.extract (lastLn, 26, NONE)
                              in
(* TODO: extract email address *)
                                (s, "")
                              end
                            else (submitter, "")
                        end
                      else (submitter, "")
                in
                  (submitter, email, details)
                end
          (* get OS info *)
          val {os, version=osVersion} = OperatingSystem.fromString (field 17, field 21)
          in {
            bugNum = valOf (Int.fromString (field 0)),
            (* ignore status_id [1] *)
            status = Status.fromString (field 2),
            (* ignore priority [3] *)
            (* ignore submitter_id [4] *)
            submitter = submitter,
            email = email,
            (* ignore assigned_to_id [6] *)
            assignedTo = field 7,
            openDate = dateField 8,
            closeDate = (case field 9 of "" => NONE | d => SOME(Timestamp.fromString d)),
            modifiedDate = dateField 10,
            summary = field 11,
            details = details,
            (* ignore _votes [13] *)
            (* ignore _voters [14] *)
            (* ignore _votage [15] *)
            architecture = Architecture.fromString (field 16),
            os = os,
            osVersion = osVersion,
            component = Component.fromString (field 18),
            resolution = Resolution.fromString (field 19),
            severity = Severity.fromString (field 20),
            smlnjVersion = field 22,
            (* ignore keywords [23] *)
            (* ignore url [24] *)
            transcript = multiLine 25,
            source = multiLine 26,
            comments = Comment.fromString (field 27)
          } : t end

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
          else raise Fail "file not found"

    fun id (x : t) = #bugNum x
    fun summary (x : t) = #summary x
    fun submitter (x : t) = {name = #submitter x, email = #email x}
    fun status (x : t) = #status x
    fun assignedTo (x : t) = #assignedTo x
    fun openDate (x : t) = #openDate x
    fun closeDate (x : t) = #closeDate x
    fun os (x : t) = {os = #os x, version = #osVersion x}
    fun arch (x : t) = #architecture x
    fun smlnjVersion (x : t) = #smlnjVersion x
    fun component (x : t) = #component x
    fun resolution (x : t) = #resolution x
    fun severity (x : t) = #severity x
    fun description (x : t) = #details x
    fun transcript (x : t) = #transcript x
    fun source (x : t) = #source x
    fun comments (x : t) = #comments x

  end;
