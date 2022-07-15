(* labels.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generate the comma-separated list of labels for an entry
 *)

structure Labels : sig

    val get : Entry.t -> string

  end = struct

    structure E = Entry
    structure C = Component
    structure R = Resolution
    structure SMap = RedBlackMapFn (
      struct
        type ord_key = string
        val compare = String.compare
      end)

    (* mapping from common keywords to labels *)
    val keywords = [
            ("basis",	                "basis-lib"),
            ("building",	        "installation"),
            ("ckit",	                "ckit-lib"),
            ("cm",	                "cm"),
            ("cml",	                "cml"),
            ("cygwin",                  "cygwin"),
            ("darwin10",	        "macos"),
            ("documentation",	        "documentation"),
            ("elaboration",	        "compiler-elab"),
            ("exene",	                "exene"),
            ("flint",	                "compiler-flint"),
            ("garbage collection",	"gc"),
            ("gc",	                "gc"),
            ("hashcons",	        "smlnj-util-lib"),
            ("heap2exec",	        "heap2exec"),
            ("ieeereal",	        "floating-point"),
            ("install",                 "installation"),
            ("installation",	        "installation"),
            ("json",	                "json-lib"),
            ("linux",	                "linux"),
            ("math",	                "floating-point"),
            ("ml-lex",	                "ml-lex"),
            ("ml-ulex",	                "ml-ulex"),
            ("ml-yacc",	                "ml-yacc"),
            ("mountain lion",	        "macos"),
            ("nlffi",	                "nlffi-lib"),
            ("osx",	                "macos"),
            ("postinstall",	        "installation"),
            ("reals",	                "floating-point"),
            ("snow leopard",	        "macos"),
            ("socket",	                "sockets"),
            ("successor-ml",	        "successor-ml"),
            ("unsafe operations",	"unsafe-struct"),
            ("windows",	                "windows")
          ]

    fun addNew ([], label : string) = [label]
      | addNew (lab::labs, label) = if (lab = label)
          then lab::labs
          else lab::addNew(labs, label)

    fun get ent = let
          val labels = ["gforge"]
          val labels = (case E.component ent
                 of C.Installer => "installer" :: labels
                  | C.Compiler => "compiler" :: labels
                  | C.Basis => "basis-lib" :: labels
                  | C.CM => "cm" :: labels
                  | C.MLLex => "ml-lex" :: labels
                  | C.MLYacc => "ml-yacc" :: labels
                  | C.MLBurg => "ml-burg" :: labels
                  | C.SMLNJLib => "smlnj-lib" :: labels
                  | C.CML => "cml" :: labels
                  | C.EXene => "exene" :: labels
                  | C.FFI => "nlffigen" :: labels
                  | C.MLULex => "ml-ulex" :: labels
                  | C.MLAntlr => "ml-antlr" :: labels
                  | _ => labels
                (* end case *))
          (* add labels based on keywords *)
          val labels = (case E.keywords ent
                 of "" => labels
                  | kws => let
                      fun chk ((kw, lab), labels) = if String.isSubstring kw kws
                            then addNew (labels, lab)
                            else labels
                      in
                        List.foldl chk labels keywords
                      end
                (* end case *))
          val labels = (case E.resolution ent
                 of R.Won'tFix => "wontfix" :: labels
                  | R.Invalid => "invalid" :: labels
                  | _ => labels
                (* end case *))
          val labels = (if E.isBug ent then "bug" else "enhancement") :: labels
          in
            String.concatWith "," labels
          end

  end
