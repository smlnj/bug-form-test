(* labels.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Generate the comma-separated list of labels for an entry
 *)

structure Labels : sig

    val get : Entry.t -> string list

  end = struct

    structure E = Entry
    structure C = Component

    fun get ent = let
          val labels = []
          val labels = (case E.component ent
                 of Installer => "installer" :: labels
                  | Compiler
                  | Basis => "basis-lib" :: labels
                  | CM => "cm" :: labels
                  | MLLex => "ml-lex"
                  | MLYacc => "ml-yacc"
                  | MLBurg => "ml-burg"
                  | SMLNJLib
                  | CML
                  | EXene
                  | FFI
                  | MLULex
                  | MLAntlr
                  | _ => labels
                (* end case *))
          val labels = (case E.resolution ent
                 of E.Won'tFix => "wontfix" :: labels
                  | E.Invalid => "invalid" :: labels
                  | _ => labels
                (* end case *))
          val labels = (if E.isBug ent then "bug" else "enhancement") :: labels
          in
            String.concatWith "," labels
          end

  end
