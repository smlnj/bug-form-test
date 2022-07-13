(* name-map.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Mapping gforge names to GitHub names for selected people.
 *)

structure NameMap : sig

    (* convert a name if possible *)
    val map : string -> string

    (* rewrite a string by replacing known names with "@" mentions *)
    val rewrite : string -> string

  end = struct

    structure MT = MatchTree
    structure SMap = ListMapFn (
      struct
        type ord_key = string
        val compare = String.compare
      end)

    (* name/GitHub name pairs *)
    val tbl = [
            ("Alley Stoughton", "@alleystoughton"),
            ("Benjamin Quiring", "@bquiring"),
            ("Christopher League", "@league"),
            ("Dan Licata", "@dlicata335"),
            ("DanGrossman", "@djg98115"),
            ("David MacQueen", "@dmacqueen"),
            ("Harrison Grodin", "@HarrisonGrodin"),
            ("John Reppy", "@JohnReppy"),
            ("Jon Riehl", "@jriehl"),
            ("Karl Crary", "@kcrary"),
            ("Kavon Farvardin", "@kavon"),
            ("Konrad Slind", "@konrad-slind"),
            ("Lars Bergstrom", "@larsbergstrom"),
            ("Masaya Saito", "@mmsaito"),
            ("matt rice", "@ratmice"),
            ("Matthew Fluet", "@MatthewFluet"),
            ("Matthias Blume", "@mathias-blume"),
            ("Mike Rainey", "@mikerainey"),
            ("Phil Clayton", "@pclayton"),
            ("Simon Hollingshead", "@simonhollingshead"),
            ("Skye Soss", "@Skyb0rg007"),
            ("Vesa Norrman", "@vesanorrman")
          ]

    val map = let
          val nMap = List.foldl SMap.insert' SMap.empty tbl
          in
            fn name => (case SMap.find (nMap, name) of SOME n => n | NONE => name)
          end

  (* implement rewriting using RE.match *)
    local
      fun mkRule (s1, s2) = (s1, fn (MT.Match({pos:int, len}, _)) => s2)
      val matcher : (char, int) StringCvt.reader -> (string, int) StringCvt.reader =
            RE.match (List.map mkRule tbl)
    in
    fun rewrite txt = let
          val len = size txt
          fun getc i = if (i < len) then SOME(String.sub(txt, i), i+1) else NONE
          val matcher = matcher getc
          fun consSlice (start, i, strs) = if (start < i)
                then substring(txt, start, i-start) :: strs
                else strs
          fun lp (start, i, strs) = (case matcher i
                 of SOME(s, j) => if (start < i)
                      then lp (j, j, s :: consSlice(start, i, strs))
                      else lp (j, j, s::strs)
                  | NONE => if (i < len)
                      then lp (start, i+1, strs)
                      else consSlice(start, i, strs)
                (* end case *))
          in
            concat (List.rev (lp (0, 0, [])))
          end
    end (* local *)

  end
