(* util.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure RE = RegExpFn(
    structure P = AwkSyntax
    structure E = BackTrackEngine)

structure Util =
  struct

    val toLower = String.map Char.toLower

    val lines = String.fields (fn #"\n" => true | _ => false)

  end
