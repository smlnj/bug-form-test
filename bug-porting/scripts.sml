(* some expressions that do useful things with the bug database.
 * Run the command
 *
 *      sml -m sources.cm
 *
 * and then use the code below
 *)

(* load the database *)
val db = DB.load {bugs="bug-db.csv", features="feature-db.csv"};

(* print keywords *)
fun prKeywords db = List.app
      (fn (n, kw) => print(concat[
              StringCvt.padRight #" " 4 (Int.toString n ^ ":"), " ", kw, "\n"
            ]))
        (DB.mapPartial
          (fn ent => case Entry.keywords ent of "" => NONE | kw => SOME(Entry.id ent, kw))
          db);

(* list submitters *)
fun prSubmitters db = List.app
      (fn (n, who) => print(concat[
              StringCvt.padRight #" " 4 (Int.toString n ^ ":"), " ", who, "\n"
            ]))
        (DB.mapPartial
          (fn ent => (case Entry.submitter ent
                 of {name="", ...} => NONE
                  | {name, ...} => SOME(Entry.id ent, name)
                (* end case *)))
          db);

(* get a bug by number *)
fun get db id = valOf (DB.find (db, id));
