type t =
  | Base

(* Utilities *)

(**
  There are no kind variables, so alpha-equivalence is just structural
  equivalence.
 *)
let alpha_equivalent = Pervasives.(=)

let to_string kn = match kn with
  | Base -> "*"

(* Constructors *)

let base = Base
