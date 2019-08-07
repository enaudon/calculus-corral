# Known issues

## Generated inference variable naming

Generated inference variables are named without consideration of
in-scope user-defined inference variables, and may therefore share
the same name.

```
let app = \x . \f . f x in
\x . app (x : exists 'A :: * . 'A -> 'A) ;

(*
  Type as: ('A -> 'A) -> (('A -> 'A) -> 'A) -> 'A
  Instead of: ('A -> 'A) -> (('A -> 'A) -> 'B) -> 'B
*)
```

Since generated and user-defined inference variables are internally
distinct, the returned type is correct.  However, this is not clear from
the way it's printed.

## User-defined inference variable scoping

Distinct user-defined inference variables with the same name and
overlapping scope are incorrectly treated as a single inference
variable.

```
> \x . x : exists A . A -> A : exists A . (A -> A) -> A -> A ;
(* Results in "type variable 'A' occurs in 'A -> A' *)
```

## Rigid inference variables scoping

The scoping of rigid inference variables is not checked, so they may escape
their scope.

```
> \x . x : forall A . A -> A ;
(* Should give "type variable 'A' would escape its scope" *)
```

## Second-class recursive types

Mu-binders may only occur at the top-level of types.

```
> List = mu List :: * => * . \A :: * . [Nil; Cons : {head : A; tail : List A}] ;
List
  : * => *
  = mu List :: * => * . \A :: * . [Nil : {}; Cons : {head : A; tail : List A}] ;
(* Results in...
Fatal error: exception Invalid_argument("Isorecursive_types_imp__Type.Inferencer.unify: unexpected type abstraction")
*)
```
