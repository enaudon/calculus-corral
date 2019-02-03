# Known issues

## Type variable naming

Distinct type variables with the same name and overlapping scope are
incorrectly treated as a single type variable.

```
> \x . x : exists A . A -> A : exists A . (A -> A) -> A -> A ;
(* Results in "type variable 'A' occurs in 'A -> A' *)
```

## Rigid type variables scoping

The scoping of rigid type variables is not checked, so they may escape
their scope.

```
> \x . x : forall A . A -> A ;
(* Should give "type variable 'A' would escape its scope" *)
```
