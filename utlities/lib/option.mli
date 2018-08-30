(** {Functions} *)

(**
  [default x opt] evaluates to [y], when [opt] is [Some y].  Otherwise,
  it evaluates to [x].
 *)
val default : 'a -> 'a option -> 'a

(**
  [fold fn opt init] evaluates to [fn init x], when [opt] is [Some x].
  Otherwise, [fold] evaluates to [init].
 *)
val fold : ('a -> 'b -> 'a) -> 'a -> 'b option -> 'a
