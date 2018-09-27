(** {Functions} *)

(**
  [default x opt] evaluates to [y], when [opt] is [Some y].  Otherwise,
  it evaluates to [x].
 *)
val default : 'a -> 'a option -> 'a

(**
  [iter fn opt] applies [fn] to [x], when [opt] is [Some x].  Otherwise,
  it does nothing.
 *)
val iter : ('a -> unit) -> 'a option -> unit

(**
  [map fn opt] evaluates to [Some (fn x)], when [opt] is [Some x].
  Otherwise, [map] evaluates to [None].
 *)
val map : ('a -> 'b) -> 'a option -> 'b option

(**
  [fold fn opt init] evaluates to [fn init x], when [opt] is [Some x].
  Otherwise, [fold] evaluates to [init].
 *)
val fold : ('a -> 'b -> 'b) -> 'a option -> 'b -> 'b
