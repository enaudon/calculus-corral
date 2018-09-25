(** Row

  Rows are un-ordered collections of data, where each datum is indexed
  by an identifier label.
 *)

(** {1 Types} *)

(** The type of rows containing data of type ['a]. *)
type 'a t

(** {1 Constructors and Destructors} *)

(** [of_list fields] constructs a row from [fields]. *)
val of_list : (Identifier.t * 'a) list -> 'a t

(** [to_list row] constructs a list of fields from [row]. *)
val to_list : 'a t -> (Identifier.t * 'a) list

(** {1 Iterators} *)

(** [iter fn row] applies [fn] to each field in [row]. *)
val iter : (Identifier.t -> 'a -> unit) -> 'a t -> unit

(**
  [map fn row] constructs a new row by applying [fn] to each datum in
  [row].
 *)
val map : ('a -> 'b) -> 'a t -> 'b t

(**
  [fold fn m init] computes [fn idN valN (... (fn id0 val0 init)...)],
  where the [id]'s and [val]'s come from the fields in [m].
 *)
val fold : (Identifier.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

(** {1 Scanners} *)

(**
  [for_all2 pred row1 row2] checks that the pair-wise fields in [row1]
  and [row2] satisfy [pred].
 *)
val for_all2 :
  (Identifier.t -> 'a -> Identifier.t -> 'b -> bool) ->
  'a t ->
  'b t ->
  bool
