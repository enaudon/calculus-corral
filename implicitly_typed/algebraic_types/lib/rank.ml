module Id = Identifier

type t = int

let init = -1

let mono = 0

module Pools = struct

  (* TODO: Replace this with a more efficient data-structure. *)
  module Map = Map.Make (struct
    type nonrec t = t
    let compare = Pervasives.compare
  end)

  type p = {
    top : t;
    pools : Kind.t Id.Map.t Map.t;
    ranks : t Id.Map.t;
  }

  let empty = {top = init; pools = Map.empty; ranks = Id.Map.empty}

  let push ps =
    let top' = ps.top + 1 in
    {ps with top = top'; pools = Map.add top' Id.Map.empty ps.pools}

  let peek {top; pools; _} = Map.find top pools

  let pop ps =
    {ps with top = ps.top - 1; pools = Map.remove ps.top ps.pools}

  let find_pool : p -> t -> Kind.t Id.Map.t = fun ps rank ->
    try
      Map.find rank ps.pools
    with Not_found ->
      failwith @@ Printf.sprintf
        "Rank.Pools.find_pool: No pool at rank %d.  Top rank is %d."
        rank
        ps.top

  let insert : p -> t -> Identifier.t -> Kind.t -> p =
      fun ps rank id kn ->
    let pool = find_pool ps rank in
    { ps with
      pools = Map.add rank (Id.Map.add id kn pool) ps.pools;
      ranks = Id.Map.add id rank ps.ranks; }

  let remove : p -> t -> Identifier.t -> p = fun ps rank id ->
    let pool = find_pool ps rank in
    { ps with
      pools = Map.add rank (Id.Map.del id pool) ps.pools;
      ranks = Id.Map.del id ps.ranks; }

  let find : p -> t -> Identifier.t -> Kind.t = fun ps rank id ->
    try
      Id.Map.find id @@ find_pool ps rank
    with Not_found ->
      failwith @@ Printf.sprintf
        "Rank.Pools.find: Could not find %s.%d.  Top rank is %d."
        (Id.to_string id)
        rank
        ps.top

  let register ps id = insert ps ps.top id

  let unregister ps id = remove ps ps.top id

  let update ps id1 id2 =
    let rank =
      try
        Id.Map.find id1 ps.ranks
      with Id.Unbound _ ->
        failwith @@ Printf.sprintf
          "Rank.Pools.update: Unbound %s"
          (Id.to_string id1)
    in
    let rank' =
      try
        min rank @@ Id.Map.find id2 ps.ranks
      with Id.Unbound _ ->
        failwith @@ Printf.sprintf
          "Rank.Pools.update: Unbound %s"
          (Id.to_string id2)
    in
    let kn = find ps rank id1 in
    insert (remove ps rank id1) rank' id1 kn

  let is_mono ps id =
    try
      Id.Map.find id ps.ranks >= mono
    with Id.Unbound _ ->
      failwith @@ Printf.sprintf
        "Rank.Pools.is_mono: Unbound %s"
        (Id.to_string id)

end
