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
    pools : Id.Set.t Map.t;
    ranks : t Id.Map.t;
  }

  let empty = {top = init; pools = Map.empty; ranks = Id.Map.empty}

  let push ps =
    let top' = ps.top + 1 in
    {ps with top = top'; pools = Map.add top' Id.Set.empty ps.pools}

  let peek {top; pools; _} = Map.find top pools

  let pop ps =
    {ps with top = ps.top - 1; pools = Map.remove ps.top ps.pools}

  let find_pool : p -> t -> Id.Set.t = fun ps rank ->
    try
      Map.find rank ps.pools
    with Not_found ->
      failwith @@ Printf.sprintf
        "Rank.Pools.find_pool: No pool at rank %d.  Top rank is %d."
        rank
        ps.top

  let insert : p -> t -> Identifier.t -> p = fun ps rank id ->
    let pool = find_pool ps rank in
    { ps with
      pools = Map.add rank (Id.Set.add id pool) ps.pools;
      ranks = Id.Map.add id rank ps.ranks; }

  let remove : p -> t -> Identifier.t -> p = fun ps rank id ->
    let pool = find_pool ps rank in
    { ps with
      pools = Map.add rank (Id.Set.del id pool) ps.pools;
      ranks = Id.Map.del id ps.ranks; }

    let register ps id = insert ps ps.top id

    let unregister ps id = remove ps ps.top id

    let update ps id1 id2 =
      let rank = Id.Map.find id1 ps.ranks in
      let rank' = min rank @@ Id.Map.find id2 ps.ranks in
      insert (remove ps rank id1) rank' id1

    let is_mono ps id = Id.Map.find id ps.ranks >= mono

end
