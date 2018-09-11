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

  let insert : p -> t -> Identifier.t -> p = fun ps rank id ->
    let {top; pools; ranks} = ps in
    let pools' =
      try
        Map.add rank (Id.Set.add id @@ Map.find rank pools) pools
      with Not_found ->
        failwith @@ Printf.sprintf
          "Rank.Pools.insert: Not_found %s.%d.  Current rank is %d."
          (Id.to_string id)
          rank
          top
    in
    {ps with pools = pools'; ranks = Id.Map.add id rank ranks}

  let remove : p -> t -> Identifier.t -> p = fun ps rank id ->
    let {top; pools; ranks} = ps in
    let pools' =
      try
        Map.add rank (Id.Set.del id @@ Map.find rank pools) pools
      with Not_found ->
        failwith @@ Printf.sprintf
          "Rank.Pools.remove: Not_found %s.%d.  Current rank is %d."
          (Id.to_string id)
          rank
          top
    in
    {ps with pools = pools'; ranks = Id.Map.del id ranks}

    let register ps id = insert ps ps.top id

    let unregister ps id = remove ps ps.top id

    let update ps id1 id2 =
      let rank = Id.Map.find id1 ps.ranks in
      let rank' = min rank @@ Id.Map.find id2 ps.ranks in
      insert (remove ps rank id1) rank' id1

    let is_mono ps id = Id.Map.find id ps.ranks >= mono

end
