module Id = Identifier

type rank = int

let mono = 0

type t = {
  pools : Id.Set.t Stack.t;
  ranks : rank Id.Map.t;
}

let ranks_find : t -> Id.t -> rank = fun ps id ->
  try
    Id.Map.find id ps.ranks
  with Id.Unbound _ ->
    failwith @@ Printf.sprintf
      "Inference_variable_state.ranks_find: Unbound %s"
      (Id.to_string id)

let top : t -> rank = fun ps ->
  Stack.size ps.pools - 1

let insert : t -> rank -> Id.t -> t = fun ps rank id ->
  let pool = Stack.get rank ps.pools in
  { pools = Stack.update rank (Id.Set.add id pool) ps.pools;
    ranks = Id.Map.add id rank ps.ranks }

let remove : t -> rank -> Id.t -> t = fun ps rank id ->
  let pool = Stack.get rank ps.pools in
  { pools = Stack.update rank (Id.Set.del id pool) ps.pools;
    ranks = Id.Map.del id ps.ranks }

let empty = {pools = Stack.empty; ranks = Id.Map.empty}

let push ps =
  {ps with pools = Stack.push Id.Set.empty ps.pools}

let peek ps =
  Stack.peek ps.pools

let pop ps =
  {ps with pools = Stack.pop ps.pools}

let register ps id =
  insert ps (top ps) id

let unregister ps id =
  remove ps (top ps) id

let update ps id1 id2 =
  let rank = ranks_find ps id1 in
  let rank' = min rank @@ ranks_find ps id2 in
  insert (remove ps rank id1) rank' id1

let is_mono ps id =
  ranks_find ps id >= mono
