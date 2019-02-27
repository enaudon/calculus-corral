module Id = Identifier

type rank = int

let mono = 0

type t = {
  pools : Kind.t Id.Map.t Stack.t;
  ranks : rank Id.Map.t;
}

let ranks_find : t -> Id.t -> rank = fun ps id ->
  try
    Id.Map.find id ps.ranks
  with Id.Unbound _ ->
    failwith @@ Printf.sprintf
      "Inference_variable_state.ranks_find: Unbound %s"
      (Id.to_string id)

let pools_find : t -> rank -> Id.t -> Kind.t = fun ps rank id ->
  try
    Id.Map.find id @@ Stack.get rank ps.pools
  with Id.Unbound _ ->
    failwith @@ Printf.sprintf
      "Inference_variable_state.ranks_find: Could not find %s.%d."
      (Id.to_string id)
      rank

let top : t -> rank = fun ps ->
  Stack.size ps.pools - 1

let insert : t -> rank -> Id.t -> Kind.t -> t = fun ps rank id kn ->
  let pool = Stack.get rank ps.pools in
  { pools = Stack.update rank (Id.Map.add id kn pool) ps.pools;
    ranks = Id.Map.add id rank ps.ranks }

let remove : t -> rank -> Id.t -> t = fun ps rank id ->
  let pool = Stack.get rank ps.pools in
  { pools = Stack.update rank (Id.Map.del id pool) ps.pools;
    ranks = Id.Map.del id ps.ranks }

let empty = {pools = Stack.empty; ranks = Id.Map.empty}

let push ps =
  {ps with pools = Stack.push Id.Map.empty ps.pools}

let peek ps =
  Stack.peek ps.pools

let pop ps =
  {ps with pools = Stack.pop ps.pools}

let register ps id kn =
  insert ps (top ps) id kn

let unregister ps id =
  remove ps (top ps) id

let update ps id1 id2 =
  let rank = ranks_find ps id1 in
  let rank' = min rank @@ ranks_find ps id2 in
  let kn = pools_find ps rank id1 in
  insert (remove ps rank id1) rank' id1 kn

let is_mono ps id =
  ranks_find ps id >= mono
