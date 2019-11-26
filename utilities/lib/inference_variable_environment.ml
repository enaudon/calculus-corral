module Id = Identifier

type rank = int

type 'a t =
  { pools : 'a Id.Map.t Stack.t;
    ranks : rank Id.Map.t }

let ranks_find : Id.t -> 'a t -> rank =
 fun id ps ->
   try Id.Map.find id ps.ranks
   with Id.Unbound _ ->
     failwith
     @@ Printf.sprintf
          "Inference_variable_state.ranks_find: Unbound %s"
          (Id.to_string id)

let pools_find : rank -> Id.t -> 'a t -> 'a =
 fun rank id ps ->
   try Id.Map.find id @@ Stack.get rank ps.pools
   with Id.Unbound _ ->
     failwith
     @@ Printf.sprintf
          "Inference_variable_state.pools_find: Could not find %s.%d."
          (Id.to_string id)
          rank

let top : 'a t -> rank = fun ps -> Stack.size ps.pools - 1

let insert : rank -> Id.t -> 'a -> 'a t -> 'a t =
 fun rank id data ps ->
   let pool = Stack.get rank ps.pools in
   { pools = Stack.update rank (Id.Map.add id data pool) ps.pools;
     ranks = Id.Map.add id rank ps.ranks }

let remove : rank -> Id.t -> 'a t -> 'a t =
 fun rank id ps ->
   let pool = Stack.get rank ps.pools in
   { pools = Stack.update rank (Id.Map.del id pool) ps.pools;
     ranks = Id.Map.del id ps.ranks }

let empty = {pools = Stack.empty; ranks = Id.Map.empty}

let push ps = {ps with pools = Stack.push Id.Map.empty ps.pools}

let peek ps = Stack.peek ps.pools

let pop ps = {ps with pools = Stack.pop ps.pools}

let update id1 id2 (ps : 'a t) : 'a t =
  let rank = ranks_find id1 ps in
  let rank' = min rank @@ ranks_find id2 ps in
  let data = pools_find rank id1 ps in
  ps |> remove rank id1 |> insert rank' id1 data

let insert id data ps = insert (top ps) id data ps

let remove id ps = remove (top ps) id ps

let find id ps = pools_find (ranks_find id ps) id ps
