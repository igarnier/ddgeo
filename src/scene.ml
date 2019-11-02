open Gg

type scene = {raw_data : Raw_data.t; meshes : Mesh.t list; camera : Camera.t}

let make ~raw_data ~camera =
  (* TODO: check buffer lengths *)
  {raw_data; meshes = []; camera}

let intersect_ray {raw_data; meshes; _} {Traverse.origin; normal; _} =
  List.fold_left
    (fun hit mesh ->
      let mat = mesh.Mesh.model in
      let imat = M4.inv mat in
      let origin = P3.tr imat origin in
      let normal = V3.tr imat normal in
      let inormal =
        V3.v (1. /. V3.x normal) (1. /. V3.y normal) (1. /. V3.z normal)
      in
      let ray = {Traverse.origin; normal; inormal} in
      match hit with
      | Traverse.NoHit ->
          Mesh.intersect_ray raw_data mesh ray
      | _ ->
          hit)
    Traverse.NoHit
    meshes
