open Gg

type t = {
  tri_start : int;
  tri_end : int;
  (* texture   : Texture.t; *)
  model : M4.t;
  state : Traverse.Tree.t;
  node : Bih.node;
}

let make tri_start tri_end model raw_data =
  let objects =
    Array.init (tri_end - tri_start + 1) (fun i -> tri_start + i)
  in
  Format.eprintf "Generating BIH...%!" ;
  let (state, node) = Traverse.Tree.build raw_data 5 objects in
  (* Format.eprintf "%a\n" Bih.pp node ; *)
  Format.eprintf "Done.\n%!" ;
  {tri_start; tri_end; model; state; node}

let pre_compose mesh mat = {mesh with model = M4.mul mesh.model mat}

let scale sv mesh = pre_compose mesh M4.(scale3 sv)

let rotate axis angle mesh = pre_compose mesh M4.(rot3_axis axis angle)

let translate vec mesh = pre_compose mesh M4.(move3 vec)

let intersect_ray raw_data mesh ray =
  Traverse.traverse raw_data mesh.state.index ray 0.0 max_float mesh.node
