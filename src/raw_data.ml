open Gg

type t = {
  vertices : (float, Bigarray.float32_elt) Buff.t;
  colors : (float, Bigarray.float32_elt) Buff.t;
  indices : (int32, Bigarray.int32_elt) Buff.t;
  normals : (float, Bigarray.float32_elt) Buff.t;
  tnormals : (float, Bigarray.float32_elt) Buff.t;
  (* These are _vertices_ normals *)
  texcoords : (float, Bigarray.float32_elt) Buff.t;
}

let vertex_count {vertices; _} =
  let b = Buff.get_buffer vertices in
  Bigarray.Array1.dim b / 3

let triangle_count {indices; _} =
  let b = Buff.get_buffer indices in
  Bigarray.Array1.dim b / 3

let get_vertex {vertices = (module B); _} i =
  Ba.get_v3 B.buff (i * 3)
  [@@inline]

let get_color {colors = (module B); _} i = Ba.get_v3 B.buff (i * 3) [@@inline]

let get_triangle {indices = (module B); _} i =
  Ba.geti_3d B.buff (i * 3)
  [@@inline]

let get_normal {normals = (module B); _} i =
  Ba.get_v3 B.buff (i * 3)
  [@@inline]

let get_tnormal {tnormals = (module B); _} i =
  Ba.get_v3 B.buff (i * 3)
  [@@inline]

let get_texcoord {texcoords = (module B); _} i =
  Ba.get_v2 B.buff (i * 2)
  [@@inline]

let set_random_colors (colors : Buff.f32buffer) =
  Random.self_init () ;
  for i = 0 to (Ba.length colors / 3) - 1 do
    let r = Random.float 1.0 in
    let g = Random.float 1.0 in
    let b = Random.float 1.0 in
    Ba.set_3d colors (i * 3) r g b
  done

let make ~vertices ~colors ~indices ~normals ~tnormals ~texcoords =
  let vlen = Ba.length vertices in
  (* Ensure we have the good number of indices for triangles *)
  assert (Ba.length indices mod 3 = 0) ;
  let indices = Buff.of_i32buffer `Element_array indices in
  let vertices = Buff.of_f32buffer `Array vertices in
  let colors =
    let ba =
      match colors with
      | None ->
          let arr = Ba.create Float32 vlen in
          Bigarray.Array1.fill arr 0.2 ;
          set_random_colors arr ;
          arr
      | Some ba ->
          ba
    in
    Buff.of_f32buffer `Array ba
  in
  let normals = Buff.of_f32buffer `Array normals in
  let tnormals = Buff.of_f32buffer `Array tnormals in
  let texcoords =
    let ba =
      match texcoords with
      | None ->
          let arr = Ba.create Float32 vlen in
          Bigarray.Array1.fill arr 0.2 ;
          arr
      | Some ba ->
          ba
    in
    Buff.of_f32buffer `Array ba
  in
  {vertices; colors; indices; normals; tnormals; texcoords}
