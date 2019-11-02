open Gg
open Tgl3
open Utils
open Scene

(* ------------------------------------------------------------------------- *)
(* typedefs *)

(* engine state *)
type gl_state = {
  vertex_array_id : int;
  vertex_shader : string;
  vertex_shader_id : int;
  fragment_shader : string;
  fragment_shader_id : int;
  program_id : int;
}

type attribute = Vertex | Color | Normal

(* ------------------------------------------------------------------------- *)
(* helpers *)

let get_string len f =
  let a = bigarray_create Bigarray.char len in
  f a ; Gl.string_of_bigarray a

let m4_to_ba m =
  let arr = bigarray_create Bigarray.float32 16 in
  arr.{0} <- M4.e00 m ;
  arr.{1} <- M4.e10 m ;
  arr.{2} <- M4.e20 m ;
  arr.{3} <- M4.e30 m ;
  arr.{4} <- M4.e01 m ;
  arr.{5} <- M4.e11 m ;
  arr.{6} <- M4.e21 m ;
  arr.{7} <- M4.e31 m ;
  arr.{8} <- M4.e02 m ;
  arr.{9} <- M4.e12 m ;
  arr.{10} <- M4.e22 m ;
  arr.{11} <- M4.e32 m ;
  arr.{12} <- M4.e03 m ;
  arr.{13} <- M4.e13 m ;
  arr.{14} <- M4.e23 m ;
  arr.{15} <- M4.e33 m ;
  arr

(* ------------------------------------------------------------------------- *)
(* attribute to index map *)

let attribute_index : attribute -> int = function
  | Vertex ->
      0
  | Color ->
      1
  | Normal ->
      2

let attribute_count : attribute -> int = function
  | Vertex ->
      3
  | Color ->
      3
  | Normal ->
      3

let map_meshes f scene = {scene with meshes = List.map f scene.meshes}

(* ------------------------------------------------------------------------- *)
(* engine initialisation *)

let compile_vertex_shader src =
  let get_shader sid e = get_int (Gl.get_shaderiv sid e) in
  let sid = Gl.create_shader Gl.vertex_shader in
  Gl.shader_source sid src ;
  Gl.compile_shader sid ;
  if get_shader sid Gl.compile_status = Gl.true_ then Ok sid
  else
    let len = get_shader sid Gl.info_log_length in
    let log = get_string len (Gl.get_shader_info_log sid len None) in
    Gl.delete_shader sid ;
    Error (`Msg log)

let compile_fragment_shader src =
  let get_shader sid e = get_int (Gl.get_shaderiv sid e) in
  let sid = Gl.create_shader Gl.fragment_shader in
  Gl.shader_source sid src ;
  Gl.compile_shader sid ;
  if get_shader sid Gl.compile_status = Gl.true_ then Ok sid
  else
    let len = get_shader sid Gl.info_log_length in
    let log = get_string len (Gl.get_shader_info_log sid len None) in
    Gl.delete_shader sid ;
    Error (`Msg log)

let vertex_shader v =
  Printf.sprintf
    "\n\
    \  #version %s core\n\
    \  layout(location = 0) in vec3 vertex;\n\
    \  layout(location = 1) in vec3 color;\n\
    \  layout(location = 2) in vec3 normal;\n\
    \  out vec4 v_color;\n\
    \  uniform mat4 trans;\n\n\
    \  vec3 light_color = vec3(1.0, 1.0, 1.0);\n\
    \  float coeff = clamp(dot(normal, vec3(0.6, 0.0, 0.2)), 0, 1);\n\
    \  void main()\n\
    \  {\n\
    \    vec3 shade = color + coeff * light_color;\n\
    \    v_color = vec4(shade, 1.0);\n\
    \    gl_Position = trans * vec4(vertex, 1);\n\
    \  }"
    v

let fragment_shader v =
  Printf.sprintf
    "\n\
    \  #version %s core\n\
    \  in vec4 v_color;\n\
    \  out vec4 color;\n\
    \  void main() { color = v_color; }"
    v

let get_program pid e = get_int (Gl.get_programiv pid e)

let init ~clear_color ~vertex_shader ~fragment_shader =
  let (r, g, b) = V3.to_tuple clear_color in
  Gl.clear_color r g b 0.0 ;
  Gl.enable Gl.depth_test ;
  Gl.depth_func Gl.less ;
  Gl.enable Gl.cull_face_enum ;
  let vertex_array_id = get_int (Gl.gen_vertex_arrays 1) in
  Gl.bind_vertex_array vertex_array_id ;
  compile_vertex_shader vertex_shader
  >>= fun vshader_id ->
  compile_fragment_shader fragment_shader
  >>= fun fshader_id ->
  let pid = Gl.create_program () in
  Gl.attach_shader pid vshader_id ;
  Gl.delete_shader vshader_id ;
  Gl.attach_shader pid fshader_id ;
  Gl.delete_shader fshader_id ;
  Gl.bind_attrib_location pid 0 "vertex" ;
  (* nommage de l'argument *)
  Gl.bind_attrib_location pid 1 "color" ;
  (* nommage de l'argument *)
  Gl.bind_attrib_location pid 2 "normal" ;
  (* nommage de l'argument *)
  Gl.link_program pid ;
  if get_program pid Gl.link_status = Gl.true_ then
    Ok
      {
        vertex_array_id;
        vertex_shader;
        vertex_shader_id = vshader_id;
        fragment_shader;
        fragment_shader_id = fshader_id;
        program_id = pid;
      }
  else
    let len = get_program pid Gl.info_log_length in
    let log = get_string len (Gl.get_program_info_log pid len None) in
    Gl.delete_program pid ;
    Error (`Msg log)

let activate_attrib attrib array_buff =
  let attr_index = attribute_index attrib in
  let attr_count = attribute_count attrib in
  let attr_glelt = Buff.get_gl_elt array_buff in
  Gl.enable_vertex_attrib_array attr_index ;
  Gl.bind_buffer Gl.array_buffer (Buff.get_gl_id array_buff) ;
  Gl.vertex_attrib_pointer attr_index attr_count attr_glelt false 0 (`Offset 0)

(* ------------------------------------------------------------------------- *)
(* scene data *)

let render_mesh program_id scene mesh =
  let {Raw_data.vertices; colors; normals; indices; _} =
    scene.Scene.raw_data
  in
  let mesh_mat = mesh.Mesh.model in
  let view_mat = Camera.view scene.camera in
  let proj_mat = Camera.projection scene.camera in
  let mat = M4.mul proj_mat (M4.mul view_mat mesh_mat) in
  (* let s = Format.asprintf "%a" M4.pp mat in
   * Format.eprintf "%s\n\n%!" s ; *)
  let matrix_id = Gl.get_uniform_location program_id "trans" in
  (* Gl.enable Gl.cull_face_enum ;
   * Gl.cull_face Gl.front ; *)
  Gl.uniform_matrix4fv matrix_id 1 false (m4_to_ba mat) ;
  (* First (cf the '0') argument of vertex shader is a 3-vec, set its source *)
  activate_attrib Vertex vertices ;
  (* Second argument of vertex shader is a 3-vec (color) *)
  activate_attrib Color colors ;
  (* Third argument of vertex shader is a 3-vec (normal) *)
  activate_attrib Normal normals ;
  (* bind the index buffer *)
  Gl.bind_buffer Gl.element_array_buffer (Buff.get_gl_id indices) ;
  let count = Buff.dim indices in
  Gl.draw_elements Gl.triangles count Gl.unsigned_int (`Offset 0) ;
  Gl.disable_vertex_attrib_array (attribute_index Vertex) ;
  Gl.disable_vertex_attrib_array (attribute_index Color) ;
  Gl.disable_vertex_attrib_array (attribute_index Normal)

let render program_id scene =
  Gl.clear Gl.(color_buffer_bit + depth_buffer_bit) ;
  Gl.use_program program_id ;
  (* Gl.polygon_mode Gl.front_and_back Gl.line ; *)
  List.iter (fun mesh -> render_mesh program_id scene mesh) scene.meshes
