open Gg
open Tgl3
open Utils

type f32buffer = (float, Bigarray.float32_elt) bigarray

type i32buffer = (int32, Bigarray.int32_elt) bigarray

module type S = sig
  type elt

  type kind

  val gl_kind : int

  val buff : (elt, kind) bigarray

  val gl_id : int
end

type ('elt, 'kind) t = (module S with type elt = 'elt and type kind = 'kind)

type float32_glbuffer = {fb : f32buffer; gl_id : int}

type int32_glbuffer = {ib : i32buffer; gl_id : int}

(* ------------------------------------------------------------------------- *)
(* buffer manipulation *)

let generate_array_buffer_id ba =
  let id = get_int (Gl.gen_buffers 1) in
  let sz = Gl.bigarray_byte_size ba in
  Gl.bind_buffer Gl.array_buffer id ;
  Gl.buffer_data Gl.array_buffer sz (Some ba) Gl.static_draw ;
  id

let generate_element_array_buffer_id ba =
  let id = get_int (Gl.gen_buffers 1) in
  let sz = Gl.bigarray_byte_size ba in
  Gl.bind_buffer Gl.element_array_buffer id ;
  Gl.buffer_data Gl.element_array_buffer sz (Some ba) Gl.static_draw ;
  id

let generate_buffer_id kind ba =
  match kind with
  | `Array ->
      generate_array_buffer_id ba
  | `Element_array ->
      generate_element_array_buffer_id ba

let of_f32buffer :
    [`Array | `Element_array] -> f32buffer -> (float, Bigarray.float32_elt) t =
 fun kind buff ->
  let gl_id = generate_buffer_id kind buff in
  ( module struct
    type elt = float

    type kind = Bigarray.float32_elt

    let gl_kind = Gl.float

    let buff = buff

    let gl_id = gl_id
  end )

let of_i32buffer :
    [`Array | `Element_array] -> i32buffer -> (int32, Bigarray.int32_elt) t =
 fun kind buff ->
  let gl_id = generate_buffer_id kind buff in
  ( module struct
    type elt = int32

    type kind = Bigarray.int32_elt

    let gl_kind = Gl.unsigned_int

    let buff = buff

    let gl_id = gl_id
  end )

let pp_f32 (a : f32buffer) fmtr =
  for i = 0 to Bigarray.Array1.dim a - 1 do
    Format.fprintf fmtr "%f; " a.{i}
  done ;
  Format.fprintf fmtr "\n"

let pp_i32 (a : i32buffer) fmtr =
  for i = 0 to Bigarray.Array1.dim a - 1 do
    Format.fprintf fmtr "%ld; " a.{i}
  done ;
  Format.fprintf fmtr "\n"

(* let get_gl_elt : type k e r. (k, e, r) t -> int =
 *  fun buff ->
 *   match buff with
 *   | Array {data; gl_id = _} -> (
 *       let k = Bigarray.Array1.kind data in
 *       match k with
 *       | Bigarray.Float32 ->
 *           Gl.float
 *       | Bigarray.Int32 ->
 *           Gl.unsigned_int
 *       | _ ->
 *           assert false )
 *   | Element_array {data; gl_id = _} -> (
 *       let k = Bigarray.Array1.kind data in
 *       match k with
 *       | Bigarray.Float32 ->
 *           Gl.float
 *       | Bigarray.Int32 ->
 *           Gl.unsigned_int
 *       | _ ->
 *           assert false ) *)

let dim : type e k. (e, k) t -> int =
 fun (module B) -> Bigarray.Array1.dim B.buff

let pp : type e k. Format.formatter -> (e, k) t -> unit =
 fun fmtr (module B) ->
  match Bigarray.Array1.kind B.buff with
  | Bigarray.Float32 ->
      pp_f32 B.buff fmtr
  | Bigarray.Int32 ->
      pp_i32 B.buff fmtr
  | _ ->
      assert false

let get_buffer : type e k. (e, k) t -> (e, k) bigarray =
 fun (module B) -> B.buff

let get_gl_id : type e k. (e, k) t -> int = fun (module B) -> B.gl_id

let get_gl_elt : type e k. (e, k) t -> int = fun (module B) -> B.gl_kind
