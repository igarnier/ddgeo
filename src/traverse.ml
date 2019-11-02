open Gg

(* a ray is a line going through [origin] and colinear to [normal] *)
type ray = {
  origin : V3.t;
  normal : V3.t;
  (* unit length vector *)
  inormal : V3.t; (* normal reciprocal *)
}

(* whenever a ray successfully hits a triangle, we store the point on the ray [t], and the point on the
 * triangle [(u,v)] (barycentric coords). This last element could be discarded for our purposes. *)
type hit = {
  t : float;
  (* intersection distance on the ray *)
  u : float;
  (* fst barycentric coord. *)
  v : float; (* snd barycentric coord. *)
}

type tri_index = int

type outcome = NoHit | Hit of tri_index * hit

let to_array v = [|V3.x v; V3.y v; V3.z v|]

let aabb_of_triangle v1 v2 v3 =
  let v1 = to_array v1 in
  let v2 = to_array v2 in
  let v3 = to_array v3 in
  Aabb.(
    let mins = array_min v1 (array_min v2 v3)
    and maxs = array_max v1 (array_max v2 v3) in
    {mins; maxs})

module Elt = struct
  type t = int (* triangle index *)

  let dim = 3

  type state = Raw_data.t

  let extents (raw_data : state) (tri_index : t) =
    let indices = Buff.get_buffer raw_data.indices in
    let vertices = Buff.get_buffer raw_data.vertices in
    let (i1, i2, i3) = Ba.geti_3d indices (tri_index * 3) in
    let v1 = Ba.get_v3 vertices (i1 * 3) in
    let v2 = Ba.get_v3 vertices (i2 * 3) in
    let v3 = Ba.get_v3 vertices (i3 * 3) in
    aabb_of_triangle v1 v2 v3
end

module Tree = Bih.Make (Elt)

let epsilon = 0.0001

(* Moller-Trumbore ray-triangle intersection *)
let moller_trumbore ray p0 p1 p2 =
  let edge1 = V3.sub p1 p0 in
  let edge2 = V3.sub p2 p0 in
  let pvec = V3.cross ray.normal edge2 in
  let det = V3.dot edge1 pvec in
  if det > ~-.epsilon && det < epsilon then None
  else
    let inv_det = 1.0 /. det in
    let tvec = V3.sub ray.origin p0 in
    let ucoord = V3.dot tvec pvec *. inv_det in
    if ucoord < 0.0 || ucoord > 1.0 then None
    else
      let qvec = V3.cross tvec edge1 in
      let vcoord = V3.dot ray.normal qvec *. inv_det in
      if vcoord < 0.0 || ucoord +. vcoord > 1.0 then None
      else
        let hit_dist = V3.dot edge2 qvec *. inv_det in
        Some {t = hit_dist; u = ucoord; v = vcoord}

(* Intersect a ray with all the triangles between [tri_start; tri_end].
   Picks closest hit, if any. *)
let intersect_ray raw_data tri_index ray tmin tmax tri_start tri_end =
  let min_hit = ref NoHit in
  for i = tri_start to tri_end do
    let (t1, t2, t3) = Raw_data.get_triangle raw_data tri_index.(i) in
    if V3.dot ray.normal (Raw_data.get_tnormal raw_data tri_index.(i)) > 0.0
    then ()
    else
      let v1 = Raw_data.get_vertex raw_data t1 in
      let v2 = Raw_data.get_vertex raw_data t2 in
      let v3 = Raw_data.get_vertex raw_data t3 in
      let res = moller_trumbore ray v1 v2 v3 in
      match res with
      | Some ({t; _} as hit) -> (
        match !min_hit with
        | NoHit ->
            min_hit := Hit (tri_index.(i), hit)
        | Hit (_, {t = t'; _}) ->
            if t < t' && tmin <= t && t <= tmax then
              min_hit := Hit (tri_index.(i), hit)
            else () )
      | None ->
          ()
  done ;
  !min_hit

let fmin (x : float) (y : float) = if x < y then x else y

let fmax (x : float) (y : float) = if x < y then y else x

let proj v i =
  match i with 0 -> V3.x v | 1 -> V3.y v | 2 -> V3.z v | _ -> assert false
  [@@inline]

(* Ray traversal. Could be made simpler by symmetrising the code, at the cost of some alloc. *)
let rec traverse raw_data tri_index ray tmin tmax bih =
  if tmin >= tmax then NoHit
  else
    match bih with
    | Bih.Leaf {start; stop} ->
        (* Format.eprintf "<%d-%d>\n%!" start stop ; *)
        intersect_ray raw_data tri_index ray tmin tmax start stop
    | Bih.Node {axis; leftclip; rightclip; left; right} ->
        (* let () =
         *   Format.eprintf "node: axis= %d, [%f,%f]\n%!" axis leftclip rightclip
         * in *)
        (* TODO : dim <- axis *)
        if proj ray.normal axis >= 0.0 then
          (* going left-to-right : first left, then right *)
          let ray_start =
            proj ray.origin axis +. (proj ray.normal axis *. tmin)
          in
          let ray_end =
            proj ray.origin axis +. (proj ray.normal axis *. tmax)
          in
          if ray_start <= leftclip then
            (* going through left subtree *)
            let far_clip =
              fmin
                ((leftclip -. proj ray.origin axis) *. proj ray.inormal axis)
                tmax
            in
            let left_hit =
              traverse raw_data tri_index ray tmin far_clip left
            in
            if leftclip <= rightclip then
              match left_hit with
              | NoHit ->
                  if rightclip <= ray_end then
                    let near_clip =
                      fmax
                        ( (rightclip -. proj ray.origin axis)
                        *. proj ray.inormal axis )
                        tmin
                    in
                    traverse raw_data tri_index ray near_clip tmax right
                  else NoHit
              | Hit _ ->
                  left_hit (* early exit *)
            else if rightclip <= ray_end then
              let near_clip =
                fmax
                  ((rightclip -. proj ray.origin axis) *. proj ray.inormal axis)
                  tmin
              in
              let right_hit =
                traverse raw_data tri_index ray near_clip tmax right
              in
              match (left_hit, right_hit) with
              | (NoHit, NoHit) ->
                  NoHit
              | (NoHit, x) | (x, NoHit) ->
                  x
              | (Hit (_, {t = t1; _}), Hit (_, {t = t2; _})) ->
                  if t1 < t2 then left_hit else right_hit
            else left_hit
          else if rightclip <= ray_end then
            let near_clip =
              fmax
                ((rightclip -. proj ray.origin axis) *. proj ray.inormal axis)
                tmin
            in
            traverse raw_data tri_index ray near_clip tmax right
          else NoHit
        else
          (* going right-to-left : first right, then left *)
          let ray_start =
            proj ray.origin axis +. (proj ray.normal axis *. tmin)
          in
          let ray_end =
            proj ray.origin axis +. (proj ray.normal axis *. tmax)
          in
          if rightclip <= ray_start then
            (* going through right subtree *)
            let far_clip =
              fmin
                ((rightclip -. proj ray.origin axis) *. proj ray.inormal axis)
                tmax
            in
            let right_hit =
              traverse raw_data tri_index ray tmin far_clip right
            in
            if leftclip < rightclip then
              match right_hit with
              | NoHit ->
                  if ray_end <= leftclip then
                    let near_clip =
                      fmax
                        ( (leftclip -. proj ray.origin axis)
                        *. proj ray.inormal axis )
                        tmin
                    in
                    traverse raw_data tri_index ray near_clip tmax left
                  else NoHit
              | Hit _ ->
                  right_hit (* early exit *)
            else if ray_end <= leftclip then
              let near_clip =
                fmax
                  ((leftclip -. proj ray.origin axis) *. proj ray.inormal axis)
                  tmin
              in
              let left_hit =
                traverse raw_data tri_index ray near_clip tmax left
              in
              match (right_hit, left_hit) with
              | (NoHit, NoHit) ->
                  NoHit
              | (NoHit, x) | (x, NoHit) ->
                  x
              | (Hit (_, {t = t1; _}), Hit (_, {t = t2; _})) ->
                  if t1 < t2 then right_hit else left_hit
            else right_hit
          else if ray_end <= leftclip then
            let near_clip =
              fmax
                ((leftclip -. proj ray.origin axis) *. proj ray.inormal axis)
                tmin
            in
            traverse raw_data tri_index ray near_clip tmax left
          else NoHit
