open Gg

type t = {
  view : M4.t;
  projection : M4.t;
  eye : V3.t;
  towards : V3.t;
  up : V3.t;
}

let pp fmtr {eye; towards; up; _} =
  Format.fprintf
    fmtr
    "eye=%a towards=%a up=%a"
    V3.pp
    eye
    V3.pp
    towards
    V3.pp
    up

let pi = acos ~-.1.

let neutral =
  {
    view = M4.id;
    projection = M4.id;
    eye = V3.zero;
    towards = V3.ox;
    up = V3.oy;
  }

let view {view; _} = view

let projection {projection; _} = projection

let set_projection_persp ~left ~right ~bot ~top ~near ~far camera =
  let projection = M4.persp ~left ~right ~bot ~top ~near ~far in
  {camera with projection}

(* let set_projection_fov ~fov ~near ~far ~aspect camera =
 *   let top = near *. (tan (pi /. 180.0 *. 0.5 *. fov)) in
 *   let bot = -. top in
 *   let right = top *. aspect in
 *   let left = -. right in
 *   set_projection_persp ~left ~right ~bot ~top ~near ~far camera *)

let deg_to_rad deg = deg *. pi /. 180.0

let set_projection_fov ~fov ~near ~far ~aspect camera =
  let f = 1.0 /. tan (deg_to_rad fov *. 0.5) in
  let m00 = f /. aspect in
  let m11 = f in
  let m22 = (far +. near) /. (near -. far) in
  let m23 = 2. *. far *. near /. (near -. far) in
  let m32 = ~-.1.0 in
  {
    camera with
    projection =
      M4.v m00 0.0 0.0 0.0 0.0 m11 0.0 0.0 0.0 0.0 m22 m23 0.0 0.0 m32 0.0;
  }

let _look_at (eye : P3.t) (towards : P3.t) (up : V3.t) =
  let delta = V3.unit towards in
  let s = V3.cross delta (V3.unit up) in
  let u = V3.cross (V3.unit s) delta in
  let look_at_mat =
    M4.of_rows
      (V4.of_v3 s ~w:0.0)
      (V4.of_v3 u ~w:0.0)
      (V4.neg (V4.of_v3 delta ~w:0.0))
      (V4.of_v3 V3.zero ~w:1.0)
  in
  M4.mul look_at_mat (M4.move3 (V3.neg eye))

let look_at ~(eye : P3.t) ~(center : P3.t) ~(up : V3.t) camera =
  let towards = V3.sub center eye in
  let view = _look_at eye towards up in
  {camera with view; eye; towards; up}

let translate (delta : V3.t) camera =
  let eye = V3.add camera.eye delta in
  let view = _look_at eye camera.towards camera.up in
  {camera with view; eye}

let world_to_screen camera =
  let proj = camera.projection in
  let view = camera.view in
  M4.mul proj view

let ndc xwin ywin xpos ypos =
  let xpos = float xpos in
  let ypos = float ypos in
  let xwin = float xwin in
  let ywin = float ywin in
  let xratio = xpos /. xwin in
  (* in [0,1]*)
  let yratio = ypos /. ywin in
  (* in [0,1]*)
  let xndc = (xratio *. 2.0) -. 1. in
  let yndc = ~-.((yratio *. 2.0) -. 1.) in
  (xndc, yndc)

let screen_to_world camera xwin ywin xpos ypos =
  (* let w2s = Camera.world_to_screen camera in
   * let p = V3.tr w2s (V3.v 1.0 1.0 ~-.1.0) in *)
  let (x, y) = ndc xwin ywin xpos ypos in
  (* let vec = V3.v x y 0.0 in *)
  let mat = M4.inv (world_to_screen camera) in
  V4.ltr mat (V4.v x y 0.0 1.0)

(* let v =
 * Format.eprintf "test: %a\n" V4.pp (V4.ltr mat (V4.v x y 0.0 1.0)) ;
 * V3.tr mat vec *)

let ray_at_pixel camera xwin ywin xpos ypos =
  let pixel = screen_to_world camera xwin ywin xpos ypos in
  let pixel = V4.homogene pixel in
  let origin = camera.eye in
  let normal = V3.unit (V3.sub (V3.of_v4 pixel) origin) in
  let inormal =
    V3.v (1. /. V3.x normal) (1. /. V3.y normal) (1. /. V3.z normal)
  in
  {Traverse.origin; normal; inormal}
