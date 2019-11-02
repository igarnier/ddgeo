open Tsdl
open Tgl3
open Gg
open Utils

let _ = Sdl.init Sdl.Init.video >>= fun () -> Ok ()

let pp_opengl_info ppf () =
  let pp = Format.fprintf in
  let pp_opt ppf = function
    | None ->
        pp ppf "error"
    | Some s ->
        pp ppf "%s" s
  in
  pp ppf "@[<v>@," ;
  pp ppf "Renderer @[<v>@[%a@]@," pp_opt (Gl.get_string Gl.renderer) ;
  pp
    ppf
    "@[OpenGL %a / GLSL %a@]@]@,"
    pp_opt
    (Gl.get_string Gl.version)
    pp_opt
    (Gl.get_string Gl.shading_language_version) ;
  pp ppf "@]"

let create_window w h ~gl:(maj, min) =
  let flags = Sdl.Window.(opengl + resizable) in
  let title = Printf.sprintf "OpenGL %d.%d (core profile)" maj min in
  let set a v = Sdl.gl_set_attribute a v in
  set Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core
  >>= fun () ->
  set Sdl.Gl.context_major_version maj
  >>= fun () ->
  set Sdl.Gl.context_minor_version min
  >>= fun () ->
  set Sdl.Gl.doublebuffer 1
  >>= fun () ->
  Sdl.create_window title ~w ~h flags
  >>= fun win ->
  Sdl.gl_create_context win
  >>= fun ctx ->
  Sdl.gl_make_current win ctx
  >>= fun () ->
  Sdl.log "%a" pp_opengl_info () ;
  Ok (win, ctx)

let (win, ctx) =
  match create_window 800 600 ~gl:(3, 2) with
  | Ok res ->
      res
  | _ ->
      failwith "could not create window"

let filename =
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Please provide a wavefront file.\n%!" ;
    exit 1 )
  else Sys.argv.(1)

let camera =
  Camera.neutral
  |> Camera.set_projection_fov ~fov:90.0 ~near:0.2 ~far:200.0 ~aspect:1.33
  |> Camera.look_at
       ~eye:(V3.v 0.0 0.0 0.0)
       ~center:(V3.v 0.0 0.0 ~-.1.0)
       ~up:(V3.v 0.0 1.0 0.0)

let load_scene () =
  let raw_data = Wavefront.load ~filename ~verbose:true in
  let mesh =
    Mesh.make
      0
      (Raw_data.triangle_count raw_data - 1)
      (M4.move3 (V3.v 0.0 0.0 ~-.10.0))
      raw_data
  in
  (* let mesh = Mesh.rotate V3.oy 0.75 mesh in *)
  (* let mesh = Mesh.rotate V3.oz 0.75 mesh in *)
  (* let mesh =
   *   Mesh.pre_compose mesh (M4.scale3 (V3.v 0.0001 0.0001 0.0001)) in *)
  (* let mesh = Mesh.pre_compose mesh (fun t -> M4.rot3_axis V3.oy t) in *)
  (* let mesh' =
   *   let open Mesh in
   *   { tri_start = 0 ;
   *     tri_end   = Engine.triangle_count scene - 1 ;
   *     model     = Gg.M4.id }
   *   (\* |> Mesh.rotate Gg.V3.ox (0.5 *. acos ~-. 1.) *\)
   *   (\* |> Mesh.translate (Gg.V3.v 5.0 0.0 10.0) *\)
   *   |> Mesh.scale (Gg.V3.v 0.1 0.1 0.1)
   * in *)
  {Scene.meshes = [mesh]; raw_data; camera}

let draw pid scene win = Engine.render pid scene ; Sdl.gl_swap_window win

let reshape w h = Gl.viewport 0 0 w h

let init_engine v =
  let glsl_version gl_version =
    match gl_version with
    | (3, 2) ->
        "150"
    | (3, 3) ->
        "330"
    | (4, 0) ->
        "400"
    | (4, 1) ->
        "410"
    | (4, 2) ->
        "420"
    | (4, 3) ->
        "430"
    | (4, 4) ->
        "440"
    | _ ->
        assert false
  in
  let v = glsl_version v in
  Engine.init
    ~clear_color:V3.zero
    ~vertex_shader:(Engine.vertex_shader v)
    ~fragment_shader:(Engine.fragment_shader v)

let unres = function Ok x -> x | Error _ -> assert false

let event_loop win scene (draw : Scene.scene -> Sdl.window -> unit) =
  let e = Sdl.Event.create () in
  let key_scancode e = Sdl.Scancode.enum Sdl.Event.(get e keyboard_scancode) in
  let event e = Sdl.Event.(enum (get e typ)) in
  let window_event e = Sdl.Event.(window_event_enum (get e window_event_id)) in
  Format.eprintf "camera:\n%a\n" Camera.pp scene.Scene.camera ;
  let rec loop scene =
    ignore (Sdl.wait_event_timeout (Some e) 1) ;
    draw scene win ;
    match event e with
    | `Quit ->
        ()
    | `Mouse_button_down ->
        let xpos = Sdl.Event.(get e mouse_button_x) in
        let ypos = Sdl.Event.(get e mouse_button_y) in
        let (xwin, ywin) = Sdl.get_window_size win in
        let ray = Camera.ray_at_pixel scene.Scene.camera xwin ywin xpos ypos in
        ( match Scene.intersect_ray scene ray with
        | NoHit ->
            Format.printf "%d %d: no hit\n%!" xpos ypos
        | Hit _ ->
            Format.printf "%d %d: hit\n%!" xpos ypos ) ;
        loop scene
    | `Key_down when key_scancode e = `R ->
        let surf =
          match Sdl.get_window_surface win with Ok s -> s | _ -> assert false
        in
        unres @@ Sdl.lock_surface surf ;
        unres @@ Sdl.fill_rect surf None 0l ;
        let pitch = Sdl.get_surface_pitch surf in
        Format.eprintf "pitch: %d\n" pitch ;
        let format = Sdl.get_surface_format_enum surf in
        assert (Sdl.Pixel.eq format Sdl.Pixel.format_rgb888) ;
        Format.eprintf "format: %s\n%!" (Sdl.get_pixel_format_name format) ;
        let pixels =
          Sdl.get_surface_pixels surf Ba.(ba_kind_of_ba_scalar_type UInt8)
        in
        let set_pixel x y =
          let index = (x * 4) + (y * pitch) in
          pixels.{index} <- 255 ;
          pixels.{index + 1} <- 0 ;
          pixels.{index + 2} <- 0
        in
        let (xwin, ywin) = Sdl.get_window_size win in
        (* let c = Camera.screen_to_world camera xwin ywin xpos ypos in *)
        let () =
          for i = 0 to xwin - 1 do
            for j = 0 to ywin - 1 do
              let ray = Camera.ray_at_pixel scene.Scene.camera xwin ywin i j in
              match Scene.intersect_ray scene ray with
              | NoHit ->
                  () (* Format.printf "%d %d: no hit\n%!" i j *)
              (* Format.printf "no hit\n%!" *)
              | Hit _ ->
                  (* Format.printf "%d %d: hit\n%!" i j ; *)
                  set_pixel i j
              (* Format.printf "hit\n%!" *)
            done
          done
        in
        Sdl.unlock_surface surf ;
        unres @@ Sdl.update_window_surface win ;
        Format.eprintf "done\n%!" ;
        Unix.sleepf 5.0 ;
        loop scene
    | `Key_down ->
        let camera =
          match key_scancode e with
          | `Up ->
              Camera.translate (V3.v 0.0 0.0 ~-.0.5) scene.Scene.camera
          | `Down ->
              Camera.translate (V3.v 0.0 0.0 0.5) scene.Scene.camera
          | `Left ->
              Camera.translate (V3.v 0.5 0.0 0.0) scene.Scene.camera
          | `Right ->
              Camera.translate (V3.v ~-.0.5 0.0 0.0) scene.Scene.camera
          | _ ->
              scene.Scene.camera
        in
        loop {scene with camera}
    | `Window_event -> (
      match window_event e with
      | `Exposed | `Resized ->
          let (w, h) = Sdl.get_window_size win in
          reshape w h ; draw scene win ; loop scene
      | _ ->
          loop scene )
    | _ ->
        loop scene
  in
  draw scene win ; loop scene

let _ =
  match
    init_engine (3, 3)
    >>= fun state_engine ->
    let program = state_engine.program_id in
    let scene = load_scene () in
    Ok (event_loop win scene (draw program))
  with
  | Error (`Msg s) ->
      Format.eprintf "error: %s" s ;
      exit 1
  | Ok _ ->
      ()
