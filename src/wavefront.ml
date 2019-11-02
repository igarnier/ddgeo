open Gg
open Angstrom

type face = {
  (* vertices *)
  v1 : int;
  v2 : int;
  v3 : int;
  (* texture coords *)
  vt1 : int;
  vt2 : int;
  vt3 : int;
  (* normals *)
  vn1 : int;
  vn2 : int;
  vn3 : int;
}

type result =
  | Vertex of V3.t
  | Coords of V2.t
  | Normal of V3.t
  | Face of face
  | Quad of face * face

let vertex x y z = Vertex (V3.v x y z)

let coords x y = Coords (V2.v x y)

let normal x y z = Normal (V3.v x y z)

let face v1 v2 v3 ?(vt1 = -1) ?(vt2 = -1) ?(vt3 = -1) ?(vn1 = -1) ?(vn2 = -1)
    ?(vn3 = -1) () =
  {v1; v2; v3; vt1; vt2; vt3; vn1; vn2; vn3}

(* this combinator is a bit lax *)
let float =
  take_while1 (function '0' .. '9' | '-' | '.' -> true | _ -> false)
  >>| float_of_string

let int =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

let spaces = skip_many (char ' ')

let three_floats f =
  sep_by spaces float
  >>= function
  | [x; y; z] -> return (Some (f x y z)) | _ -> fail "three_floats: error"

let vertex = char 'v' >>= fun _ -> spaces >>= fun _ -> three_floats vertex

let vertex_normal =
  string "vn" >>= fun _ -> spaces >>= fun _ -> three_floats normal

let texture_coord =
  string "vt"
  >>= fun _ ->
  spaces
  >>= fun _ ->
  sep_by spaces float
  >>= function
  | [vx; vy] | [vx; vy; 0.0] ->
      return (Some (coords vx vy))
  | _ ->
      fail "texture coord: error"

let triangle =
  let int_slash_int =
    int >>= fun i1 -> char '/' >>= fun _ -> int >>= fun i2 -> return (i1, i2)
  in
  let int_slash_slash_int =
    int
    >>= fun i1 ->
    char '/'
    >>= fun _ -> char '/' >>= fun _ -> int >>= fun i2 -> return (i1, i2)
  in
  let int_slash_int_slash_int =
    int
    >>= fun i1 ->
    char '/'
    >>= fun _ ->
    int
    >>= fun i2 -> char '/' >>= fun _ -> int >>= fun i3 -> return (i1, i2, i3)
  in
  char 'f'
  >>= fun _ ->
  spaces
  >>= fun _ ->
  choice
    [
      ( sep_by spaces int_slash_int_slash_int
      >>= function
      | [(v1, vt1, vn1); (v2, vt2, vn2); (v3, vt3, vn3)] ->
          return (Some (Face (face v1 v2 v3 ~vt1 ~vt2 ~vt3 ~vn1 ~vn2 ~vn3 ())))
      | _ ->
          fail "face: error" );
      ( sep_by spaces int_slash_slash_int
      >>= function
      | [(v1, vn1); (v2, vn2); (v3, vn3)] ->
          return (Some (Face (face v1 v2 v3 ~vn1 ~vn2 ~vn3 ())))
      | _ ->
          fail "face: error" );
      ( sep_by spaces int_slash_int
      >>= function
      | [(v1, vt1); (v2, vt2); (v3, vt3)] ->
          (*| [ (vt1, v1); (vt2, v2); (vt3, v3) ] ->*)
          return (Some (Face (face v1 v2 v3 ~vt1 ~vt2 ~vt3 ())))
      | _ ->
          fail "face: error" );
      ( sep_by spaces int_slash_int
      >>= function
      | [(v1, vt1); (v2, vt2); (v3, vt3); (v4, vt4)] ->
          let face1 = face v1 v2 v3 ~vt1 ~vt2 ~vt3 () in
          let face2 = face v1 v3 v4 ~vt1 ~vt2:vt3 ~vt3:vt4 () in
          return (Some (Quad (face1, face2)))
      | _ ->
          fail "face: error" );
      ( sep_by spaces int
      >>= function
      | [v1; v2; v3; v4] ->
          let face1 = face v1 v2 v3 () in
          let face2 = face v1 v3 v4 () in
          return (Some (Quad (face1, face2)))
      | _ ->
          fail "face: error" );
      ( sep_by spaces int
      >>= function
      | [x; y; z] ->
          return (Some (Face (face x y z ())))
      | _ ->
          fail "face: error" );
    ]

let comment = char '#' >>= fun _ -> return None

let mtllib = string "mtllib" >>= fun _ -> return None

let usemtl = string "usemtl" >>= fun _ -> return None

let group = char 'g' >>= fun _ -> return None

let smooth_shading = char 's' >>= fun _ -> return None

let wavefront_line =
  choice
    [
      vertex;
      vertex_normal;
      texture_coord;
      triangle;
      comment;
      mtllib;
      usemtl;
      group;
      smooth_shading;
    ]

let bbox vertices = List.fold_left Box3.add_pt Box3.empty vertices

let centroid (vertices : v3 list) =
  let len = List.length vertices in
  let ilen = 1.0 /. float_of_int len in
  List.fold_left (fun acc vx -> V3.(acc + smul ilen vx)) V3.zero vertices

let recenter (vertices : v3 list) =
  let c = centroid vertices in
  List.map (fun v -> V3.sub v c) vertices

let compute_normals (vertices : v3 list) =
  let c = centroid vertices in
  List.map (fun vx -> V3.(unit (vx - c))) vertices

let load ~filename ~verbose =
  let fd = open_in filename in
  let line_num = ref 0 in
  let rec loop vertices coords normals triangles =
    match input_line fd with
    | line -> (
        let _ = incr line_num in
        match parse_string wavefront_line line with
        | Result.Error _ | Result.Ok None ->
            loop vertices coords normals triangles
        | Result.Ok (Some (Vertex v)) ->
            loop (v :: vertices) coords normals triangles
        | Result.Ok (Some (Coords c)) ->
            loop vertices (c :: coords) normals triangles
        | Result.Ok (Some (Normal n)) ->
            loop vertices coords (n :: normals) triangles
        | Result.Ok (Some (Face tri)) ->
            loop vertices coords normals (tri :: triangles)
        | Result.Ok (Some (Quad (tri1, tri2))) ->
            loop vertices coords normals (tri2 :: tri1 :: triangles)
        | exception Failure msg ->
            Printf.eprintf
              "error at line %d: %s\nline: %s\n"
              !line_num
              msg
              line ;
            exit 1 )
    | exception End_of_file ->
        close_in fd ;
        ( List.rev vertices,
          List.rev coords,
          List.rev normals,
          List.rev triangles )
  in
  let (vertices, coords, normals, triangles) = loop [] [] [] [] in
  let vertices = recenter vertices in
  let vnum = List.length vertices
  and cnum = List.length coords
  and tnum = List.length triangles in
  let nnum = vnum in
  let normals =
    match normals with
    | [] ->
        compute_normals vertices
    | _ ->
        assert (List.length normals = vnum) ;
        normals
  in
  let vbuffer = Ba.create Float32 (vnum * 3) in
  let cbuffer = Ba.create Float32 (cnum * 2) in
  let nbuffer = Ba.create Float32 (nnum * 3) in
  let tbuffer = Ba.create Int32 (tnum * 3) in
  let tnbuffer = Ba.create Float32 (tnum * 3) in
  List.iteri (fun i v -> Ba.set_v3 vbuffer (i * 3) v) vertices ;
  List.iteri (fun i v -> Ba.set_v2 cbuffer (i * 2) v) coords ;
  List.iteri (fun i v -> Ba.set_v3 nbuffer (i * 3) v) normals ;
  List.iteri
    (fun i {v1; v2; v3; _} ->
      let i1 = v1 - 1 in
      let i2 = v2 - 1 in
      let i3 = v3 - 1 in
      Ba.seti_3d tbuffer (i * 3) i1 i2 i3 ;
      let v1 = Ba.get_v3 vbuffer (i1 * 3) in
      let v2 = Ba.get_v3 vbuffer (i2 * 3) in
      let v3 = Ba.get_v3 vbuffer (i3 * 3) in
      let d1 = V3.(v2 - v1) in
      let d2 = V3.(v3 - v1) in
      let tnormal = V3.unit (V3.cross d1 d2) in
      (* let tnormal =
       *   V3.unit
       *     (V3.add
       *        (Ba.get_v3 nbuffer (i1 * 3))
       *        (V3.add (Ba.get_v3 nbuffer (i2 * 3)) (Ba.get_v3 nbuffer (i3 * 3))))
       * in *)
      Format.eprintf
        "normal: for %a %a %a = %a\n%!"
        V3.pp
        v1
        V3.pp
        v2
        V3.pp
        v3
        V3.pp
        tnormal ;
      Ba.set_v3 tnbuffer (i * 3) tnormal)
    triangles ;
  if verbose then (
    let box = bbox vertices in
    Format.eprintf "loaded %s\n" filename ;
    Format.eprintf "vertices: %d\n" vnum ;
    Format.eprintf "txcoords: %d\n" cnum ;
    Format.eprintf "normals:  %d\n" nnum ;
    Format.eprintf "tris:     %d\n%!" tnum ;
    Format.eprintf "bbox:     %a\n%!" Box3.pp box ;
    Format.eprintf "centroid: %a\n%!" V3.pp (centroid vertices) ) ;
  Raw_data.make
    ~vertices:vbuffer
    ~indices:tbuffer
    ~colors:None
    ~normals:nbuffer
    ~tnormals:tnbuffer
    ~texcoords:(Some cbuffer)
