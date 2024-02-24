open Raylib

let width = 800
let height = 800
let rk = 5
let dt = 1.0 /. (float_of_int rk)

type point =
  {
    prevpos : float * float;
    pos : float * float;
    color : Color.t;
  }

type argument =
  {
    points : point list;
  }



let (+$) (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
let (-$) (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)
let ( *$ ) s (x, y) = (x *. s, y *. s)
let dist (x1, y1) (x2, y2) = sqrt ((x2 -. x1)**2. +. (y2 -. y1)**2.)
let zero = (0., 0.)

let grav = (0., 0.1)
let center = (float_of_int width /. 2., float_of_int height /. 2.)
let rayon_arenne = 200.
let rayon = 20.

let draw {points} =
  draw_circle (int_of_float (fst center)) (int_of_float (snd center)) (rayon_arenne +. rayon) Color.raywhite;
  List.iteri (fun  i {pos; color} -> 
  draw_circle (int_of_float (fst pos)) (int_of_float (snd pos)) rayon color;
  ) points

let fixpos (x,y) =
  let to_center = (x,y) -$ center in
  let dist = dist zero to_center in
  if dist > rayon_arenne then
    let to_center = (rayon_arenne /.dist) *$ to_center  in
    center +$ to_center
  else
    (x,y)

let rec fixcol a c = function
  | [] -> a
  | t::q ->
        let b = t.pos in
        let distance = dist a b in
        fixcol (a +$ 
            if distance < 2.0*.rayon && c <> t.color then 
                let v = (a -$ b) in
                let n = (1.0 /. distance) *$ v in
                let delta = (2.0*.rayon -. distance)/.2.0 in
                delta *$ n 
             else zero) c q
  

let fixcolision points =
  List.map (fun a -> 
    {
      pos = fixcol a.pos a.color points; 
      prevpos = a.prevpos; 
      color = a.color}
    ) points

let forces prev pos =
  let v = pos -$ prev in
  List.fold_left (+$) zero
    [
      grav;
      -0.01 *$ v;
    ]


let update {points} =
  let nextpoints = List.map (fun {prevpos; pos; color} -> 
    {
      prevpos = pos; 
      pos = fixpos (2.0 *$ pos -$ prevpos +$ ((dt**2.0) *$ forces prevpos pos));
      color = color
      }
    ) points
  in {points = fixcolision nextpoints}

let rec loop args =
  if window_should_close () then close_window ()
  else 
  begin
    let open Color in
    begin_drawing ();
    draw_rectangle 0 0 width height (fade black 0.5);
    draw (args);
    end_drawing ();
    if is_mouse_button_pressed MouseButton.Left then
      let x = get_mouse_x () in
      let y = get_mouse_y () in
      loop {points = {pos = (float_of_int x, float_of_int y); prevpos = (float_of_int x, float_of_int y); color = fade (color_from_hsv (Random.float 360.) 1. 1.) 0.7}::args.points}
    else
      loop (let rec aux n args = if n = 0 then args else aux (n-1) (update args) in aux rk args)
        
  end

let () =
  init_window width height "Hello, World!";
  Random.self_init ();
  set_target_fps 60;
  loop {points = List.init 10 (fun _ ->
    let pos = (400. -. Random.float 1., 400. -. Random.float 1.) in
    {
      pos = pos;
      prevpos = pos;
      color = fade (color_from_hsv (Random.float 360.) 1. 1.) 0.7 ;
    })};
  Printf.printf "Hello, World!\n";
