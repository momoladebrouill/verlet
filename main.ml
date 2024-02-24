open Raylib

let width = 900
let height = 900
let rk = 2
let dt = 1.0 /. (float_of_int rk)
let k = 0.75
let zero = (0., 0.)
let grav = (0., 0.1)
let center = (float_of_int width /. 2., float_of_int height /. 2.)
let rayon_arenne = 400.
let rayon = 20.

type point =
  {
    prevpos : float * float;
    pos : float * float;
    color : Color.t;
    mass : float;
  }

type argument =
  {
    points : point list;
    time : int;
  }



let (+$) (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
let (-$) (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)
let ( *$ ) s (x, y) = (x *. s, y *. s)
let dist (x1, y1) (x2, y2) = sqrt ((x2 -. x1)**2. +. (y2 -. y1)**2.)

let draw {points} =
  draw_circle (int_of_float (fst center)) (int_of_float (snd center)) (rayon_arenne +. rayon) Color.raywhite;
  List.iteri (fun  i {pos; color; mass} -> 
  draw_circle (int_of_float (fst pos)) (int_of_float (snd pos)) (rayon*.mass) color;
  ) points

let fixpos (x,y) =
  let to_center = (x,y) -$ center in
  let dist = dist zero to_center in
  if dist > rayon_arenne then
    let to_center = (rayon_arenne /.dist) *$ to_center  in
    center +$ to_center
  else
    (x,y)

let rec fixcol a ma c = function
  | [] -> a
  | t::q ->
        let b = t.pos in
        let mb = t.mass in
        let distance = dist a b in
        fixcol (a +$ 
            if distance < (ma+.mb)*.rayon && c <> t.color then 
                let v = (a -$ b) in
                let n = (1.0 /. distance) *$ v in
                let delta = k*. ((ma+.mb)*.rayon -. distance)*.ma/.(ma+.mb) in
                delta *$ n 
             else zero)  ma c q
  
let fixcolision points =
  List.map (fun a -> 
    {
      pos = fixcol a.pos a.mass a.color points; 
      prevpos = a.prevpos; 
      color = a.color;
      mass = a.mass
    }) points

let forces prev pos =
  let v = pos -$ prev in
  List.fold_left (+$) zero
    [
      grav;
      -0.01 *$ v;
    ]


let update {points;time} =
  let nextpoints = List.map (fun {prevpos; pos; color; mass} -> 
    {
      prevpos = pos; 
      pos = fixpos (2.0 *$ pos -$ prevpos +$ ((dt**2.0) *$ forces prevpos pos));
      color = color;
      mass = mass
      }
    ) points
  in {points = fixcolision nextpoints;time = time+1}

let rec loop args =
  if window_should_close () then close_window ()
  else 
  begin
    let open Color in
    begin_drawing ();
    draw_rectangle 0 0 width height (fade black 0.5);
    draw (args);
    end_drawing ();
    if is_mouse_button_down MouseButton.Left && args.time mod 3 = 0 then
      let x = get_mouse_x () in
      let y = get_mouse_y () in
      loop {points = {pos = (float_of_int x, float_of_int y); prevpos = (float_of_int x, float_of_int y);mass = 0.2 +. Random.float 0.8; color = fade (color_from_hsv (Random.float 360.) 1. 1.) 0.5}::args.points; time = args.time+1}
    else
      loop (let rec aux n args = if n = 0 then args else aux (n-1) (update args) in aux rk args)
        
  end

let () =
  init_window width height "Hello, World!";
  Random.self_init ();
  set_target_fps 60;
  loop {points = List.init 10 (fun _ ->
    let pos = center +$ (1.0 -. Random.float 2.0 , 1.0 -. Random.float 2.0) in
    {
      pos = pos;
      prevpos = pos;
      mass = 0.2+.Random.float 0.8;
      color = fade (color_from_hsv (Random.float 360.) 1. 1.) 0.5 ;
    }); time = 0};
  Printf.printf "Hello, World!\n";
