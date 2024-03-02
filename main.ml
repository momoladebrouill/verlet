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
    temperature : float;
  }

type argument =
  {
    points : point list;
    time : int;
  }
let boilplate = 3* height/4
let (+$) (x1, y1) (x2, y2) = (x1 +. x2, y1 +. y2)
let (-$) (x1, y1) (x2, y2) = (x1 -. x2, y1 -. y2)
let ( *$ ) s (x, y) = (x *. s, y *. s)
let ( /$ ) (x, y) s = (x /. s, y /. s)
let dist (x1, y1) (x2, y2) = sqrt ((x2 -. x1)**2. +. (y2 -. y1)**2.)

let draw {points} =
  draw_rectangle 0 (boilplate) width (height - boilplate) Color.red;
  draw_circle (int_of_float (fst center)) (int_of_float (snd center)) (rayon_arenne +. rayon) Color.raywhite;
  List.iteri (fun  i {pos; color; mass;temperature} -> 
  draw_circle (int_of_float (fst pos)) (int_of_float (snd pos)) (rayon*.mass) (color_from_hsv (270.0*.(1.0-.temperature)) 1.0 mass);
  ) points

let fixpos (x,y) =
  let to_center = (x,y) -$ center in
  let dist = dist zero to_center in
  if dist > rayon_arenne then
    let to_center = (rayon_arenne /.dist) *$ to_center  in
    center +$ to_center
  else
    (x,y)

let rec fixcol a ma ta c = function
  | [] -> a,ta
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
             else zero) 
         ma 
         (if distance < (ma+.mb)*. rayon then ta*.0.8 +. t.temperature *.0.2 else ta)
         c q
  
let fixcolision points =
  List.map (fun a -> 
    let pos,temp = fixcol a.pos a.mass a.temperature a.color points in
    {
      pos = pos; 
      prevpos = a.prevpos; 
      color = a.color;
      mass = a.mass;
      temperature = temp
    }) points

let forces prev pos mass mouse temperature =
  let v = pos -$ prev in
  List.fold_left (+$) zero
    [
      grav;
      -0.01 *$ v;
      (0.0,-.temperature/.2.0);
      (pos -$ mouse) /$ (dist pos mouse)**1.5
    ]

let update {points;time} =
  let mouse = (float_of_int (get_mouse_x ()), float_of_int (get_mouse_y ())) in
  let nextpoints = List.map (fun {prevpos; pos; color; mass;temperature} -> 
    {
      prevpos = pos; 
      pos = fixpos (2.0 *$ pos -$ prevpos +$ ((dt**2.0) *$ forces prevpos pos mass mouse temperature)) ;
      color = color;
      mass = mass;
      temperature = if dist pos center > rayon_arenne  && pos |> snd |> int_of_float > boilplate then 0.2 +. temperature*.0.8 (*thermostat à 0.2*) 
      else temperature *. 0.99 (*refroidissement*)
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
      let x = float_of_int (width/2) +. Random.float 100.0 -. 50.0 in
      let y = float_of_int (height/2) +. Random.float 100.0 -. 50.0 in
      loop {
        points = {
            pos = (x,y); 
            prevpos = (x,y);
            mass = 0.2 +. Random.float 0.8; 
            color = fade (color_from_hsv (Random.float 360.) 1. 1.) 0.5;
            temperature = 0.0
         }::args.points; 
        time = args.time+1
        }
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
      temperature = 0.0
    }); time = 0};
  Printf.printf "Hello, World!\n";
