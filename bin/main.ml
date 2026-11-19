open Graphics

(* Window dimensions — change these to resize the window *)
let screen_width = 1000
let screen_height = 640

(* Draw one frame. Right now it just clears the screen to a solid color.
   Add your own drawing here later. *)
let draw () =
  set_color (rgb 255 255 255);
  fill_rect 0 0 screen_width screen_height;
  synchronize ()

let () =
  (* Open the window *)
  open_graph (Printf.sprintf " %dx%d" screen_width screen_height);
  set_window_title "CamelRide";
  auto_synchronize false; (* we call synchronize () ourselves each frame *)
  Random.self_init ();

  let running = ref true in
  let last_frame = ref (Unix.gettimeofday ()) in
  try
    while !running do
      let now = Unix.gettimeofday () in
      let dt = min 0.05 (now -. !last_frame) in
      last_frame := now;
      ignore dt; (* frame time in seconds, handy for game logic later *)

      (* Input: press Q to quit. Add more key handling here later. *)
      if key_pressed () then (
        let key = read_key () in
        if key = 'q' || key = 'Q' then running := false);

      draw ();

      (* Cap the loop at roughly 60 fps *)
      Unix.sleepf (max 0. (0.016 -. (Unix.gettimeofday () -. now)))
    done;
    close_graph ()
  with
  | Graphic_failure message when message = "graphic screen not opened" ->
      prerr_endline "The window was closed; exiting."
