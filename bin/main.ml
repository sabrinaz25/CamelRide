open Raylib

(* Window dimensions — change these to resize the window *)
let screen_width = 1000
let screen_height = 640

(* Draw one frame. Right now it just clears the screen to a solid color.
   Add your own drawing here later. *)
let draw () =
  begin_drawing ();
  clear_background Color.white;
  end_drawing ()

let () =
  (* Open the window *)
  init_window screen_width screen_height "CamelRide";
  set_target_fps 60;
  (* raylib caps the loop at 60 fps for us *)
  Random.self_init ();

  (* Loop until the close button, Esc, or Q is pressed. window_should_close
     covers the close button and Esc *)
  while not (window_should_close () || is_key_pressed Key.Q) do
    (* frame time in seconds, handy for game logic later *)
    let dt = Float.min 0.05 (get_frame_time ()) in
    ignore dt;

    (* Input: is_key_down is true for every frame a key is held, which is what
       jetpack thrust needs. Add more key handling here later. *)
    ignore (is_key_down Key.Space);

    draw ()
  done;
  close_window ()
