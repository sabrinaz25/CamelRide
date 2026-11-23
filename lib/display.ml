(* display.ml is explained by everything in display.mli, this is the code execution*)

open Raylib

let init () =
  init_window
    (int_of_float World.screen_width)
    (int_of_float World.screen_height)
    "CamelRide";
  set_target_fps 60

let clear () =
  begin_drawing ();
  clear_background Color.white

let draw_player p =
  let x, y, w, h = Player.hitbox p in
  draw_rectangle
    (int_of_float x) (int_of_float y)
    (int_of_float w) (int_of_float h)
    Color.black

let draw_obstacle o =
  match Obstacles.kind o with
  | Obstacles.Beam -> (
      (* One thick black line between the beam's two ends. *)
      match Obstacles.beam_ends o with
      | Some ((x1, y1), (x2, y2)) ->
          draw_line_ex
            (Vector2.create x1 y1)
            (Vector2.create x2 y2)
            Obstacles.beam_thickness Color.black
      | None -> ())
  | Obstacles.Missile -> (
      (* Fired missile: solid black rectangle. *)
      match Obstacles.missile_position o with
      | Some (x, y) ->
          draw_rectangle
            (int_of_float x) (int_of_float y)
            (int_of_float Obstacles.missile_width)
            (int_of_float Obstacles.missile_height)
            Color.black
      | None -> ())
  | Obstacles.Missile_warning _ -> (
      (* Warning: black outline only, so it looks harmless. *)
      match Obstacles.missile_position o with
      | Some (x, y) ->
          draw_rectangle_lines
            (int_of_float x) (int_of_float y)
            (int_of_float Obstacles.missile_width)
            (int_of_float Obstacles.missile_height)
            Color.black
      | None -> ())

let draw_stats ~distance ~speed =
  draw_text (Printf.sprintf "DISTANCE %05.0f" distance) 20 20 24 Color.black;
  draw_text (Printf.sprintf "SPEED %03.0f" speed) 20 50 24 Color.black

let present () = end_drawing ()
let close () = close_window ()
