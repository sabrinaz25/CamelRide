open Camelride
open Raylib

let main () =
  (* Initialize the display window. *)
  Display.init ();

  (* Initialize random seed and game state. *)
  Random.self_init ();
  let game = Game.init ~seed:(Random.bits ()) in

  (* Main game loop. *)
  let rec game_loop game =
    if not (window_should_close () || is_key_pressed Key.Q) then begin

      (* Get elapsed time since the previous frame. *)
      let dt = Float.min 0.05 (get_frame_time ()) in

      (* Read jetpack input. *)
      let thrusting = is_key_down Key.Space in

      (* Update or restart the game. *)
      let updated_game =
        match Game.status game with
        | Game.Running ->
            Game.update ~thrusting ~dt game

        | Game.Game_over ->
            if is_key_pressed Key.R then
              Game.restart ~seed:(Random.bits ()) game
            else
              game
      in

      (* Start drawing this frame. *)
      Display.clear ();

      (* Draw player. *)
      Display.draw_player (Game.player updated_game);

      (* Draw all active obstacles. *)
      List.iter
        Display.draw_obstacle
        (Game.obstacles updated_game);

      (* Draw distance and speed. *)
      let current_world = Game.world updated_game in

      Display.draw_stats
        ~distance:(World.distance current_world)
        ~speed:(World.speed current_world);

      (* Draw game-over message. *)
      (match Game.status updated_game with
       | Game.Running -> ()
       | Game.Game_over ->
           draw_text
             "GAME OVER - Press R to restart"
             250 300 25 Color.red);

      (* Finish drawing. *)
      Display.present ();

      (* Continue to the next frame. *)
      game_loop updated_game
    end
  in

  game_loop game;

  (* Close window when exiting. *)
  Display.close ()

let () = main ()
