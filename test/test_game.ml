open OUnit2
open Camelride

let tests = "game tests"
  >::: [
    ( "game initializes as running" >:: fun _ ->
      let game = Game.init ~seed:42 in
      assert_equal Game.Running (Game.status game)
    );

    ( "game initializes with a player" >:: fun _ ->
      let game = Game.init ~seed:42 in
      let player = Game.player game in
      assert_equal 100. (Player.x player)
    );

    ( "game initializes without obstacles" >:: fun _ ->
      let game = Game.init ~seed:42 in
      assert_equal [] (Game.obstacles game)
    );

    ( "game initializes with an alive collision state" >:: fun _ ->
      let game = Game.init ~seed:42 in
      assert_equal true (Collision.is_alive (Game.collision game))
    );

    ( "player moves when the game updates" >:: fun _ ->
      let game = Game.init ~seed:42 in
      let old_y = Player.y (Game.player game) in
      let updated_game = Game.update ~thrusting:true ~dt:0.1 game in
      let new_y = Player.y (Game.player updated_game) in
      assert_bool "Player should move upward when thrusting"
        (new_y < old_y)
    );

    ( "world advances when the game updates" >:: fun _ ->
      let game = Game.init ~seed:42 in
      let old_distance = World.distance (Game.world game) in
      let updated_game = Game.update ~thrusting:false ~dt:0.1 game in
      let new_distance = World.distance (Game.world updated_game) in
      assert_bool "World distance should increase"
        (new_distance > old_distance)
    );

    ( "restart returns game to running state" >:: fun _ ->
      let game = Game.init ~seed:42 in
      let restarted_game = Game.restart ~seed:99 game in
      assert_equal Game.Running (Game.status restarted_game);
      assert_equal [] (Game.obstacles restarted_game)
    );
  ]

let _ = run_test_tt_main tests
