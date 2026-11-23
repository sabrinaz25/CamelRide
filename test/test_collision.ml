open OUnit2
open Camelride

(* Tests for the Collision module.

   Collision only exposes [create], [check], and [is_alive]. So every test 
   follows the same pattern which is to make a player, make some obstacles,
   call [check], and see whether the player survived. *)

(* A camel standing on the floor at x = 200. Its hitbox is the 40x40 square
   with top-left corner (200, 560). *)
let player = Player.create ~x:200. ~ceiling:60. ~floor:600.

(* [scroll o] is obstacle [o] after moving left 800 pixels, which puts an
   obstacle spawned at the right edge of the screen (x = 1000) right on top of
   the camel (x = 200). *)
let scroll o = Obstacles.update o ~speed:800. ~target_y:560. ~dt:1.0

(* A horizontal beam at the camel's height, spawned far away on the right
   edge. [scroll] moves it towards the camel. *)
let beam_at_camel_height =
  Obstacles.of_spawn
    (World.Beam { y = 560.; length = 200.; orientation = World.Horizontal })

(* A missile warning at the camel's height. It counts down 1.5 seconds before
   firing *)
let warning_at_camel_height = Obstacles.of_spawn (World.Missile { y = 560. })

let tests = "collision tests"
  >::: [
         ( "a new collision state starts alive" >:: fun _ ->
           Collision.is_alive (Collision.create ()) |> assert_equal true );
         ( "no obstacles means the camel survives" >:: fun _ ->
           let s = Collision.check ~player ~obstacles:[] (Collision.create ()) in
           assert_equal true (Collision.is_alive s) );
         ( "an obstacle far away does not hurt the camel" >:: fun _ ->
           (* the beam is still at the right edge, x = 1000 *)
           let s =
             Collision.check ~player ~obstacles:[ beam_at_camel_height ]
               (Collision.create ())
           in
           assert_equal true (Collision.is_alive s) );
         ( "a beam on top of the camel kills it" >:: fun _ ->
           let s =
             Collision.check ~player
               ~obstacles:[ scroll beam_at_camel_height ]
               (Collision.create ())
           in
           assert_equal false (Collision.is_alive s) );
         ( "a missile warning cannot hurt the camel" >:: fun _ ->
           (* even though it overlaps the camel, a warning has no hitbox *)
           let s =
             Collision.check ~player
               ~obstacles:[ scroll warning_at_camel_height ]
               (Collision.create ())
           in
           assert_equal true (Collision.is_alive s) );
         ( "once the camel is dead it stays dead" >:: fun _ ->
           let s =
             Collision.check ~player
               ~obstacles:[ scroll beam_at_camel_height ]
               (Collision.create ())
           in
           (* check again with nothing around: still dead *)
           let s = Collision.check ~player ~obstacles:[] s in
           assert_equal false (Collision.is_alive s) );
       ]

let _ = run_test_tt_main tests
