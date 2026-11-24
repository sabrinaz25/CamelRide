open OUnit2
open Camelride

(* [run w n dt] advances [w] by [n] frames of [dt] seconds, returning the final
   world and every spawn paired with the distance at which it appeared. *)
let run w n dt =
  let rec go w n acc =
    if n = 0 then (w, List.rev acc)
    else
      let w', spawns = World.advance w ~dt in
      let acc = List.map (fun s -> (World.distance w', s)) spawns @ acc in
      go w' (n - 1) acc
  in
  go w n []

(* Screen height a beam covers, matching the doc in world.mli *)
let beam_height length = function
  | World.Vertical -> length
  | World.Horizontal -> 0.
  | World.Diagonal_up | World.Diagonal_down -> length /. Float.sqrt 2.

let seeds = [ 1; 2; 3; 4; 5 ]

let world_tests =
  "World"
  >::: [
         ( "starts at zero distance" >:: fun _ ->
           assert_equal 0. (World.distance (World.init ~seed:1)) );
         ( "speed never decreases and is capped" >:: fun _ ->
           let rec check w n =
             if n > 0 then (
               let w', _ = World.advance w ~dt:0.016 in
               assert_bool "decreased" (World.speed w' >= World.speed w);
               assert_bool "too fast" (World.speed w' <= 900.);
               check w' (n - 1))
           in
           check (World.init ~seed:1) 10_000 );
         ( "same seed gives the same run" >:: fun _ ->
           let _, a = run (World.init ~seed:42) 3000 0.016 in
           let _, b = run (World.init ~seed:42) 3000 0.016 in
           assert_equal a b );
         ( "obstacles appear in order, after the start" >:: fun _ ->
           let _, spawns = run (World.init ~seed:7) 10_000 0.016 in
           let ds = List.map fst spawns in
           assert_bool "nothing spawned" (List.length ds > 5);
           assert_bool "too early" (List.hd ds >= 800.);
           let rec sorted = function
             | a :: (b :: _ as rest) -> a <= b && sorted rest
             | _ -> true
           in
           assert_bool "out of order" (sorted ds) );
         ( "no missiles before they unlock" >:: fun _ ->
           List.iter
             (fun seed ->
               let _, spawns = run (World.init ~seed) 10_000 0.016 in
               List.iter
                 (fun (d, s) ->
                   match s with
                   | World.Missile _ -> assert_bool "early missile" (d > 3000.)
                   | World.Beam _ -> ())
                 spawns)
             seeds );
         ( "every orientation appears in a long run" >:: fun _ ->
           let _, spawns = run (World.init ~seed:3) 20_000 0.016 in
           let has o =
             List.exists
               (fun (_, s) ->
                 match s with
                 | World.Beam { orientation; _ } -> orientation = o
                 | World.Missile _ -> false)
               spawns
           in
           List.iter
             (fun o -> assert_bool "orientation missing" (has o))
             World.[ Vertical; Horizontal; Diagonal_up; Diagonal_down ] );
         ( "obstacles stay on screen vertically" >:: fun _ ->
           List.iter
             (fun seed ->
               let _, spawns = run (World.init ~seed) 10_000 0.016 in
               List.iter
                 (fun (_, s) ->
                   match s with
                   | World.Beam { y; length; orientation } ->
                       assert_bool "above top" (y >= 0.);
                       assert_bool "below bottom"
                         (y +. beam_height length orientation
                         <= World.screen_height)
                   | World.Missile { y } ->
                       assert_bool "missile off screen"
                         (y >= 0. && y <= World.screen_height))
                 spawns)
             seeds );
         ( "background offset stays in range" >:: fun _ ->
           let w, _ = run (World.init ~seed:9) 5000 0.016 in
           let o = World.background_offset w in
           assert_bool "out of range" (o >= 0. && o < World.screen_width) );
       ]

let tests = "World tests" >::: [ world_tests ]
let _ = run_test_tt_main tests
