open OUnit2
open Camelride

(* Expected values below are worked out by hand from the constants in
   obstacles.ml: beam_thickness = 16, missile 60x24, warning_time = 1.5,
   tracking_speed = 250, missile_speed = 400, and World.screen_width = 1000. *)

let eps = 1e-6
let feq a b = Float.abs (a -. b) < eps

let assert_float msg expected actual =
  assert_equal ~msg ~cmp:feq ~printer:string_of_float expected actual

let string_of_rect (x, y, w, h) = Printf.sprintf "(%g, %g, %g, %g)" x y w h

let assert_rects msg expected actual =
  assert_equal ~msg
    ~cmp:(fun a b ->
      List.length a = List.length b
      && List.for_all2
           (fun (x1, y1, w1, h1) (x2, y2, w2, h2) ->
             feq x1 x2 && feq y1 y2 && feq w1 w2 && feq h1 h2)
           a b)
    ~printer:(fun rs ->
      "[" ^ String.concat "; " (List.map string_of_rect rs) ^ "]")
    expected actual

let beam ?(y = 100.) ?(length = 200.) orientation =
  Obstacles.of_spawn (World.Beam { y; length; orientation })

let missile y = Obstacles.of_spawn (World.Missile { y })

let step ?(speed = 300.) ?(target_y = 0.) ~dt o =
  Obstacles.update o ~speed ~target_y ~dt

let is_warning o =
  match Obstacles.kind o with
  | Missile_warning _ -> true
  | _ -> false

let beam_tests =
  [
    ( "vertical beam starts at the right edge with y as its top" >:: fun _ ->
      assert_rects "hitbox"
        [ (1000., 100., 16., 200.) ]
        (Obstacles.hitbox (beam Vertical)) );
    ( "horizontal beam is length wide and thickness tall" >:: fun _ ->
      assert_rects "hitbox"
        [ (1000., 100., 200., 16.) ]
        (Obstacles.hitbox (beam Horizontal)) );
    ( "beam scrolls left at world speed and ignores target_y" >:: fun _ ->
      let b = step ~speed:300. ~target_y:500. ~dt:0.5 (beam Vertical) in
      assert_rects "hitbox" [ (850., 100., 16., 200.) ] (Obstacles.hitbox b) );
    ( "beam kind and no missile position" >:: fun _ ->
      let b = beam Horizontal in
      assert_equal Obstacles.Beam (Obstacles.kind b);
      assert_equal None (Obstacles.missile_position b) );
    ( "vertical beam ends run down its middle" >:: fun _ ->
      assert_equal
        (Some ((1008., 100.), (1008., 300.)))
        (Obstacles.beam_ends (beam Vertical)) );
    ( "diagonal up beam goes from bottom-left to top-right" >:: fun _ ->
      (* extent = 200 / sqrt 2 *)
      let e = 200. /. Float.sqrt 2. in
      match Obstacles.beam_ends (beam Diagonal_up) with
      | Some ((x1, y1), (x2, y2)) ->
          assert_float "x1" 1000. x1;
          assert_float "y1" (100. +. e) y1;
          assert_float "x2" (1000. +. e) x2;
          assert_float "y2" 100. y2
      | None -> assert_failure "expected beam ends" );
    ( "diagonal down beam goes from top-left to bottom-right" >:: fun _ ->
      let e = 200. /. Float.sqrt 2. in
      match Obstacles.beam_ends (beam Diagonal_down) with
      | Some ((x1, y1), (x2, y2)) ->
          assert_float "x1" 1000. x1;
          assert_float "y1" 100. y1;
          assert_float "x2" (1000. +. e) x2;
          assert_float "y2" (100. +. e) y2
      | None -> assert_failure "expected beam ends" );
    ( "diagonal hitbox is a gap-free chain of squares along the beam"
    >:: fun _ ->
      let b = beam Diagonal_down in
      let squares = Obstacles.hitbox b in
      let centers =
        List.map (fun (x, y, w, h) -> (x +. (w /. 2.), y +. (h /. 2.))) squares
      in
      (* 200 / 16 = 12.5, rounded up to 13 steps, so 14 squares *)
      assert_equal ~printer:string_of_int 14 (List.length squares);
      List.iter
        (fun (_, _, w, h) ->
          assert_float "square width" 16. w;
          assert_float "square height" 16. h)
        squares;
      (* Every center lies on the line y - 100 = x - 1000. *)
      List.iter
        (fun (cx, cy) ->
          assert_float "center on line" (cx -. 1000.) (cy -. 100.))
        centers;
      let first = List.hd centers and last = List.nth centers 13 in
      assert_float "first x" 1000. (fst first);
      assert_float "last x" (1000. +. (200. /. Float.sqrt 2.)) (fst last);
      let rec check_gaps = function
        | (x1, y1) :: ((x2, y2) :: _ as rest) ->
            assert_bool "neighbors at most one thickness apart"
              (Float.hypot (x2 -. x1) (y2 -. y1) <= 16. +. eps);
            check_gaps rest
        | _ -> ()
      in
      check_gaps centers );
  ]

let missile_tests =
  [
    ( "new missile is a full-length warning with no hitbox" >:: fun _ ->
      let m = missile 300. in
      assert_equal
        (Obstacles.Missile_warning { time_left = 1.5 })
        (Obstacles.kind m);
      assert_rects "hitbox" [] (Obstacles.hitbox m);
      assert_equal None (Obstacles.beam_ends m) );
    ( "warning sign sits at the right edge, centered on y" >:: fun _ ->
      (* x = 1000 - 60 - 10 padding, y = 300 - 24 / 2 *)
      assert_equal
        (Some (930., 288.))
        (Obstacles.missile_position (missile 300.)) );
    ( "warning moves toward target at tracking speed" >:: fun _ ->
      (* 250 px/s for 0.1 s = 25 px up, so the center goes 300 -> 275 *)
      let m = step ~target_y:0. ~dt:0.1 (missile 300.) in
      assert_equal (Some (930., 263.)) (Obstacles.missile_position m) );
    ( "warning stops at the target instead of overshooting" >:: fun _ ->
      let m = step ~target_y:290. ~dt:0.1 (missile 300.) in
      assert_equal (Some (930., 278.)) (Obstacles.missile_position m) );
    ( "warning counts down and stays dangerous-free" >:: fun _ ->
      let m = step ~dt:1.0 (missile 300.) in
      assert_bool "still a warning" (is_warning m);
      assert_rects "hitbox" [] (Obstacles.hitbox m);
      assert_bool "never offscreen" (not (Obstacles.is_offscreen m)) );
    ( "missile fires when warning runs out" >:: fun _ ->
      let m = step ~target_y:300. ~dt:1.5 (missile 300.) in
      assert_equal Obstacles.Missile (Obstacles.kind m);
      assert_rects "hitbox" [ (1000., 288., 60., 24.) ] (Obstacles.hitbox m) );
    ( "fired missile flies faster than the world and stops tracking" >:: fun _ ->
      let fired = step ~target_y:300. ~dt:1.5 (missile 300.) in
      (* (300 + 400) px/s for 0.5 s = 350 px left; target_y no longer matters *)
      let m = step ~speed:300. ~target_y:0. ~dt:0.5 fired in
      assert_rects "hitbox" [ (650., 288., 60., 24.) ] (Obstacles.hitbox m) );
  ]

let offscreen_tests =
  [
    ( "beam is offscreen only once fully past the left edge" >:: fun _ ->
      (* Horizontal beam is 200 wide, starts at x = 1000. *)
      let b = beam Horizontal in
      let almost = step ~speed:1000. ~dt:1.19 b in
      let gone = step ~speed:1000. ~dt:1.21 b in
      assert_bool "still visible" (not (Obstacles.is_offscreen almost));
      assert_bool "gone" (Obstacles.is_offscreen gone) );
    ( "update_all moves everything and drops offscreen obstacles in order"
    >:: fun _ ->
      let far =
        Obstacles.of_spawn
          (World.Beam { y = 50.; length = 100.; orientation = Vertical })
      in
      let wide = beam ~y:400. ~length:3000. Horizontal in
      let m = missile 300. in
      let result =
        Obstacles.update_all [ far; wide; m ] ~speed:1000. ~target_y:300.
          ~dt:1.1
      in
      (* far: 1000 - 1100 + 16 < 0, removed. wide: x = -100, still visible. *)
      assert_equal ~printer:string_of_int 2 (List.length result);
      assert_rects "wide beam first"
        [ (-100., 400., 3000., 16.) ]
        (Obstacles.hitbox (List.hd result));
      assert_bool "missile second" (is_warning (List.nth result 1)) );
  ]

let () =
  run_test_tt_main
    ("obstacles" >::: beam_tests @ missile_tests @ offscreen_tests)
