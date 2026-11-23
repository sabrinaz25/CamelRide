open OUnit2
open Camelride

(* Expected values worked out by hand: gravity 1500, thrust 3000, max_rise 450,
   max_fall 650, 40x40 camel. Starts on the floor at y = 600 - 40 = 560. Each
   frame is 0.1 s. *)

let start () = Player.create ~x:100. ~ceiling:0. ~floor:600.

(* [run n ~thrusting p] is [p] after [n] frames. *)
let rec run n ~thrusting p =
  if n = 0 then p
  else run (n - 1) ~thrusting (Player.update ~thrusting ~dt:0.1 p)

(* Check y and velocity, allowing tiny rounding errors. *)
let check ~y ~v p =
  let close a b = Float.abs (a -. b) < 1e-6 in
  assert_equal ~cmp:close ~printer:string_of_float ~msg:"y" y (Player.y p);
  assert_equal ~cmp:close ~printer:string_of_float ~msg:"v" v
    (Player.velocity p)

let tests =
  "player"
  >::: [
         ("start" >:: fun _ -> check ~y:560. ~v:0. (start ()));
         ( "no thrust stays on floor" >:: fun _ ->
           check ~y:560. ~v:0. (run 1 ~thrusting:false (start ())) );
         ( "one frame of thrust" >:: fun _ ->
           check ~y:545. ~v:(-150.) (run 1 ~thrusting:true (start ())) );
         ( "rise speed capped" >:: fun _ ->
           check ~y:155. ~v:(-450.) (run 10 ~thrusting:true (start ())) );
         ( "momentum after release" >:: fun _ ->
           check ~y:440. ~v:(-300.)
             (run 1 ~thrusting:false (run 3 ~thrusting:true (start ()))) );
         ( "stops at ceiling" >:: fun _ ->
           check ~y:0. ~v:0. (run 30 ~thrusting:true (start ())) );
         ( "fall speed capped" >:: fun _ ->
           let top = run 30 ~thrusting:true (start ()) in
           check ~y:280. ~v:650. (run 6 ~thrusting:false top) );
         ( "random input stays in bounds" >:: fun _ ->
           Random.init 42;
           let p = ref (start ()) in
           for _ = 1 to 10_000 do
             p :=
               Player.update ~thrusting:(Random.bool ()) ~dt:(Random.float 0.05)
                 !p;
             let y = Player.y !p and v = Player.velocity !p in
             assert_bool "on screen" (0. <= y && y <= 560.);
             assert_bool "speed" (-450. <= v && v <= 650.)
           done );
       ]

let () = run_test_tt_main tests
