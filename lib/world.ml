type orientation =
  | Vertical
  | Horizontal
  | Diagonal_up
  | Diagonal_down

type spawn =
  | Beam of {
      y : float;
      length : float;
      orientation : orientation;
    }
  | Missile of { y : float }

(* An obstacle that has been generated but not yet released. [at] is the
   distance at which [spawn] appears at the right edge of the screen. *)
type planned = {
  at : float;
  spawn : spawn;
}

(* AF: [{ distance; speed; generated_until; upcoming; rng }] is a run that has
   scrolled [distance] pixels at [speed] pixels/s. The level has been generated
   up to [generated_until], and [upcoming] holds the obstacles generated but not
   yet on screen. [rng] produces the rest of the level.

   RI: [distance >= 0.], [base_speed <= speed <= max_speed], [upcoming] is
   sorted by [at], and every [at] in [upcoming] is [> distance]. *)
type t = {
  distance : float;
  speed : float;
  generated_until : float;
  upcoming : planned list;
  rng : Random.State.t;
}

let screen_width = 1000.
let screen_height = 640.

(* Tunable constants. Starting guesses; adjust them while playtesting. *)
let base_speed = 300.
let max_speed = 900.
let ramp = 0.02 (* speed gained per pixel traveled *)
let first_spawn = 800.
let lookahead = 2. *. screen_width
let margin = 40. (* keep obstacles off the top and bottom edges *)
let zigzag_after = 1500.
let slant_after = 2000.
let corridor_after = 3000.
let missiles_after = 3000.
let speed_at d = Float.min max_speed (base_speed +. (ramp *. d))

(* [difficulty speed] is 0. at the start of a run and 1. at max speed. *)
let difficulty speed = (speed -. base_speed) /. (max_speed -. base_speed)

(* How far a 45-degree beam of [length] reaches across and down. *)
let diagonal_extent length = length /. Float.sqrt 2.

(* Screen height a beam covers. Its [y] is the top of that space. *)
let beam_height length = function
  | Vertical -> length
  | Horizontal -> 0.
  | Diagonal_up | Diagonal_down -> diagonal_extent length

(* Screen width a beam covers. *)
let beam_width length = function
  | Vertical -> 0.
  | Horizontal -> length
  | Diagonal_up | Diagonal_down -> diagonal_extent length

let init ~seed =
  {
    distance = 0.;
    speed = base_speed;
    generated_until = first_spawn;
    upcoming = [];
    rng = Random.State.make [| seed |];
  }

let rand rng lo hi = lo +. Random.State.float rng (hi -. lo)

(* [random_beam rng ~length orientation] is a beam at a random height that fits
   between the top and bottom margins. *)
let random_beam rng ~length orientation =
  let y =
    rand rng margin (screen_height -. margin -. beam_height length orientation)
  in
  Beam { y; length; orientation }

(* Patterns. Each returns [(pieces, length)]: [pieces] are [(offset, spawn)]
   pairs placed relative to the start of the chunk, and [length] is how much
   distance the chunk takes up. [sec s] converts [s] seconds of flight at the
   current speed into pixels, so gaps shrink in reaction time as the run speeds
   up but never become impossible. *)

(* One straight beam at a random height. *)
let single rng ~sec ~d =
  let orientation = if Random.State.bool rng then Vertical else Horizontal in
  let length = rand rng 120. 280. in
  ( [ (0., random_beam rng ~length orientation) ],
    beam_width length orientation
    +. sec (rand rng (1.6 -. (0.7 *. d)) (2.1 -. (0.7 *. d))) )

(* Vertical beams alternating ceiling and floor. They overlap in height, so the
   player has to move up and down between them. *)
let zigzag rng ~sec ~d =
  let n = 3 + Random.State.int rng 3 in
  let length = 300. in
  let spacing = sec (1.1 -. (0.3 *. d)) in
  let pieces =
    List.init n (fun i ->
        let y =
          if i mod 2 = 0 then margin else screen_height -. margin -. length
        in
        (float_of_int i *. spacing, Beam { y; length; orientation = Vertical }))
  in
  (pieces, (float_of_int n *. spacing) +. sec 0.8)

(* Diagonal beams alternating up and down slopes. *)
let slant rng ~sec ~d =
  let n = 2 + Random.State.int rng 2 in
  let length = 260. in
  let spacing = diagonal_extent length +. sec (1.0 -. (0.3 *. d)) in
  let pieces =
    List.init n (fun i ->
        let orientation = if i mod 2 = 0 then Diagonal_up else Diagonal_down in
        (float_of_int i *. spacing, random_beam rng ~length orientation))
  in
  (pieces, (float_of_int n *. spacing) +. sec 0.6)

(* Two horizontal beams forming a channel that narrows with difficulty but stays
   much taller than the camel. *)
let corridor rng ~sec ~d =
  let gap = 220. -. (60. *. d) in
  let center =
    rand rng (margin +. (gap /. 2.)) (screen_height -. margin -. (gap /. 2.))
  in
  let length = sec 1.2 in
  ( [
      (0., Beam { y = center -. (gap /. 2.); length; orientation = Horizontal });
      (0., Beam { y = center +. (gap /. 2.); length; orientation = Horizontal });
    ],
    length +. sec 0.9 )

(* Two or three missiles in a row, three only when the run is fast. *)
let missile_wave rng ~sec ~d =
  let n = 2 + Random.State.int rng (if d > 0.5 then 2 else 1) in
  let spacing = sec 0.6 in
  let pieces =
    List.init n (fun i ->
        ( float_of_int i *. spacing,
          Missile { y = rand rng margin (screen_height -. margin) } ))
  in
  (pieces, (float_of_int n *. spacing) +. sec 1.0)

(* An empty stretch so the player can recover. *)
let breather _ ~sec ~d:_ = ([], sec 0.8)

(* [pick_pattern rng ~distance ~speed] picks a pattern with weights that shift
   toward harder ones as the run goes on, and generates it. [distance] decides
   which patterns are unlocked yet. *)
let pick_pattern rng ~distance ~speed =
  let d = difficulty speed in
  let sec s = s *. speed in
  let unlocked after w = if distance > after then w else 0. in
  let options =
    [
      (4. -. (2. *. d), single);
      (1., breather);
      (unlocked zigzag_after (1. +. (2. *. d)), zigzag);
      (unlocked slant_after (1. +. (1.5 *. d)), slant);
      (unlocked corridor_after (0.5 +. (2. *. d)), corridor);
      (unlocked missiles_after (1. +. (2. *. d)), missile_wave);
    ]
  in
  let total = List.fold_left (fun acc (w, _) -> acc +. w) 0. options in
  let rec choose roll = function
    | [] -> single
    | [ (_, p) ] -> p
    | (w, p) :: others -> if roll < w then p else choose (roll -. w) others
  in
  let pattern = choose (Random.State.float rng total) options in
  pattern rng ~sec ~d

(* [fill w] generates chunks until the level reaches [lookahead] pixels past the
   player. *)
let rec fill w =
  if w.generated_until >= w.distance +. lookahead then w
  else
    let pieces, length =
      pick_pattern w.rng ~distance:w.generated_until ~speed:w.speed
    in
    let planned =
      pieces
      |> List.map (fun (dx, spawn) -> { at = w.generated_until +. dx; spawn })
      |> List.sort (fun a b -> Float.compare a.at b.at)
    in
    fill
      {
        w with
        upcoming = w.upcoming @ planned;
        generated_until = w.generated_until +. length;
      }

let advance w ~dt =
  let distance = w.distance +. (w.speed *. dt) in
  let w =
    fill
      {
        w with
        distance;
        speed = speed_at distance;
        rng = Random.State.copy w.rng;
      }
  in
  let ready, later = List.partition (fun p -> p.at <= distance) w.upcoming in
  ({ w with upcoming = later }, List.map (fun p -> p.spawn) ready)

let speed w = w.speed
let distance w = w.distance
let background_offset w = Float.rem (w.distance *. 0.5) screen_width
