(* AF: [{ x; y; vy; ceiling; floor }] is a camel whose hitbox has its top-left
   corner at screen position [(x, y)], moving vertically at [vy] pixels/s
   (negative is up), kept between screen y [ceiling] and screen y [floor].

   RI: [ceiling <= y <= floor -. height] and [-. max_rise <= vy <= max_fall]. *)
type t = {
  x : float;
  y : float;
  vy : float;
  ceiling : float;
  floor : float;
}

(* Tubale constants. Startinf guesses; adjust them while playtesting. *)
let gravity = 1500.
let thrust = 3000.
let max_rise = 450.
let max_fall = 650.
let width = 40.
let height = 40.

(* [clamp lo hi v] is [v] limited to the range [lo, hi]. *)
let clamp lo hi v = Float.max lo (Float.min hi v)

let create ~x ~ceiling ~floor =
  { x; y = floor -. height; vy = 0.; ceiling; floor }

let x p = p.x
let y p = p.y
let velocity p = p.vy
let on_floor p = p.y >= p.floor -. height
let at_ceiling p = p.y <= p.ceiling
let hitbox p = (p.x, p.y, width, height)

let update ~thrusting ~dt p =
  (* 1-2: gravity always applies; thrust only while the key is held *)
  let v = p.vy +. (gravity *. dt) in
  let v = if thrusting then v -. (thrust *. dt) else v in
  (* 3: speed limits *)
  let v = clamp (-.max_rise) max_fall v in
  (* 4: move *)
  let new_y = p.y +. (v *. dt) in
  (* 5: stay inside the play area and stop on contact *)
  let lowest = p.floor -. height in
  if new_y >= lowest then { p with y = lowest; vy = (if v > 0. then 0. else v) }
  else if new_y <= p.ceiling then
    { p with y = p.ceiling; vy = (if v < 0. then 0. else v) }
  else { p with y = new_y; vy = v }
