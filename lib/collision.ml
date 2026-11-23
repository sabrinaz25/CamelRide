(* See collision.mli for what each function promises. The comments in this file
   explain how fucntion logic works. *)

(* The collision state checks if alive or dead and once dea, never comes back
   (at least in that cuurent round) *)
type t = { alive : bool }

let create () = { alive = true }

(* [overlap a b] is true if rectangles [a] and [b] cover any of the same
   pixels. A rectangle is [(x, y, w, h)] with [(x, y)] at the top-left and
   w and h as width and height of the in game objects. This checks if two boxes 
   overlap horizontally (if the left edge of each is left of the right edge of 
   the other), and the same vertically. *)
let overlap (x1, y1, w1, h1) (x2, y2, w2, h2) =
  x1 < x2 +. w2 && x2 < x1 +. w1 && y1 < y2 +. h2 && y2 < y1 +. h1

(* [hits player_box obstacle] is true if [obstacle] touches the camel. By 
   checking whether any rectangle (current missil) overlaps with camel's hitbox *)

let hits player_box obstacle =
  List.exists (fun box -> overlap player_box box) (Obstacles.hitbox obstacle)

let check ~player ~obstacles state =
  if not state.alive then state (* already dead: nothing left to check *)
  else
    let player_box = Player.hitbox player in
    let crashed = List.exists (hits player_box) obstacles in
    { alive = not crashed }

let is_alive state = state.alive
