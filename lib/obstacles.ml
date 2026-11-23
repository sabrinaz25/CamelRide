(* AF: [Beam_at] is a beam with its bounding box's top-left at [(x, y)].
   [Warning] is an unfired missile centered on [center_y], firing in [time_left]
   seconds. [Flying] is a fired missile with top-left at [(x, y)].

   RI: [length > 0.] and [0. <= time_left <= warning_time]. *)
type t =
  | Beam_at of {
      x : float;
      y : float;
      length : float;
      orientation : World.orientation;
    }
  | Warning of {
      center_y : float;
      time_left : float;
    }
  | Flying of {
      x : float;
      y : float;
    }

type rect = float * float * float * float

type kind =
  | Beam
  | Missile_warning of { time_left : float }
  | Missile

(* Tunable constants. [tracking_speed] is below the camel's top speeds so the
   player can outrun a warning. *)
let beam_thickness = 16.
let missile_width = 60.
let missile_height = 24.
let warning_time = 1.5
let tracking_speed = 250.
let missile_speed = 400.

(* Gap between a warning sign and the right edge. *)
let warning_padding = 10.

(* How far a 45-degree beam reaches across and down. Matches [World]. *)
let diagonal_extent length = length /. Float.sqrt 2.

(* Width and height of a beam's bounding box. *)
let beam_size length : World.orientation -> float * float = function
  | Vertical -> (beam_thickness, length)
  | Horizontal -> (length, beam_thickness)
  | Diagonal_up | Diagonal_down ->
      let e = diagonal_extent length in
      (e, e)

let of_spawn : World.spawn -> t = function
  | Beam { y; length; orientation } ->
      Beam_at { x = World.screen_width; y; length; orientation }
  | Missile { y } -> Warning { center_y = y; time_left = warning_time }

(* Moves [from] toward [target] by at most [step], without overshooting. *)
let approach ~from ~target ~step =
  if from < target then Float.min target (from +. step)
  else Float.max target (from -. step)

let update o ~speed ~target_y ~dt =
  match o with
  | Beam_at b -> Beam_at { b with x = b.x -. (speed *. dt) }
  | Warning w ->
      let center_y =
        approach ~from:w.center_y ~target:target_y ~step:(tracking_speed *. dt)
      in
      let time_left = w.time_left -. dt in
      if time_left <= 0. then
        (* Fire from just off the right edge. *)
        Flying
          { x = World.screen_width; y = center_y -. (missile_height /. 2.) }
      else Warning { center_y; time_left }
  | Flying m -> Flying { m with x = m.x -. ((speed +. missile_speed) *. dt) }

let is_offscreen = function
  | Beam_at { x; length; orientation; _ } ->
      let w, _ = beam_size length orientation in
      x +. w < 0.
  | Warning _ -> false
  | Flying { x; _ } -> x +. missile_width < 0.

let update_all os ~speed ~target_y ~dt =
  os
  |> List.map (fun o -> update o ~speed ~target_y ~dt)
  |> List.filter (fun o -> not (is_offscreen o))

let kind = function
  | Beam_at _ -> Beam
  | Warning { time_left; _ } -> Missile_warning { time_left }
  | Flying _ -> Missile

let beam_ends = function
  | Beam_at { x; y; length; orientation } -> (
      let half = beam_thickness /. 2. in
      match orientation with
      | Vertical -> Some ((x +. half, y), (x +. half, y +. length))
      | Horizontal -> Some ((x, y +. half), (x +. length, y +. half))
      | Diagonal_up ->
          let e = diagonal_extent length in
          Some ((x, y +. e), (x +. e, y))
      | Diagonal_down ->
          let e = diagonal_extent length in
          Some ((x, y), (x +. e, y +. e)))
  | Warning _ | Flying _ -> None

(* Squares spaced evenly from one end to the other, at most one thickness apart
   so there are no gaps. *)
let diagonal_squares (x1, y1) (x2, y2) =
  let len = Float.hypot (x2 -. x1) (y2 -. y1) in
  let n = max 1 (int_of_float (Float.ceil (len /. beam_thickness))) in
  let half = beam_thickness /. 2. in
  List.init (n + 1) (fun i ->
      let f = float_of_int i /. float_of_int n in
      let cx = x1 +. (f *. (x2 -. x1)) in
      let cy = y1 +. (f *. (y2 -. y1)) in
      (cx -. half, cy -. half, beam_thickness, beam_thickness))

let hitbox o =
  match o with
  | Beam_at { x; y; length; orientation = (Vertical | Horizontal) as dir } ->
      let w, h = beam_size length dir in
      [ (x, y, w, h) ]
  | Beam_at { orientation = Diagonal_up | Diagonal_down; _ } -> (
      match beam_ends o with
      | Some (p1, p2) -> diagonal_squares p1 p2
      | None -> [])
  | Warning _ -> []
  | Flying { x; y } -> [ (x, y, missile_width, missile_height) ]

let missile_position = function
  | Warning { center_y; _ } ->
      Some
        ( World.screen_width -. missile_width -. warning_padding,
          center_y -. (missile_height /. 2.) )
  | Flying { x; y } -> Some (x, y)
  | Beam_at _ -> None
