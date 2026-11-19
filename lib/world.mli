(** The direction a beam points. [Diagonal_up] rises from left to right, and
    [Diagonal_down] falls from left to right. *)
type orientation =
  | Vertical
  | Horizontal
  | Diagonal_up
  | Diagonal_down

(** A new obstacle for [Obstacles] to create at the right edge of the screen. *)
type spawn =
  | Beam of {
      y : float;  (** lowest point of the beam *)
      length : float;  (** length along the beam, including diagonals *)
      orientation : orientation;
    }
  | Missile of { y : float  (** height the warning first appears at *) }

type t
(** Abstract type of world *)

val screen_width : float
(** The width of the screen *)

val screen_height : float
(** The height of the screen *)

val init : seed:int -> t
(** [init ~seed] initializes the world with a given random seed. The same seed
    will produce the same world state. *)

val advance : t -> dt:float -> t * spawn list
(** [advance world ~dt] advances the world state by [dt] seconds, ramping up the
    speed and scheduling new obstacles. Returns obstacles that should appear in
    next frame. *)

val speed : t -> float
(** [speed world] returns the current speed of the world. *)

val distance : t -> float
(** [distance world] returns the total distance traveled in the world. *)

val background_offset : t -> float
(** [background_offset world] returns the current offset of the background,
    which ranges [0, [screen_width]]. Used for parallax scrolling. *)
