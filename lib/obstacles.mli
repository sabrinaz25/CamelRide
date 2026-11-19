(** Beams and missiles that move across the screen. [World] decides when they
    appear. This module moves them and reports their hitboxes. *)

type t
(** Abstract type of an obstacle *)

type rect = float * float * float * float
(** A rectangle [(x, y, w, h)], where [(x, y)] is its top-left corner. This is
    the same shape as [Player.hitbox]. *)

(** What an obstacle currently is, used by [Display] to choose what to draw. *)
type kind =
  | Beam
  | Missile_warning of { time_left : float }
      (** A missile that has not fired yet. [time_left] is seconds until it
          fires. *)
  | Missile  (** A missile that has fired. *)

val beam_thickness : float
(** The thickness of a beam, in pixels *)

val missile_width : float
(** The width of a missile, in pixels *)

val missile_height : float
(** The height of a missile, in pixels *)

val warning_time : float
(** How long a missile's warning lasts before it fires, in seconds *)

val tracking_speed : float
(** The fastest a missile warning can move up or down to follow the camel, in
    pixels per second *)

val missile_speed : float
(** How much faster than the world a fired missile flies, in pixels/s *)

val of_spawn : World.spawn -> t
(** [of_spawn s] creates the obstacle described by [s] just off the right edge
    of the screen. Beams can be vertical, horizontal, or diagonal. *)

val update : t -> speed:float -> target_y:float -> dt:float -> t
(** [update o ~speed ~target_y ~dt] moves [o] forward by [dt] seconds while the
    world scrolls at [speed]. Beams scroll with the world. A missile warning
    follows [target_y], the camel's height, then fires and flies left. *)

val update_all : t list -> speed:float -> target_y:float -> dt:float -> t list
(** [update_all os ~speed ~target_y ~dt] updates every obstacle in [os] and
    removes the ones that have left the screen. *)

val kind : t -> kind
(** [kind o] returns what [o] currently is. *)

val hitbox : t -> rect list
(** [hitbox o] returns the rectangles that kill the camel on contact. A diagonal
    beam is a chain of small squares along its length. A missile warning returns
    [[]], since it cannot hurt the camel. *)

val beam_ends : t -> ((float * float) * (float * float)) option
(** [beam_ends o] returns the two end points of [o] if it is a beam, so
    [Display] can draw it as one thick line. Returns [None] for missiles. *)

val missile_position : t -> (float * float) option
(** [missile_position o] returns the top-left corner of [o] if it is a missile.
    For a warning, this is where the missile will appear. Returns [None] for
    beams. *)

val is_offscreen : t -> bool
(** [is_offscreen o] returns whether [o] has fully left the left edge of the
    screen. *)
