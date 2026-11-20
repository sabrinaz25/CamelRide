(** Each frame, gravity pulls the camel down and, while the player holds the
    thrust key, the jetpack pushes the camel up. The camel keeps its momentum
    between frames and cannot leave the play area.

    Coordinates follow raylib's screen convention: [(0, 0)] is the top-left
    corner of the window and y increases downward. So a positive velocity means
    moving down and a negative velocity means moving up. The camel's position
    [(x, y)] is the top-left corner of its hitbox.

    Units: positions are in pixels, time in seconds, velocities in pixels per
    second, and accelerations in pixels per second squared. *)

type t
(** The state of the camel in one frame, its position, its vertical velocity,
    and the floor and ceiling it is kept between. Values of type [t] are
    immutable*)

(** Tunable constants *)

val gravity : float
(** Donward acceleration applied every fram, in pixels/s^2. Always positive. *)

val thrust : float
(** Upwared acceleration applied while thurst is held, in pixels/s^2. Must be
    greater than [gravity], or the camel can never rise. *)

val max_rise : float
(** Fastest upward speed allowed, in pixels/s, given as a positive number. The
    velocity is never less than [-. max_rise]. *)

val max_fall : float
(** Fastest downboard speed allowed, in pxels/'s, given as a positive number.
    The velocity is never greater than [max_fall]. *)

val width : float
(** Width of the camel's hitbox, in pixels. *)

val height : float
(** Height of the camel's hitbox in pixels. *)

(** Creating the camel *)

val create : x:float -> ceiling:float -> floor:float -> t
(** [create ~x ~ceiling ~floor] is a camel at horizontal position [x], standing
    still on the floor, so its velocity is [0.] and its y is [floor -. height].
    [ceiling] is the screen y of the top of the play area and [floor] is the
    screen y of the ground.

    Requires: [ceiling +. height <= floor]. *)

(** Reading the state *)

val x : t -> float
(** [x p] is the screen x of the left edge of [p]'s hitbox. It does not change
    during a run, because the world scrolls past the camel. *)

val y : t -> float
(** [y p] is the screen y of the top edge of [p]'s hitbox. It is always between
    [ceiling] and [floor -. height]. *)

val velocity : t -> float
(** [velocity p] is [p]'s vertical velocity in pixels/s. Negative means moving
    up and positive means moving down. *)

val on_floor : t -> bool
(** [on_floor p] is [true] if [p] is resting on the floor. *)

val at_ceiling : t -> bool
(** [at_ceiling p] is true if [p] is touching the ceiling. *)

val hitbox : t -> float * float * float * float
(** [hitbox p] is [(x, y, w, h)]. The top-left corner of [p]'s hitbox and its
    width and height. The collision module uses this rectangle. *)

(** Moving *)

val update : thrusting:bool -> dt:float -> t -> t
(** [update ~thrusting ~dt p] is [p] after [dt] seconds: apply gravity and (if
    [thrusting]) thrust to the velocity, clamp it to [[-. max_rise, max_fall]],
    move by [velocity *. dt], then clamp the position between ceiling and floor,
    setting the velocity to [0.] on contact. Requires: [dt >= 0.]. *)
