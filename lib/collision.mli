
(** Detects collisions between Carrey the Camel and obstacles.

    This module checks whether the player's hitbox intersects
    dangerous obstacle hitboxes and tracks whether the player
    is still alive.

    It does not move the player or obstacles, draw anything,
    or read keyboard input. *)

type t
(** Abstract state of collision detection during a run. *)

val create : unit -> t
(** [create ()] creates a new collision state with the player alive. *)

val check :
  player:Player.t ->
  obstacles:Obstacles.t list ->
  t ->
  t
(** [check ~player ~obstacles state] checks whether the player
    intersects any dangerous obstacle.

    If a collision occurs, the player is marked as dead.
    Missile warnings do not cause collisions. *)

val is_alive : t -> bool
(** [is_alive state] indicates whether the player is still alive. *)
