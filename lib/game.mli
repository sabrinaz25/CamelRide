
(** Coordinates the main Jetpack Joyride game.

    This module manages the overall game state and coordinates
    updates to the player, world, obstacles, and collision modules.
    *)

type t
(** Abstract type representing the complete game state. *)

type status =
  | Running
  | Game_over
(** The current status of the game. *)

(** Creating a game *)

val init : seed:int -> t
(** [init ~seed] creates a new game using the given random seed.
    Initializes the player, world, obstacles, and collision state. *)

(** Reading game state *)

val status : t -> status
(** [status game] returns the current game status. *)

val player : t -> Player.t
(** [player game] returns the current player state. *)

val world : t -> World.t
(** [world game] returns the current world state. *)

val obstacles : t -> Obstacles.t list
(** [obstacles game] returns the currently active obstacles. *)

val collision : t -> Collision.t
(** [collision game] returns the current collision state. *)

(** Updating the game *)

val update : thrusting:bool -> dt:float -> t -> t
(** [update game] advances the game by [dt] seconds.

    If the game is running:
    - Updates the world and obtains newly scheduled obstacle spawns.
    - Creates new obstacles using [Obstacles.of_spawn].
    - Updates the player's physics.
    - Updates existing obstacles using [Obstacles.update_all].
    - Checks for collisions using [Collision.check].
    - Updates the game status if the player dies.

    If the game is over, the state remains unchanged.

   *)

(** Restarting the game *)

val restart : seed:int -> t -> t
(** [restart game] starts a new game using the supplied
    random seed, resetting the player, world, obstacles,
    and collision state. *)
