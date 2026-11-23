(** Drawing the game on screen with the OCaml [Raylib] library.

    This first version is intentionally plain with a white background, solid black
    shapes, and black text with no colors or art while the rest of the game is being 
    built, and will be replaced later with more detailed art. Each frame, the game 
    loop calls in order of [clear], the draw functions for whatever is on screen, 
    then [present] to show the finished frame. *)

val init : unit -> unit
(** [init ()] opens the window at [World.screen_width] x [World.screen_height],
    and sets the title. *)

val clear : unit -> unit
(** [clear ()] fills the whole window with white. Call at the start of every
    frame before drawing anything else. *)

val draw_player : Player.t -> unit
(** [draw_player p] draws the camel as a solid black rectangle using
    [Player.hitbox]. A plain rectangle for now; it becomes a real camel sprite
    in the final version. *)

val draw_obstacle : Obstacles.t -> unit
(** [draw_obstacle o] draws [o] based on [Obstacles.kind]:

    - Beam: a thick black line between its two end points
      ([Obstacles.beam_ends]).
    - Missile: a solid black rectangle ([Obstacles.missile_position] with
      [Obstacles.missile_width] and [Obstacles.missile_height]).
    - Missile warning: a black outline only, so the player can see it cannot
      hurt the camel yet. *)

val draw_stats : distance:float -> speed:float -> unit
(** [draw_stats ~distance ~speed] draws the current distance and speed as black
    text near the top of the window. *)

val present : unit -> unit
(** [present ()] shows the finished frame Since [clear] appears on screen all at once. Call once at
    the end of every frame. *)

val close : unit -> unit
(** [close ()] closes the window. Call after the game loop ends. *)