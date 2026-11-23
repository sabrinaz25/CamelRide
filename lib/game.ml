
type status =
  | Running
  | Game_over

type t = {
  player_state : Player.t;
  world_state : World.t;
  obstacle_list : Obstacles.t list;
  collision_state : Collision.t;
  game_status : status;
}

(* Creates a new game. *)
let init ~seed =
  let world = World.init ~seed in

  let player =
    Player.create
      ~x:100.0
      ~ceiling:0.0
      ~floor:(World.screen_height -. Player.height)
  in

  {
    player_state = player;
    world_state = world;
    obstacle_list = [];
    collision_state = Collision.create ();
    game_status = Running;
  }

(* Returns the current game status. *)
let status game =
  game.game_status

(* Returns the current player. *)
let player game =
  game.player_state

(* Returns the current world. *)
let world game =
  game.world_state

(* Returns the current obstacles. *)
let obstacles game =
  game.obstacle_list

(* Returns the current collision state. *)
let collision game =
  game.collision_state

(* Updates the game by one frame. *)
let update ~thrusting ~dt game =
  match game.game_status with
  | Game_over ->
      game

  | Running ->
      (* 1. Update the world and get new obstacle spawns. *)
      let new_world, spawns =
        World.advance game.world_state ~dt
      in

      (* 2. Update player physics. *)
      let new_player =
        Player.update ~thrusting ~dt game.player_state
      in

      (* 3. Update existing obstacles. *)
      let updated_obstacles =
        Obstacles.update_all
          game.obstacle_list
          ~speed:(World.speed new_world)
          ~target_y:(Player.y new_player)
          ~dt
      in

      (* 4. Convert new spawns into obstacles. *)
      let new_obstacles =
        List.map Obstacles.of_spawn spawns
      in

      (* 5. Combine obstacles and remove offscreen ones. *)
      let all_obstacles =
        updated_obstacles @ new_obstacles
        |> List.filter (fun o -> not (Obstacles.is_offscreen o))
      in

      (* 6. Check for collisions. *)
      let new_collision =
        Collision.check
          ~player:new_player
          ~obstacles:all_obstacles
          game.collision_state
      in

      (* 7. Determine whether the game is still running. *)
      let new_status =
        if Collision.is_alive new_collision then
          Running
        else
          Game_over
      in

      (* 8. Return the updated game state. *)
      {
        player_state = new_player;
        world_state = new_world;
        obstacle_list = all_obstacles;
        collision_state = new_collision;
        game_status = new_status;
      }

(* Starts a fresh game using a new random seed. *)
let restart ~seed _game =
  init ~seed
