open Game
open Game__Board

(*To run, run "OCAMLRUNPARAM=b dune exec bin/main.exe" in command line, or make
  play*)

(** Printing functions for ANSITerminal printing*)
let print_green s = ANSITerminal.print_string [ANSITerminal.green] s
let print_white s = ANSITerminal.print_string [ANSITerminal.white] s  

(** [number_of_players] is an int ref that will be changed depending on how 
    many are playing*)  
let number_of_players = ref (-1)

(** [player_color] sets each player to have a certain distinct color in the 
    terminal*)
let player_color =
  [
    (-1, ANSITerminal.white);
    (0, ANSITerminal.blue);
    (1, ANSITerminal.red);
    (2, ANSITerminal.yellow);
    (3, ANSITerminal.cyan);
    (4, ANSITerminal.green);
    (5, ANSITerminal.magenta);
  ]

(** [initial_troops] gives each player a certain number of troops depending 
    on how many players are in the game*)  
let initial_troops =
  [
    (2, Array.make 2 40);
    (3, Array.make 3 35);
    (4, Array.make 4 20);
    (5, Array.make 5 25);
    (6, Array.make 6 20);
  ]

let rules_string = "\nWelcome to RISK in OCaml!
RISK is a popular strategy board game that is played with two to six players. 
The goal of the game is to conquer the world by capturing all the territories 
on the game board. The game board is divided into several continents, which 
are further divided into territories. At the beginning of the game, each player 
is assigned a certain number of territories and armies. Players take turns 
placing their armies on their territories and attacking their opponents. 
Players can attack territories adjacent to their own territories, and can 
continue attacking as long as they have armies to spare. If a player captures a
territory, they can place additional armies on it. The game continues until one 
player has conquered all the territories on the game board, at which point they 
are declared the winner. Risk involves a combination of strategy, luck, and 
diplomacy, making it a challenging and engaging game.\n\n
Press Enter to proceed to the game"  

let territories_owned = Array.make 6 []

(** [get_num_players ()] reads input from the user to get the number of players
    in the current game*)
let rec get_num_players () : int =
  print_white "> ";
  let x = read_line () in
  try
    if int_of_string x < 2 || int_of_string x > 6 then (
      print_green
        "Sorry, RISK is a 2-6 player game! \n";
      get_num_players ())
    else int_of_string x
  with exn ->
    if String.uppercase_ascii x = "QUIT" then (
      print_green "Goodbye!\n";
      exit 0)
    else
      print_green
        "hmmm this didn't seem to be a valid integer- enter the ASCII \
         character of how many players are playing, i.e 3 \n";
    get_num_players ()

(** [get_map ()] gets the user input on which map they would like to use for
    their game of RISK*)    
let rec get_map () : territory list * string =
  print_white "> ";
  let x = read_line () in
  try
    let board =
      ( Game__Board.territories_from_file
          (Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ x ^ ".json")),
        x ^ ".txt" )
    in
    board
  with exn ->
    if String.uppercase_ascii x = "QUIT" then (
      print_green "Goodbye!\n";
      exit 0)
    else
      print_green
        "Hmm this doesn't seem to be a valid map. Try territories_basic \n";
    get_map ()

(** [read_whole_file f] evaluates to contents of a file [f] as a string*)
let read_whole_file 
(filename : string) : string =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

(** [territories_from_players p] takes in a player list [p] and outputs a 
    territory list of all the territories each player possesses*)  
let territories_from_players 
(players : player list) : territory list =
  List.fold_left (fun acc player -> acc @ get_territories player) [] players

(** [print_map m t] takes in the map name [m] and current territory list [t]
    and outputs the current map in the terminal*)
let print_map 
(map_name : string) 
(terr_list : territory list) : unit =
  ANSITerminal.erase Screen;
  let map_string = read_whole_file ("data" ^ Filename.dir_sep ^ map_name) in
  let map_list = String.split_on_char ',' map_string in
  List.iter
    (fun x ->
      if
        List.exists
          (fun z -> String.compare z x = 0)
          (Game__Board.territories_list terr_list)
      then
        ANSITerminal.print_string
          [
            snd
              (List.find
                 (fun z ->
                   fst z
                   = get_player_number
                       (Game__Board.get_territory_from_string x terr_list))
                 player_color);
            ANSITerminal.Bold;
          ]
          (x ^ ", "
          ^ string_of_int
              (Game__Board.get_territory_numtroops
                 (Game__Board.get_territory_from_string x terr_list)))
      else print_white x)
    map_list;
  ANSITerminal.print_string [ ANSITerminal.blue ] "\nPlayer One is Blue";
  ANSITerminal.print_string [ ANSITerminal.red ] ", Player Two is Red";
  if !number_of_players > 2 then
    ANSITerminal.print_string [ ANSITerminal.yellow ] ", Player Three is Yellow";
  if !number_of_players > 3 then
    ANSITerminal.print_string [ ANSITerminal.cyan ] ", Player Four is Cyan";
  if !number_of_players > 4 then
    print_green ", Player Five is Green";
  if !number_of_players > 5 then
    ANSITerminal.print_string [ ANSITerminal.magenta ] ", Player Six is Magenta";
  print_white "\n"

(** [assign_players n p num i t] assigns territories [t] randomly to each player
    based on a number of players [n] and number of territories [num]*)
let rec assign_players 
(num_players : int)
(players_num_territories : (int * int ref) list) 
(num_territories : int)
(initial_troops : int array) 
(terr_list : territory list) : territory list =
  Random.self_init ();
  match terr_list with
  | [] -> []
  | h :: t ->
      let roll = Random.int num_players in
      let num_territories_roller =
        snd (List.find (fun z -> fst z = roll) players_num_territories)
      in
      if
        num_territories mod num_players = 0
        && !num_territories_roller >= num_territories / num_players
        || !num_territories_roller >= (num_territories / num_players) + 1
      then
        assign_players num_players players_num_territories num_territories
          initial_troops terr_list
      else (
        num_territories_roller := !num_territories_roller + 1;
        initial_troops.(roll) <- initial_troops.(roll) - 1;
        ignore (Game__Board.add_armies_to_territory h 1);
        ignore (Game__Board.set_territory_owner h roll);
        territories_owned.(roll) <- h :: territories_owned.(roll);
        h
        :: assign_players num_players players_num_territories num_territories
             initial_troops t)

(** [put_troops_here c t num p i] puts a number of troops on a territory [t] based
    on string input [i] *)
let rec put_troops_here 
(color : ANSITerminal.style) 
(t : territory) 
(num_players : int)
(player_num : int) 
(input : string) : unit =
  try
    let want_troops_int = int_of_string input in
    if
      want_troops_int < 0
      || want_troops_int
         > (snd (List.find (fun x -> fst x = num_players) initial_troops)).(player_num)
    then (
      ANSITerminal.print_string [ color ]
        "This is not a valid amount of troops, either it is negative or you do \
         not have enough troops. Please try again \n\
        \ > ";
      put_troops_here color t num_players player_num (read_line ()))
    else (
      (snd (List.find (fun x -> fst x = num_players) initial_troops)).(player_num) <-
        (snd (List.find (fun x -> fst x = num_players) initial_troops)).(player_num)
        - want_troops_int;
      ignore (Game__Board.add_armies_to_territory t want_troops_int))
  with exn ->
    if String.compare (String.uppercase_ascii input) "QUIT" = 0 then (
      ANSITerminal.print_string [ color ] "Goodbye!\n";
      exit 0)
    else
      ANSITerminal.print_string [ color ]
        "This doesn't seem to be a valid integer. Please input the ASCII \
         representation of an integer \n\
        \ > ";
    put_troops_here color t num_players player_num (read_line ())

(** [mutable_player_assign_troops n t m] assigns a number of troops to a player
    in the beginning of the game*)
let rec mutable_player_assign_troops 
(num_players : int)
(terr_list : territory list) 
(map_name : string) : territory list =
  let looper = ref num_players in
  let first_player = ref 0 in
  while !looper <> 0 do
    let terr_owned = territories_owned.(!first_player) in
    let color = snd (List.find (fun z -> fst z = !first_player) player_color) in
    ANSITerminal.print_string [ color ]
      ("\nPlayer " ^ string_of_int (!first_player + 1) ^ ": ");
    List.iter
      (fun x ->
        ANSITerminal.print_string [ color ]
          ("You have control of "
          ^ Game__Board.get_territory_name x
          ^ " and have "
          ^ string_of_int
              (snd (List.find (fun x -> fst x = num_players) initial_troops)).(!first_player)
          ^ " troops left. How many would you like to put here? \n > ");
        let input = read_line () in
        put_troops_here color x num_players !first_player input;
        ANSITerminal.erase Screen;
        print_map map_name terr_list)
      terr_owned;
    first_player := (!first_player + 1) mod num_players;
    looper := !looper - 1
  done;
  terr_list

let ignore _ = ()

(** [start_game n t m] starts the game based on the number of players [n] and 
    territory list that it's given [t]*)
let start_game 
(num_players : int) 
(terr_list : territory list)
(map_name : string) : territory list =
  print_green
    "Looks like we're ready to get going! \n";
  print_green
    "First, the game will randomly assign you the proper amount of troops for \
     how many players are playing and give you control of a proportionate \
     amount of countries. Then, it will be up to you to place your remaining \
     troops on the territories you control, based on locations and strategy. \
     The game will roll to decide who goes first!";
  print_green
    "\nHit Enter when you understand and are ready to begin!\n";
  ignore (read_line ());
  let new_terr_list =
    assign_players num_players
      [ (0, ref 0); (1, ref 0); (2, ref 0); (3, ref 0); (4, ref 0); (5, ref 0) ]
      (Game__Board.num_territories 0 terr_list)
      (snd (List.find (fun x -> fst x = num_players) initial_troops))
      terr_list
  in
  print_map map_name new_terr_list;
  let init_board =
    mutable_player_assign_troops num_players new_terr_list map_name
  in
  print_map map_name init_board;
  init_board

(** [players_from_territories t] extracts a player list from a list of 
    territories [t]*)
let players_from_territories 
(ters : territory list) : player list =
  let players =
    Array.init 6 (fun x -> Game.init_player (string_of_int x) [] 0 [])
  in
  List.iter
    (fun x ->
      players.(Game__Board.get_player_number x) <-
        Game.init_player
          (string_of_int (Game__Board.get_player_number x))
          (x :: Game.get_territories players.(Game__Board.get_player_number x))
          0 [])
    ters;
  List.filter
    (fun x -> not (x = Game.init_player (string_of_int 0) [] 0 []))
    (Array.to_list players)

(** [main ()] contains the beginning information and main game loop for the 
    entire game*)
let main () =
  print_green rules_string;
  ignore (read_line ());
  print_green
    "\n\
     Enter quit at any point to quit the game. \n\
     How many players are playing in your game?\n";
  let num_players = get_num_players () in
  number_of_players := num_players;
  print_green
    "What map would you like to play? Our options are territories_basic and \
     cornell_map \n";
  let board = get_map () in
  print_white "\n";
  let players =
    players_from_territories (start_game num_players (fst board) (snd board))
  in
  let cards = 
    init_deck 
      (Yojson.Basic.from_file
       ("data" ^ Filename.dir_sep
           ^ String.sub (snd board) 0 (String.length (snd board) - 3)
        ^ "json")) 
  in
  let initial =
    ref
      ( Game.init_state players cards
          (Yojson.Basic.from_file
             ("data" ^ Filename.dir_sep
             ^ String.sub (snd board) 0 (String.length (snd board) - 3)
             ^ "json")),
        [] )
  in
  while finished_game (fst !initial) = false do
    let t = Game.draft (fst !initial) in
    print_map (snd board) (snd t);
    let i = Game.attack (fst t) in
    print_map (snd board) (snd i);
    initial := Game.fortify (fst i);
    print_map (snd board) (snd !initial)
  done;
  let winner = List.hd (get_players (fst !initial)) in
  print_green
    ("\n\n\nCongratulations player "  ^ 
    string_of_int (int_of_string (get_name (winner)) + 1)  
    ^ ", you have conquered the world!\n\n\n");
  ()

let () = main ()

