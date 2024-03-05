open OUnit2
open Game
open Game__Board

(** TEST PLAN:
We tested all of our functions that don't have any user input using OUnit. 
All of our functions in the board module were able to be tested with OUnit. 
We tested each function in board at least once, and some of the larger functions
were tested more than once. It became a little more difficult to test the functions in
the game module since a lot of them require user input at some point in the execution. 
We found that it was significantly easier to test these functions that had terminal ouput 
through actual gameplay. We constructed our tests mainly through random testing. 
We gave the functions random game states and made sure the output was what we wanted. 
Most of the tests were glass box tests since the implementer was usually the one writing 
the tests. However, for our large game functions we made sure that people who didn't 
implement the functions still ensured of correctness. The smaller functions in the game and 
board compilation units were automatically tested with OUnit and the larger ones we made 
sure to cover every line possible when playing through our game. Functions like draft 
where there are many conditionals we ran through many times to ensure of correctness. A 
large portion of our debugging was done through this iterative process. We spent quite a few
hours ensuring each line of code was executed. We think that our test suite and gameplay 
testing approach demonstrate correctness of the system because we were quite thorough in 
the process. We wrote down all conditionals that needed to be executed in all of our game 
functions and checked off when each branch of the conditionals were executed. We spent over 
10 hours on this testing process alone and we feel very confident in the correctness of 
our system thanks to our process. *)


(* Helper functions cmp_set_like_lists, pp_string, pp_list taken from A2.
   pp_int, and pp_tuple were made for our own usage*)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether they are
    equivalent set-like lists. That means checking two things. First, they must
    both be "set-like", meaning that they do not contain any duplicates. Second,
    they must contain the same elements, though not necessarily in the same
    order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""
(** [pp_int i] pretty-prints int [i]. *)
let pp_int i = string_of_int i
(** [pp_tuple t] pretty-prints tuple [t]. *)
let pp_tuple t = "(\"" ^ fst t ^ "\",\"" ^ snd t ^ "\")"

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let territory_yojson =
  Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ "territories_basic.json")

let cornell_yojson = 
  Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ "cornell_map.json")



let get_territories_from_continent_test (name : string) (continent : string)
    (f : Yojson.Basic.t) (expected : string list) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string) expected
    (List.map get_territory_name
       (get_territories_from_continent (territories_from_file f) continent))

let get_territories_from_continent_notfound (name : string) (continent : string)
    (f : Yojson.Basic.t) (expected : string list) : test =
  name >:: fun _ ->
  assert_raises (InvalidFile f) (fun () ->
    List.map get_territory_name
       (get_territories_from_continent (territories_from_file f) continent))

let get_territory_from_string_test (name : string) (terr_name : string)
    (f : Yojson.Basic.t) (expected : string) : test =
  name >:: fun _ ->
  assert_equal expected
    (get_territory_name
       (get_territory_from_string terr_name (territories_from_file f)))
    ~printer:Fun.id

let get_terr_str_notfound (name : string) (terr_name : string)
    (f : Yojson.Basic.t) : test =
  name >:: fun _ ->
  assert_raises (UnknownTerritory terr_name) (fun () ->
      get_territory_from_string terr_name (territories_from_file f))

let territories_list_test (name : string) (f : Yojson.Basic.t)
    (continent : string) (expected : string list) : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string) expected
    (territories_list
       (get_territories_from_continent (territories_from_file f) continent))

let greenland =
  territory_yojson |> territories_from_file
  |> get_territory_from_string "Greenland"

let south_africa =
  territory_yojson |> territories_from_file
  |> get_territory_from_string "South Africa"

let ibc = cornell_yojson |> territories_from_file
|> get_territory_from_string "IBC"

let duffield = cornell_yojson |> territories_from_file
|> get_territory_from_string "Duffield"

let add_armies_to_territory_test (name : string) (t : territory)
    (num_troops : int) (expected : int) : test =
  name >:: fun _ ->
  let a = add_armies_to_territory t num_troops in
  assert_equal expected (get_territory_numtroops a)

let get_territory_numtroops_test (name : string) (t : territory)
    (expected : int) : test =
  name >:: fun _ -> assert_equal expected (get_territory_numtroops t)

let set_territory_owner_test (name : string) (t : territory) (player_num : int)
    (expected : int) : test =
  name >:: fun _ ->
  assert_equal expected (set_territory_owner t player_num |> get_player_number)

let get_player_number_test (name : string) (t : territory) (expected : int) :
    test =
  name >:: fun _ -> assert_equal expected (get_player_number t)

let num_territories_test (name : string) (num : int) (f : Yojson.Basic.t) 
(expected : int) : test =
  name >:: fun _ -> assert_equal expected (num_territories num (territories_from_file f))

let get_neighbors_test (name : string) (t : territory) (expected : string list)
    : test =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string) expected
    (get_neighbors t)

let get_name_test (name : string) (t : territory) (expected_output : string) : test =
  name >:: fun _ -> 
    assert_equal expected_output (get_territory_name t)

let board_tests =
  [
    get_territories_from_continent_test "Check territories of North America"
      "North America" territory_yojson
      [
        "Alaska";
        "Northwest Territory";
        "Greenland";
        "Quebec";
        "Eastern US";
        "Western US";
        "Central America";
        "Ontario";
        "Alberta";
      ];
    get_territories_from_continent_test "Check territories of South America"
      "South America" territory_yojson
      ["Argentina"; 
        "Brazil"; 
        "Venezuela"; 
        "Peru"
      ];
    get_territories_from_continent_test "Check territories of Africa"
      "Africa" territory_yojson
      [
        "Congo"; 
        "East Africa"; 
        "Egypt"; 
        "Madagascar"; 
        "North Africa"; 
        "South Africa"
      ];
    get_territories_from_continent_test "Check territories of Europe"
      "Europe" territory_yojson
      [
        "Great Britain"; 
        "Iceland"; 
        "Northern Europe"; 
        "Scandanavia"; 
        "Southern Europe"; 
        "Ukraine"; 
        "Western Europe"
      ]; 
    get_territories_from_continent_test "Check territories of Australia"
      "Australia" territory_yojson
      [
        "Eastern Australia"; 
        "New Guinea"; 
        "Indonesia"; 
        "Western Australia"
      ];
    get_territories_from_continent_test "Check territories of Asia"
      "Asia" territory_yojson
      [
        "Afghanistan"; 
        "China"; 
        "India"; 
        "Irkutsk"; 
        "Japan"; 
        "Kamchatka"; 
        "Middle East"; 
        "Mongolia"; 
        "Siam"; 
        "Siberia"; 
        "Ural"; 
        "Yakutsk"
      ];        
    get_territories_from_continent_test "Check territories of North Campus" 
      "North Campus" cornell_yojson
      [
        "RPCC";
        "Low-Rises";
        "Donlon";
        "Appel";
        "Jameson";
        "Ganedago";
        "Dickson";
        "Helen Newman";
        "Morrison";
        "CKB"
      ];
    get_territories_from_continent_test "Check territories of Eng Quad"
    "Engineering Quad" cornell_yojson
    [
      "Statler"; 
      "Duffield"; 
      "Carpenter"; 
      "Construction"; 
      "Rhodes"; 
      "Snee"
    ];
    get_territories_from_continent_test "Check territories of Arts Quad"
    "Arts Quad" cornell_yojson
    [
      "Ho Plaza"; 
      "Olin"; 
      "Goldwin Smith"; 
      "Johnson"; 
      "McGraw"; 
      "Uris"
    ];
    get_territories_from_continent_test "Check territories of College Town"
    "College Town" cornell_yojson
    [
      "IBC";
      "7/11";
      "CTB";
      "Ned's";
      "Hideaway";
      "Level B";
      "Loco"
    ];
    get_territories_from_continent_test "Check territories of West Campus"
    "West Campus" cornell_yojson
    [
      "Baker Tower"; 
      "Noyes Rec"; 
      "Keaton"; 
      "Bethe"; 
      "Rose"; 
      "Cook"
    ];
    get_territories_from_continent_test "Nonexistant Continent"
    "North Pole" cornell_yojson
    [];
    get_territories_from_continent_test "Misspelled Continent"
    "west campus" cornell_yojson
    [];
    get_territory_from_string_test "Search for Venezuela" "Venezuela"
      territory_yojson "Venezuela";
    get_territory_from_string_test "Search for Greenland" "Greenland"
      territory_yojson "Greenland";
    get_terr_str_notfound "Search for nonexistant territory" "Antarctica"
      territory_yojson;
    territories_list_test "Check territories of Asia as String List"
      territory_yojson "Asia"
      [
        "Afghanistan";
        "China";
        "India";
        "Irkutsk";
        "Japan";
        "Kamchatka";
        "Middle East";
        "Mongolia";
        "Siam";
        "Siberia";
        "Ural";
        "Yakutsk";
      ];
    territories_list_test "Check territories of North America as String List"
      territory_yojson "North America"
      [
        "Alaska"; 
        "Alberta"; 
        "Central America"; 
        "Eastern US"; 
        "Greenland"; 
        "Northwest Territory"; 
        "Ontario"; 
        "Quebec"; 
        "Western US"
      ];  
    territories_list_test "Check territories of Arts Quad as String List"
      cornell_yojson "Arts Quad"
      [
        "Ho Plaza"; 
        "Olin"; 
        "Goldwin Smith"; 
        "Johnson"; 
        "McGraw"; 
        "Uris"
      ];
    territories_list_test "Check territories of Engineering Quad as String List"
      cornell_yojson "Engineering Quad"
      [
        "Statler"; 
        "Duffield"; 
        "Carpenter"; 
        "Construction"; 
        "Rhodes"; 
        "Snee"
      ];   
    add_armies_to_territory_test "Add 2 troops to Greenland" greenland 2 2;
    get_territory_numtroops_test "Get num troops from South Africa" south_africa
      0;
    add_armies_to_territory_test "Add troops to South Africa" south_africa 0 0;
    set_territory_owner_test "Set Greenlands owner to 2" greenland 2 2;
    set_territory_owner_test "Set Duffield's owner to 1" duffield 1 1; 
    get_neighbors_test "Get South Africa's neighbors" south_africa
      [ "Madagascar"; "Congo"; "East Africa" ];
    get_neighbors_test "Get Greenland neighbors" greenland
      [ "Iceland"; "Northwest Territory"; "Ontario"; "Quebec" ];
    get_neighbors_test "Get IBC's neighbors" ibc
      [ "7/11"; "Snee"; "Construction"; "CTB"];
    get_neighbors_test "Get Duffield's neighbors" duffield
      ["Carpenter"; "Statler"; "Construction"; "Rhodes"]; 
    get_name_test "Get Greenland's name" greenland "Greenland";
    get_name_test "Get Duffield's name" duffield "Duffield";
    get_name_test "Get IBC's name" ibc "IBC";
    num_territories_test "Get number of territories in world" 0 territory_yojson 42;
    num_territories_test "Get number of territories in Cornell" 0 cornell_yojson 35;
    num_territories_test "Add number of territories in Cornell to 10" 10 cornell_yojson 45;
    num_territories_test "Add number of territories in world to 500" 500 territory_yojson 542
  ]

let d1 = init_deck territory_yojson
let d2 = init_deck cornell_yojson

let d_to_tuple d =
  List.map (fun x -> (Game.get_troop x, Game.get_card_territory x)) d

let deck_test 
  (name : string) 
  (deck : card list)
  (expected_output : (string * string) list) : test =
  name >:: fun _ -> assert_equal ~printer:(pp_list pp_tuple) expected_output (d_to_tuple deck)

let europe =
  get_territories_from_continent
    (territories_from_file territory_yojson)
    "Europe"

let asia =
  get_territories_from_continent (territories_from_file territory_yojson) "Asia"

let scandanavia =
  add_armies_to_territory (get_territory_from_string "Scandanavia" europe) 3

let iceland =
  add_armies_to_territory (get_territory_from_string "Iceland" europe) 5

let gb =
  add_armies_to_territory (get_territory_from_string "Great Britain" europe) 7

let china = add_armies_to_territory (get_territory_from_string "China" asia) 2
let india = add_armies_to_territory (get_territory_from_string "India" asia) 4
let t1 = [ scandanavia; iceland; gb ]
let t2 = [ china; india ]

let p_all = init_player "winner" (territories_from_file territory_yojson) 0 d1
let p1 =
  init_player "Bob"
    (get_territories_from_continent
       (territories_from_file territory_yojson)
       "North America")
    0 d1

let p2 =
  init_player "Dave"
    (get_territories_from_continent
       (territories_from_file territory_yojson)
       "South America")
    0 d1

let p3 = init_player "Joe" t2 20 d1
let p4 = init_player "Matt" t1 12 d1

let p5 = 
  init_player "Aussie"
    (get_territories_from_continent
      (territories_from_file territory_yojson)
        "Australia")
      0 d1

let p6 = 
  init_player "Africa"
    (get_territories_from_continent
      (territories_from_file territory_yojson)
        "Africa")
      0 d1

let rowan = 
  init_player "Rowan"
    (get_territories_from_continent
      (territories_from_file cornell_yojson)
        "North Campus")
      0 d1

let nigel = 
  init_player "Nigel"
    (get_territories_from_continent
      (territories_from_file cornell_yojson)
        "West Campus")
      0 d1
let aidan = 
  init_player "Aidan"
    (get_territories_from_continent
      (territories_from_file cornell_yojson)
        "Engineering Quad")
      0 d1

let teresa = 
  init_player "Teresa"
  (get_territories_from_continent
    (territories_from_file cornell_yojson)
      "College Town")
    0 d1  

let tad =
  init_player "Tad"
  (get_territories_from_continent
    (territories_from_file cornell_yojson)
      "Arts Quad")
    0 d1  

let player_test name expected_output p =
  name >:: fun _ ->
  assert_equal true (cmp_set_like_lists expected_output (List.map get_territory_name (Game.get_territories p)))

let g1 = init_state [p1;p2] d1 territory_yojson

let g2 = init_state [p4;p3;p1;p2] d1 territory_yojson

let g3 = init_state [p3;p4;p1;p2] d1 territory_yojson

let g4 = init_state [p_all] d1 territory_yojson

let g5 = init_state [p_all;p_all;p_all;p_all] d1 territory_yojson

let elimination_test (name : string) (state : Game.t) (player : Game.player)
    (expected_output : string list) : test =
  name >:: fun _ ->
  let x = elimination state player in

  let list = List.map Game.get_name (Game.get_players x) in
  assert_equal ~printer:(pp_list pp_string) expected_output list

let update_list_test (name : string) (lst : Game__Board.territory list)
    (ter : Game__Board.territory) (x : int) (expected_output : int list) : test
    =
  name >:: fun _ ->
  let out = Game.update_list lst ter x in
  let list = List.map Game__Board.get_territory_numtroops out in
  assert_equal ~printer:(pp_list pp_int) expected_output list

let finished_game_test 
  (name : string)
  (state : Game.t)
  (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (finished_game state)

let game_tests =
  [
    deck_test "Initial" d1 [("Cavalry","Alaska"); ("Infantry","Alberta"); ("Artillery","Central America"); ("Cavalry","Eastern US"); ("Infantry","Greenland"); ("Artillery","Northwest Territory"); ("Cavalry","Ontario"); ("Infantry","Quebec"); ("Artillery","Western US"); ("Cavalry","Argentina"); ("Infantry","Brazil"); ("Artillery","Venezuela"); ("Cavalry","Peru"); ("Infantry","Congo"); ("Artillery","Alaska"); ("Cavalry","Alberta"); ("Infantry","Central America"); ("Artillery","Eastern US"); ("Cavalry","Greenland"); ("Infantry","Northwest Territory"); ("Artillery","Ontario"); ("Cavalry","Quebec"); ("Infantry","Western US"); ("Artillery","Argentina"); ("Cavalry","Brazil"); ("Infantry","Venezuela"); ("Artillery","Peru"); ("Cavalry","Congo"); ("Infantry","Alaska"); ("Artillery","Alberta"); ("Cavalry","Central America"); ("Infantry","Eastern US"); ("Artillery","Greenland"); ("Cavalry","Northwest Territory"); ("Infantry","Ontario"); ("Artillery","Quebec"); ("Cavalry","Western US"); ("Infantry","Argentina"); ("Artillery","Brazil"); ("Cavalry","Venezuela"); ("Infantry","Peru"); ("Artillery","Congo")];
    deck_test "Cornell" d2 [("Cavalry","RPCC"); ("Infantry","Low-Rises"); ("Artillery","Donlon"); ("Cavalry","Appel"); ("Infantry","Jameson"); ("Artillery","Ganedago"); ("Cavalry","Dickson"); ("Infantry","Helen Newman"); ("Artillery","Morrison"); ("Cavalry","CKB"); ("Infantry","Statler"); ("Artillery","RPCC"); ("Cavalry","Low-Rises"); ("Infantry","Donlon"); ("Artillery","Appel"); ("Cavalry","Jameson"); ("Infantry","Ganedago"); ("Artillery","Dickson"); ("Cavalry","Helen Newman"); ("Infantry","Morrison"); ("Artillery","CKB"); ("Cavalry","Statler"); ("Infantry","RPCC"); ("Artillery","Low-Rises"); ("Cavalry","Donlon"); ("Infantry","Appel"); ("Artillery","Jameson"); ("Cavalry","Ganedago"); ("Infantry","Dickson"); ("Artillery","Helen Newman"); ("Cavalry","Morrison"); ("Infantry","CKB"); ("Artillery","Statler")];
    player_test "North America" [
      "Alaska";
      "Northwest Territory";
      "Greenland";
      "Quebec";
      "Eastern US";
      "Western US";
      "Central America";
      "Ontario";
      "Alberta";
    ] p1; 
    player_test "South America" [
      "Argentina";
      "Brazil";
      "Venezuela";
      "Peru"
    ] p2;
    player_test "Asia" [
      "China";
      "India"
    ] p3;
    player_test "Europe" [
      "Scandanavia";
      "Iceland";
      "Great Britain"
    ] p4;
    player_test "Australia" [
      "Eastern Australia";
      "New Guinea";
      "Western Australia";
      "Indonesia"
    ] p5;
    player_test "Africa" [
      "Congo";
      "East Africa";
      "Egypt";
      "Madagascar";
      "North Africa";
      "South Africa"
    ] p6;
    player_test "North Campus" [
      "RPCC";
      "Low-Rises";
      "Donlon";
      "Appel";
      "Jameson";
      "Ganedago";
      "Dickson";
      "Helen Newman";
      "Morrison";
      "CKB"
    ] rowan;
    player_test "Engineering Quad" [
      "Statler";
      "Duffield";
      "Carpenter";
      "Construction";
      "Rhodes";
      "Snee"
    ] aidan;
    player_test "College Town" [
      "IBC";
      "7/11";
      "CTB";
      "Ned's";
      "Hideaway";
      "Level B";
      "Loco"
    ] teresa;
    player_test "Arts Quad" [
      "Ho Plaza";
      "Olin";
      "Goldwin Smith";
      "Johnson";
      "McGraw";
      "Uris"
    ] tad;
    player_test "West Campus" [
      "Baker Tower";
      "Noyes Rec";
      "Keaton";
      "Bethe";
      "Rose";
      "Cook"
    ] nigel;
    elimination_test "elimination test" g1 p2 ["Bob"];
    elimination_test "eliminating with multiple players" g2 p1 ["Matt"; "Joe"; "Dave"];
    elimination_test "eliminating with only one player" g4 p_all [];
    elimination_test "eliminating the middle element" g2 p3 ["Matt"; "Bob"; "Dave"];
    update_list_test "updating Iceland" t1 iceland 5 [3;10;7];
    update_list_test "updating China" t2 china 12 [14;4];
    finished_game_test "state 1" g1 false;
    finished_game_test "state 2" g2 false;
    finished_game_test "state 3" g3 false;
    finished_game_test "finished state" g4 true;
    finished_game_test "multiple winnings" g5 true
]

let suite = "test suite for risk" >::: List.flatten [ board_tests; game_tests ]
let _ = run_test_tt_main suite
