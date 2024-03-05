open Board
(** The abstract type of values representing game state*)
type t


type card

type player

(** Functions to extract card fields*)
val get_troop : card -> string
val get_card_territory : card -> string
(** Functions to extract player fields*)
val get_name : player -> string
val get_territories : player -> territory list
val get_troops : player -> int
val get_deck : player -> card list
(** Functions to extract game state fields*)
val get_players : t -> player list
val get_phase : t -> int
val get_game_deck : t -> card list
val get_trade_in_ability : t -> bool
val get_trade_in_amount : t -> int
val get_game_territories : t -> territory list

(** [init_deck j] initializes a deck with 1/3 of the card being infantry, 
    1/3 being cavalry, and the rest being artillery. All cards have a 
    corresponding territory from the json map [j].*)
val init_deck : Yojson.Basic.t -> card list

(** [init_player n t i c] initializes a player with name [n], territory list 
    [t], number of troops [i], and deck [d]*)
val init_player : string -> Game__Board.territory list -> int -> card list -> player

(** [init_state p d j] is the initial game state after a player list [p],
    deck [d] and json map [j] are passed in from main*)
val init_state : player list -> card list -> Yojson.Basic.t -> t

(** [valid_trade g] returns a list of card lists that are valid trades to make
    with a given game state [g]*)
val valid_trade : t -> card list list

(** [trade g c] is the resulting game state from [g] after the player 
    makes a trade with 3 cards [c] that are predetermined as a valid
    trade*)
val trade : t -> t 

(** [draft g] is the resulting game state and territory list from [g] after 
    the player drafts a certain number of troops to territories and trades in 
    cards given the choice to *)
val draft : t -> t * Game__Board.territory list

(** [elimination g p] is the resulting game state from [g] after the current
    player eliminates a player [p] *)
val elimination : t -> player -> t

(** [update_lst t_lst t i] returns a territory list after adding a new 
    territory [t] with a number of troops on it [i] to a territory list [t_lst]*)
val update_list : Game__Board.territory list -> Game__Board.territory -> int -> Game__Board.territory list

(** [capture g i t] is the resulting game state from [g] after the player [i]
    captures a territory t with a certain number of troops from an 
    attacking territory *)
val capture : t -> int -> Game__Board.territory -> t

(** [attack g] is the resulting game state and territory list from [g] after 
    the player attacks a territory from a connecting territory*)
val attack : t -> t * Game__Board.territory list

(** [finished_game g] checks if anyone has won the game in its current state 
    [g] and returns a bool to indicate whether or not it has*)
val finished_game : t -> bool

(** [fortify g] is the resulting game state and territory list from [g] after 
    the player fortifies one territory with a certain number of troops
    from another territory *)
val fortify : t -> t * Game__Board.territory list


