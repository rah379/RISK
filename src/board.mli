(**TODO: Finish Documentation w/ more than bare bones*)
type territory
(**The abstract type representing a territory*)

exception UnknownTerritory of string
(**Raised when asked for a territory that doesn't exist*)

exception NotNeighbors of string
(**Raised when a player attempts to attack a non-neighbor*)

exception InvalidFile of Yojson.Basic.t
(**Raised when json file cannot be properly read into a territory list*)

val territories_from_file : Yojson.Basic.t -> territory list
(** [territories_from_file f] is the game's initial territory list specified by
    [f]. Requires [f] is a valid json file of territories containing names,
    continents, and neighbors*)

val get_territory_from_string : string -> territory list -> territory
(** [get_territory_from_string t lst] is the element in the territory list [lst]
    that is named [t]. Raises: [UnknownTerritory] if [t] not found. *)

val add_armies_to_territory : territory -> int -> territory
(** [add_armies_to_territory t n] adds [n] to the number of armies in [t]*)

val get_territories_from_continent : territory list -> string -> territory list
(** [get_territory_from_continent lst t] is the list of territories in [lst]
    whose continent is named [t]. *)

val num_territories : int -> territory list -> int
(** [num_territories n lst] adds the number of territories in [lst] to [n]*)

val set_territory_owner : territory -> int -> territory
(** [set_territory_owner t n] modifies the player number of territory [t] to [n]*)

val get_player_number : territory -> int
(** [get_player_number t] is the number corresponding to the player that
    controls territory [t] *)

val territories_list : territory list -> string list
(** [territories_list lst] is the list of names of the territories in [lst] *)

val get_territory_name : territory -> string
(** [get_territory_name t] is the name of [t]*)

val get_territory_numtroops : territory -> int
(** [get_territory_numtroops t] is the number of troops in [t]*)

val get_neighbors : territory -> string list
(** [get_neighbors t] is the list of neighbors of [t]*)
