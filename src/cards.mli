(** Module [cards] represents cards and folders associated with these cards and
    the actions that can be done to instances of these types (add, delete, edit,
    create, view, multiplechoice, test, etc.) *)

type folder = string
(** type [folder] represents the home of cards *)

type card = {
  front : string;
  back : string;
  home : folder;
}
(** type [card] represents an individual flashcard *)

(** type [command] represents different valid user inputs *)
type command =
  | View of string list
  | Create of string list
  | Add of string list
  | Delete of string list
  | Edit of string list
  | Test
  | WrongInput
  | Help
  | MoveOn
  | Try
  | MultipleChoice
  | Quit

val parse : string -> command
(** [parse s] parses a user's input into a command

    - [parse "   QuIt"] -> [Quit]
    - [parse "delete card  "] -> [Delete ["card"]]*)

val create_folder : command -> unit
(** [create_folder c] creates a folder by asking user for folder name. Does not
    execute if folder exists. *)

val view_folder : command -> unit
(** [view_folder c] prints folder names in terminal *)

val add_card : command -> unit
(** [add_card] adds a card to cards.json and relates it to specific folder home *)

val delete_something : command -> unit
(** [delete_something c] deletes a folder and its repsective cards or a specific
    card*)

val help : unit -> unit
(** [help] prints out the commands that can be used in quizlet program*)

val test : unit -> unit
(** [test] tests user on flashcards' back information *)

val multi : unit -> unit
(** [multi] tests user on flashcards with multiple choice quiz *)

val find_home : string list -> string -> bool
(** [find_home lst home] returns true if home exists and false otherwise *)

val create_folder_json : string -> string list -> string
(** [create_folder_json new_folder folders] returns json formatted string of
    folders that were added*)

val delete_folder_json : string -> string list -> string
(** [delete_folder_json old_folder folders] returns json formatted string with
    all folders that still exist*)

val print_folders_string : string list -> string -> string
(** [print_folders_string lst acc] returns a string of all folder names *)

val get_card_string : card list -> string -> string -> string
(** [get_card_string c home acc] returns a string of all front and back card
    information for a specific folder*)

val json_all_new_cards : folder -> folder -> folder -> string -> folder
(** [json_all_new_cards front back home card_file] creates JSON formatted string
    including new card information*)

val test_intro : unit -> unit
(** [test_intro] used in main.ml execution of makequizlet to print out `test`
    introduction*)

val multiple_choice_intro : unit -> unit
(** [multiple_choice_intro] used in main.ml execution of makequizlet to print
    out `multiplechoice` introduction*)

val edit_something : command -> unit
(** [edit_something c] modifies the back of a card or a folder name *)

val edit_folder_json : string -> string -> string list -> string
(** [edit_folder_json old_folder_name new_folder_name folders] returns updated
    folders with new names *)

val make_new_card_list_back :
  card list -> string -> string -> string -> card list -> card list
(** [make_new_card_list_back cards home front_card new_back acc] creates a new
    card list with card with new back information*)

val make_new_card_list : card list -> string -> string -> card list -> card list
(** [make_new_card_list card_lst old_home new_home acc] returns a new list of
    cards with all occurences of old_home replaced with new_home*)
