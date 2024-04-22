open Yojson.Basic.Util

type folder = string

(* type t represents a folder list for json file conversion *)
type t = { folders : folder list }

type card = {
  front : string;
  back : string;
  home : folder;
}

(* type t1 represents a card list for json file conversion *)
type t1 = { cards : card list }

(** [card c] converts a Yojson.t to a type card *)
let card c =
  {
    front = c |> member "front" |> to_string;
    back = c |> member "back" |> to_string;
    home = c |> member "home" |> to_string;
  }

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

let data_dir_prefix = "data" ^ Filename.dir_sep
let folder_file = data_dir_prefix ^ "folders.json"
let card_file = data_dir_prefix ^ "cards.json"

(* [folder f] helper for [parse_json_file filename] to convert Yosjon.t to
   folder type*)
let folder f = to_string f

(* [parse_json_file filename] returns a folder list by reading json file *)
let parse_json_file (filename : string) : folder list =
  let json = Yojson.Basic.from_file filename in
  json |> member "folders" |> to_list |> List.map folder

(* [parse_json_file_for_card filename] reads in cards.json and returns a list of
   cards*)
let parse_json_file_for_card (filename : string) : card list =
  let json = Yojson.Basic.from_file filename in
  json |> member "cards" |> to_list |> List.map card

(* [parse_helper lst] helper function for parse that does not read whitespace
   prior to first word*)
let rec parse_helper (lst : string list) : string list =
  match lst with
  | [] -> []
  | h :: t -> if h = "" then parse_helper t else h :: parse_helper t

(* [is_whitespace_only str] returns true if all parts of string is whitespace *)
let is_whitespace_only str = String.for_all (fun x -> x = ' ') str

(* [parse str] returns a command based on user input, and WrongInput if invalid
   input*)
let parse str =
  if String.length str = 0 then WrongInput
  else if is_whitespace_only str = true then WrongInput
  else
    let word_lst = parse_helper (String.split_on_char " ".[0] str) in
    if String.lowercase_ascii (List.hd word_lst) = "quit" then Quit
    else if String.lowercase_ascii (List.hd word_lst) = "create" then
      Create (List.tl word_lst)
    else if String.lowercase_ascii (List.hd word_lst) = "view" then
      View (List.tl word_lst)
    else if String.lowercase_ascii (List.hd word_lst) = "add" then
      Add (List.tl word_lst)
    else if String.lowercase_ascii (List.hd word_lst) = "delete" then
      Delete (List.tl word_lst)
    else if String.lowercase_ascii (List.hd word_lst) = "edit" then
      Edit (List.tl word_lst)
    else if String.lowercase_ascii (List.hd word_lst) = "help" then Help
    else if String.lowercase_ascii (List.hd word_lst) = "test" then Test
    else if String.lowercase_ascii (List.hd word_lst) = "moveon" then MoveOn
    else if String.lowercase_ascii (List.hd word_lst) = "try" then Try
    else if String.lowercase_ascii (List.hd word_lst) = "multiplechoice" then
      MultipleChoice
    else
      let _ =
        ANSITerminal.print_string [ ANSITerminal.red ]
          "Invalid input.\nType help for command list.\n"
      in
      WrongInput

(* Creates folder name as string *)
let rec list_to_string lst =
  match lst with
  | [] -> ""
  | [ h ] -> h
  | h :: t -> h ^ " " ^ list_to_string t

(* [folder_to_json n] returns Yojson readable type for folder *)
let folder_to_json n = `String n

(* [store_to_json string_list] returns a Yojson readable type for all folders
   for folders.json *)
let store_to_json string_list =
  `Assoc [ ("folders", `List (List.map folder_to_json string_list.folders)) ]

(*Folders converted to JSON formated string*)
let to_json s = Yojson.Basic.to_string (store_to_json s)

(* [find_home] lst home returns true if home, of type folder, exists and false
   otherwise *)
let rec find_home lst home =
  match lst with
  | [] -> false
  | h :: t -> if h = home then true else find_home t home

(* [create_folder_json] is a helper function for [create_folder] that creates a
   Yojson json formatted string of the folders that were added *)
let create_folder_json new_folder folders =
  if new_folder <> "" then
    if find_home folders new_folder = false then
      let updated_folders = new_folder :: folders in
      to_json { folders = updated_folders }
    else to_json { folders }
  else to_json { folders }

(* creates a folder and modifies cards.json file with new addition *)
let create_folder (c : command) =
  match c with
  | Create lst ->
      if List.length lst <> 0 then
        let folders = parse_json_file folder_file in
        let new_folder = list_to_string lst in
        if find_home folders new_folder = false then (
          let json_str = create_folder_json new_folder folders in
          Yojson.Basic.to_file folder_file (Yojson.Basic.from_string json_str);
          print_endline "folder added\n")
        else print_endline "This folder already exists."
      else print_endline "Please include a name when creating a folder."
  | _ -> ()

(* helper function for [view_folder]*)
let rec print_folders f : unit =
  match f with
  | [] -> print_endline ""
  | h :: t ->
      let _ = print_endline h in
      print_folders t

(* helper function for printing used in main.ml *)
let rec print_folders_string lst acc =
  match lst with
  | [] -> acc
  | h :: t -> print_folders_string t (acc ^ "\n" ^ h)

(** [get_card_string c home acc] returns a formatted string of the cards in a
    unique folder *)
let rec get_card_string c home acc =
  match c with
  | [] -> acc
  | h :: t ->
      if h.home = home then
        let front = "Front: " ^ h.front ^ "\n" in
        let back = "Back: " ^ h.back ^ "\n\n" in
        get_card_string t home (acc ^ front ^ back)
      else get_card_string t home acc

(** [prints_cards c home] prints all of the card details from a specific home
    folder. If there are no cards in a folder, print that no such folder exists.*)
let print_cards c home : unit =
  let card_str = get_card_string c home "" in
  if card_str = "" then
    print_endline
      "There are no cards in this folder or this folder does not exist.\n"
  else print_endline card_str

(* prints out list of folders or cards associated with a folder in terminal *)
let view_folder (c : command) : unit =
  match c with
  | View [ "folders" ] ->
      let folders = parse_json_file folder_file in
      let _ = print_endline "\n" in
      print_folders folders
  | View [ "cards" ] ->
      print_string "Which folder's cards would you like to view?: ";
      let folder_view = read_line () in
      let cards = parse_json_file_for_card card_file in
      let _ = print_endline "\n" in
      print_cards cards folder_view
  | _ -> print_endline "You can only `view folders` or `view cards`"

(* Format for card informatoin in Yojson json format *)
let card_to_json c =
  `Assoc
    [
      ("front", `String c.front);
      ("back", `String c.back);
      ("home", `String c.home);
    ]

(* Format for entire cards information in Yojson format *)
let store_to_json_card c =
  `Assoc [ ("cards", `List (List.map card_to_json c.cards)) ]

(*Folders converted to JSON formated string*)
let to_json_card s = Yojson.Basic.to_string (store_to_json_card s)

(* Creates JSON formated string including new card information *)
let json_all_new_cards front back home card_file =
  let cards = parse_json_file_for_card card_file in

  let new_card = { front; back; home } in
  let updated_cards = new_card :: cards in
  to_json_card { cards = updated_cards }

(* [add_card_to_json front back home] adds card information to cards.json *)
let add_card_to_json front back home =
  let json_str = json_all_new_cards front back home card_file in
  Yojson.Basic.to_file card_file (Yojson.Basic.from_string json_str);
  print_endline "Card successfully added"

(* [add_card] adds a card to cards.json and relates it to specific folder
   home *)
let add_card (c : command) =
  match c with
  | Add _ ->
      print_string "Type the front of card: ";
      let front_of_card = read_line () in
      print_string "Type the back of card: ";
      let back_of_card = read_line () in
      print_string "Type the folder you want to add a card to: ";
      let folder_home = read_line () in
      let folders = parse_json_file folder_file in
      if find_home folders folder_home = true then
        let cards = parse_json_file_for_card card_file in
        let same_card_front =
          List.filter
            (fun c -> c.front = front_of_card && c.home = folder_home)
            cards
        in
        if List.length same_card_front = 0 then
          add_card_to_json front_of_card back_of_card folder_home
        else print_endline "This card already exists in this folder."
      else
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\n\
           Folder does not exist. Returning back to home. Type 'view folders' \
           to view existing folders and add this card again."
  | _ -> ()

(* filters deleted folder old_folder and returns json string of all other
   folders *)
let delete_folder_json old_folder folders =
  if find_home folders old_folder = true then
    let updated_folders = List.filter (fun n -> n <> old_folder) folders in
    to_json { folders = updated_folders }
  else to_json { folders }

(* [delete_cards_in_folder lst home] deletes all cards associated with home.
   Helper for [delete_something c]*)
let delete_cards_in_folder lst home =
  let updated_cards = List.filter (fun c -> c.home <> home) lst in
  if List.length updated_cards < List.length lst then
    let json_str = to_json_card { cards = updated_cards } in
    Yojson.Basic.to_file card_file (Yojson.Basic.from_string json_str)

(* [delete_something c ] deletes a folder and its repsective cards or a specific
   card*)
let delete_something (c : command) =
  match c with
  | Delete [ "folder" ] ->
      print_string "What is the name of the folder you want to delete? ";
      let folder_name = read_line () in
      ANSITerminal.print_string [ ANSITerminal.red ]
        "All cards in the folder will be deleted, would you like to proceed? \
         yes/no: ";
      let continue = read_line () in
      if continue = "yes" then
        let folders = parse_json_file folder_file in
        let updated_folders = List.filter (fun n -> n <> folder_name) folders in
        if List.length updated_folders < List.length folders then (
          let json_str = to_json { folders = updated_folders } in
          Yojson.Basic.to_file folder_file (Yojson.Basic.from_string json_str);
          let cards = parse_json_file_for_card card_file in
          delete_cards_in_folder cards folder_name;
          print_endline "folder deleted")
        else
          print_endline
            "Folder that you are trying to delete does not exist, try again"
  | Delete [ "card" ] ->
      print_string "What is the front of the card you want to delete? ";
      let front_card = read_line () in
      print_string "Which folder is the card you want to delete in? ";
      let home = read_line () in
      let cards = parse_json_file_for_card card_file in
      let updated_cards =
        List.filter (fun c -> c.front <> front_card || c.home <> home) cards
      in
      if List.length updated_cards < List.length cards then (
        let json_str = to_json_card { cards = updated_cards } in
        Yojson.Basic.to_file card_file (Yojson.Basic.from_string json_str);
        print_endline "card deleted")
      else
        print_endline
          "Card and/or folder that you are trying to delete does not exist, \
           try again"
  | _ ->
      print_endline "You can only use commands `delete card` or `delete folder`"

(* [check_folder_duplicate folder_name folders] checks if a folder name already
   exists *)
let check_folder_duplicate folder_name folders =
  let is_member = List.mem folder_name folders in
  if is_member then true else false

(* [edit_folder new_folder_name folders] renames an existing folder *)
let rec edit_folder old_folder_name (new_folder_name : folder)
    (folders : folder list) (acc : folder list) =
  match folders with
  | [] -> acc
  | folder1 :: t ->
      if folder1 = old_folder_name then (
        print_endline folder1;
        edit_folder old_folder_name new_folder_name t (acc @ [ new_folder_name ]))
      else edit_folder old_folder_name new_folder_name t (acc @ [ folder1 ])

(** [edit_folder_json old_folder_name new_folder_name folders] returns updated
    folders with new names *)
let edit_folder_json old_folder_name new_folder_name folders =
  let updated_folders =
    List.fold_left
      (fun acc n ->
        if n = old_folder_name then acc @ [ new_folder_name ] else acc @ [ n ])
      [] folders
  in
  to_json { folders = updated_folders }

(* [make_new_card_list card_lst old_home new_home acc] returns a new list of
   cards with all occurences of old_home replaced with new_home*)
let rec make_new_card_list card_lst old_home new_home acc =
  match card_lst with
  | [] -> acc
  | { front = f; back = b; home = h } :: t ->
      if h = old_home then
        make_new_card_list t old_home new_home
          ({ front = f; back = b; home = new_home } :: acc)
      else
        make_new_card_list t old_home new_home
          ({ front = f; back = b; home = h } :: acc)

(* [make_new_card_list_back cards home front_card new_back acc] is a helper for
   [edit_something c]. Create new card list with card with new back
   information*)
let rec make_new_card_list_back cards home front_card new_back acc =
  match cards with
  | [] -> acc
  | { front = f; back = b; home = h } :: t ->
      if h = home && f = front_card then
        make_new_card_list_back t home front_card new_back
          ({ front = f; back = new_back; home = h } :: acc)
      else
        make_new_card_list_back t home front_card new_back
          ({ front = f; back = b; home = h } :: acc)

(* [edit_card_home card_lst old_home new_home] modifies all cards that have
   old_home to have new_home*)
let edit_card_home card_lst old_home new_home =
  let new_card_lst = make_new_card_list card_lst old_home new_home [] in
  let json_str = to_json_card { cards = new_card_lst } in
  Yojson.Basic.to_file card_file (Yojson.Basic.from_string json_str)

(* [edit_something c] modifies the back of a card or a folder name *)
let edit_something (c : command) =
  match c with
  | Edit [ "folder" ] ->
      print_string "What is the name of the folder you want to rename? ";
      let folder_name = read_line () in

      let folders = parse_json_file folder_file in
      if List.mem folder_name folders then (
        ANSITerminal.print_string [ ANSITerminal.blue ]
          ("What would you like to rename " ^ folder_name ^ " to? ");
        let new_folder_name = read_line () in

        let has_duplicate = check_folder_duplicate new_folder_name folders in
        if has_duplicate then
          ANSITerminal.print_string [ ANSITerminal.red ]
            "\nThis folder name already exists, please try again\n"
        else
          (* Edit folder name with new_folder_name + home in all cards *)
          let new_folders =
            edit_folder folder_name new_folder_name folders []
          in
          let json_str_edited = to_json { folders = new_folders } in
          Yojson.Basic.to_file folder_file
            (Yojson.Basic.from_string json_str_edited);

          let cards = parse_json_file_for_card card_file in
          edit_card_home cards folder_name new_folder_name;
          print_endline
            "Folder renamed, and cards associated with this folder have been \
             reassociated with the new name.\n")
      else
        ANSITerminal.print_string [ ANSITerminal.red ]
          "Folder that you are trying to edit does not exist, try again\n"
  | Edit [ "card" ] ->
      print_string "What is the front of the card you want to edit? ";
      let front_card = read_line () in
      print_string "Which folder is the card you want to edit in? ";
      let home = read_line () in
      let cards = parse_json_file_for_card card_file in
      let folders = parse_json_file folder_file in
      let folder_exists = List.mem home folders in
      if folder_exists then (
        let cards_related =
          List.filter
            (fun { front = f; back = _; home = h } ->
              h = home && f = front_card)
            cards
        in
        if List.length cards_related > 0 then (
          print_string
            "Do you want to edit the back of the card? Type `yes` or `no`. ";
          let edit_back = read_line () in

          if edit_back = "yes" then (
            print_string
              ("What do you want the back of the card " ^ front_card
             ^ " to be? ");
            let new_back = read_line () in
            let new_card_lst =
              make_new_card_list_back cards home front_card new_back []
            in
            let json_str = to_json_card { cards = new_card_lst } in
            let _ =
              Yojson.Basic.to_file card_file (Yojson.Basic.from_string json_str)
            in
            ANSITerminal.print_string [ ANSITerminal.green ]
              ("\nSuccessfully edited the back of the card " ^ front_card
             ^ " to be " ^ new_back))
          else
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\n\
               You can only edit the back of a card. You can only use the \
               command `yes` or `no`. Returning to home."))
      else
        ANSITerminal.print_string [ ANSITerminal.red ]
          "\nThis card or folder does not exist, please try again"
  | _ -> print_endline "You can only use commands `edit card` or `edit folder`"

(* [assoc_cards c home] helper function for [test] and [multi] *)
let rec assoc_cards c home : (string * string) list =
  match c with
  | [] -> []
  | h :: t ->
      if h.home = home then (h.front, h.back) :: assoc_cards t home
      else assoc_cards t home

(* [test_helper1 k v] helper function for [testing_answer grade lst count]*)
let test_helper1 k v =
  print_endline ("\n" ^ k);
  ANSITerminal.print_string [ ANSITerminal.cyan ] "Type your answer: ";
  let answer = read_line () in
  let lower_answer = String.lowercase_ascii answer in
  let lower_v = String.lowercase_ascii v in
  if lower_answer = lower_v then true else false

(* [testing_answer grade lst count] helper for [test] that calls for user input
   based on write or wrong input*)
let rec testing_answer (grade : bool) lst (count : int) =
  if count = 0 then
    let k, v = List.hd lst in
    let result = test_helper1 k v in
    if result then testing_answer result lst (count + 1)
    else testing_answer false lst (count + 1)
  else if not grade then
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Wrong answer. Would you like to moveon/try/quit?"
  else
    ANSITerminal.print_string [ ANSITerminal.green ]
      "Correct! Would you like to moveon/quit? ";
  let x = parse (read_line ()) in

  match x with
  | n -> (
      try
        if n = Quit then
          let _ =
            ANSITerminal.print_string [ ANSITerminal.green ]
              "Good bye! Returning to home page. please press enter \n"
          in
          ()
        else if n = Try then
          let k, v = List.hd lst in
          let result = test_helper1 k v in
          if result then testing_answer result (List.tl lst) (count + 1)
          else testing_answer false lst (count + 1)
        else if n = MoveOn then
          if List.length lst = 1 then (
            print_endline
              "There are no more cards left in this folder, please press enter \
               to return back to the main page!";
            ())
          else
            let remaining = List.tl lst in
            let k, v = List.hd remaining in
            let result = test_helper1 k v in
            testing_answer result remaining (count + 1)
      with Failure _ ->
        let _ = print_endline "try again" in
        testing_answer grade lst (count + 1))

[@@@ocamlformat "disable"]
let test_intro () = ( ANSITerminal.print_string [ ANSITerminal.blue ] 

"\n\n\
\    \n\
\  ██╗    ██╗███████╗██╗      ██████╗ ██████╗ ███╗   ███╗███████╗    ████████╗ ██████╗ \           
\ ██║    ██║██╔════╝██║     ██╔════╝██╔═══██╗████╗ ████║██╔════╝    ╚══██╔══╝██╔═══██╗ \          
\ ██║ █╗ ██║█████╗  ██║     ██║     ██║   ██║██╔████╔██║█████╗         ██║   ██║   ██║ \         
\ ██║███╗██║██╔══╝  ██║     ██║     ██║   ██║██║╚██╔╝██║██╔══╝         ██║   ██║   ██║ \       
\ ╚███╔███╔╝███████╗███████╗╚██████╗╚██████╔╝██║ ╚═╝ ██║███████╗       ██║   ╚██████╔╝ \         
\  ╚══╝╚══╝ ╚══════╝╚══════╝ ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚══════╝       ╚═╝    ╚═════╝  \      
\                                                                                     \        
\  ██████╗ ██╗   ██╗██╗███████╗██╗     ███████╗████████╗    ████████╗███████╗███████╗████████╗██╗\n\
\ ██╔═══██╗██║   ██║██║╚══███╔╝██║     ██╔════╝╚══██╔══╝    ╚══██╔══╝██╔════╝██╔════╝╚══██╔══╝██║\n\
\ ██║   ██║██║   ██║██║  ███╔╝ ██║     █████╗     ██║          ██║   █████╗  ███████╗   ██║   ██║\n\
\ ██║▄▄ ██║██║   ██║██║ ███╔╝  ██║     ██╔══╝     ██║          ██║   ██╔══╝  ╚════██║   ██║   ╚═╝\n\
\ ╚██████╔╝╚██████╔╝██║███████╗███████╗███████╗   ██║          ██║   ███████╗███████║   ██║   ██╗\n\
\  ╚══▀▀═╝  ╚═════╝ ╚═╝╚══════╝╚══════╝╚══════╝   ╚═╝          ╚═╝   ╚══════╝╚══════╝   ╚═╝   ╚═╝\n\
                                                                                          \    ";)
                                                                                          
let multiple_choice_intro () = ( ANSITerminal.print_string [ ANSITerminal.blue ]  "\n
██╗    ██╗███████╗██╗      ██████╗ ██████╗ ███╗   ███╗███████╗    ████████╗ ██████╗                             
██║    ██║██╔════╝██║     ██╔════╝██╔═══██╗████╗ ████║██╔════╝    ╚══██╔══╝██╔═══██╗                            
██║ █╗ ██║█████╗  ██║     ██║     ██║   ██║██╔████╔██║█████╗         ██║   ██║   ██║                            
██║███╗██║██╔══╝  ██║     ██║     ██║   ██║██║╚██╔╝██║██╔══╝         ██║   ██║   ██║                            
╚███╔███╔╝███████╗███████╗╚██████╗╚██████╔╝██║ ╚═╝ ██║███████╗       ██║   ╚██████╔╝                            
 ╚══╝╚══╝ ╚══════╝╚══════╝ ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚══════╝       ╚═╝    ╚═════╝                             
                                                                                                                
███╗   ███╗██╗   ██╗██╗  ████████╗██╗██████╗ ██╗     ███████╗    
████╗ ████║██║   ██║██║  ╚══██╔══╝██║██╔══██╗██║     ██╔════╝    
██╔████╔██║██║   ██║██║     ██║   ██║██████╔╝██║     █████╗      
██║╚██╔╝██║██║   ██║██║     ██║   ██║██╔═══╝ ██║     ██╔══╝     
██║ ╚═╝ ██║╚██████╔╝███████╗██║   ██║██║     ███████╗███████╗   
╚═╝     ╚═╝ ╚═════╝ ╚══════╝╚═╝   ╚═╝╚═╝     ╚══════╝╚══════╝  
          
 ██████╗██╗  ██╗ ██████╗ ██╗ ██████╗███████╗██╗
██╔════╝██║  ██║██╔═══██╗██║██╔════╝██╔════╝██║
██║     ███████║██║   ██║██║██║     █████╗  ██║
██║     ██╔══██║██║   ██║██║██║     ██╔══╝  ╚═╝
╚██████╗██║  ██║╚██████╔╝██║╚██████╗███████╗██╗
 ╚═════╝╚═╝  ╚═╝ ╚═════╝ ╚═╝ ╚═════╝╚══════╝╚═╝
"; )
[@@@ocamlformat "enable"]

let rec test () =
  print_string
    "\nWhat is the name of the folder you would like to be tested on? ";
  let folder_name = read_line () in
  let folders = parse_json_file folder_file in
  if not (List.mem folder_name folders) then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\n\
       Folder does not exist, below are the folders you have created. Please \
       try again";
    view_folder (View [ "folders" ]);
    test ())
  else
    let cards = parse_json_file_for_card card_file in
    let assoc_list = assoc_cards cards folder_name in
    if List.length assoc_list = 0 then (
      ANSITerminal.print_string [ ANSITerminal.red ]
        "\nFolder empty, please try another folder.";
      test ())
    else testing_answer false assoc_list 0

let help () =
  print_string
    "Commands you can use: \n\n\
    \     - `view`+ `folders` or `cards` : to view the contents of cards or \
     folders\n\n\
    \     - `create`+ name of folder : to create a folder\n\n\
    \     - `add` : to add a card to a folder\n\n\
    \     - `delete`+ `folder` or `card` : to delete a card or folder\n\n\
    \     - `help` : to view a list of commands\n\n\
    \     - `quit` : to exit the program\n\n\
    \     - `test` : to test your knowledge of flashcards in a particular \
     folder\n\n\
    \     - `multiplechoice` : to test your knowledge of flashcards using \
     multiple choice test     \n\n\
    \     - `edit` + `folder` or `card` : to edit the name of a folder or a \
     card's back \n"

(* Returns either "A", "B", or "C" and re-prompts user to type in inputs until
   one of the three is inputed. *)
let rec read_valid_answer () =
  let str = String.uppercase_ascii (read_line ()) in
  match str with
  | "A" | "B" | "C" -> str
  | _ ->
      print_endline "Not a valid answer. Please type A, B, or C.";
      read_valid_answer ()

(* Returns two different indices from a list [lst] that do not equal the index
   [idx]. *)
let rec grab_different_index lst idx =
  let index1 = Random.int (List.length lst) in
  let index2 = Random.int (List.length lst) in
  if index1 <> index2 && index1 <> idx && index2 <> idx then (index1, index2)
  else grab_different_index lst idx

(* Returns Some index of key in association list or None if key does not exist
   in list [lst]*)
let rec get_index key lst count =
  match lst with
  | [] -> failwith "Key does not exist in list"
  | h :: t -> if h = key then count else get_index key t (count + 1)

(* [test_helper2 k v lst] helper function for test_multi. Prints out
   multiplechoice exam question and reacts to user input*)
let test_helper2 k v lst =
  print_endline "\n";
  print_endline k;
  print_endline "\n";
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "Please choose one of the following that matches the card above: \n";
  let correct_ans_index = Random.int 3 in
  if List.length lst = 1 then
    match correct_ans_index with
    | 0 ->
        print_endline ("A: " ^ v);
        print_endline "B: I dunno";
        print_endline "C: I rly dunno";
        let x = read_valid_answer () in
        if x = "A" then true else false
    | 1 ->
        print_endline "A: I dunno";
        print_endline ("B: " ^ v);
        print_endline "C: I rly dunno";
        let x = read_valid_answer () in
        if x = "B" then true else false
    | 2 ->
        print_endline "A: I dunno";
        print_endline "B: I rly dunno";
        print_endline ("C: " ^ v);
        let x = read_valid_answer () in
        if x = "C" then true else false
    | _ -> false
  else if List.length lst = 2 then
    match correct_ans_index with
    | 0 ->
        print_endline ("A: " ^ v);
        print_endline ("B: " ^ snd (List.nth lst 1));
        print_endline "C: I rly dunno";
        let x = read_valid_answer () in
        if x = "A" then true else false
    | 1 ->
        print_endline ("A: " ^ snd (List.nth lst 1));
        print_endline ("B: " ^ v);
        print_endline "C: I rly dunno";
        let x = read_valid_answer () in
        if x = "B" then true else false
    | 2 ->
        print_endline ("A: " ^ snd (List.nth lst 1));
        print_endline "B: I rly dunno";
        print_endline ("C: " ^ v);
        let x = read_valid_answer () in
        if x = "C" then true else false
    | _ -> false
  else
    let keys, _ = List.split lst in
    let idx = get_index k keys 0 in
    let answers_index, answers_index2 = grab_different_index lst idx in
    let correct_ans_index = Random.int 3 in
    match correct_ans_index with
    | 0 ->
        print_endline ("A: " ^ v);
        print_endline ("B: " ^ snd (List.nth lst answers_index));
        print_endline ("C: " ^ snd (List.nth lst answers_index2));
        let x = read_valid_answer () in
        if x = "A" then true else false
    | 1 ->
        print_endline ("A: " ^ snd (List.nth lst answers_index));
        print_endline ("B: " ^ v);
        print_endline ("C: " ^ snd (List.nth lst answers_index2));
        let x = read_valid_answer () in
        if x = "B" then true else false
    | 2 ->
        print_endline ("A: " ^ snd (List.nth lst answers_index));
        print_endline ("B: " ^ snd (List.nth lst answers_index2));
        print_endline ("C: " ^ v);
        let x = read_valid_answer () in
        if x = "C" then true else false
    | _ -> false

(* [test_multi grade lst count og_list] helper function for [multi]*)
let rec test_multi (grade : bool) lst (count : int) og_list =
  if lst = [] then ()
  else if count = 0 then
    let k, v = List.hd lst in
    let result = test_helper2 k v og_list in
    if result then test_multi result lst (count + 1) og_list
    else test_multi false lst (count + 1) og_list
  else if not grade then
    ANSITerminal.print_string [ ANSITerminal.red ]
      "Wrong answer. Would you like to moveon/try/quit? "
  else
    ANSITerminal.print_string [ ANSITerminal.green ]
      "Correct! Do you wish to moveon/quit? ";
  let x = parse (read_line ()) in
  match x with
  | n -> (
      try
        if n = Quit then
          let _ =
            ANSITerminal.print_string [ ANSITerminal.green ]
              "Good bye! Returning to home page. please press enter \n"
          in
          ()
        else if n = Try then
          let k, v = List.hd lst in
          let result = test_helper2 k v og_list in
          if result then test_multi result (List.tl lst) (count + 1) og_list
          else test_multi false lst (count + 1) og_list
        else if n = MoveOn then
          if List.length lst = 1 then (
            print_endline
              "There are no more cards left in this folder, please press enter \
               to return back to the main page!";
            ();
            test_multi false [] 0 og_list)
          else
            let remaining = List.tl lst in
            let k, v = List.hd remaining in
            let result = test_helper2 k v og_list in
            test_multi result remaining (count + 1) og_list
      with Failure _ ->
        let _ = print_endline "invalid command, try again" in
        test_multi grade lst (count + 1) og_list)

let rec multi () =
  print_string
    "\nWhat is the name of the folder you would like to be tested on? \n";
  let folder_name = read_line () in
  let folders = parse_json_file folder_file in
  if not (List.mem folder_name folders) then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\n\
      \ Folder does not exist, below are the folders you have created. Please \
       try again";
    view_folder (View [ "folders" ]);
    multi ())
  else
    let cards = parse_json_file_for_card card_file in
    let assoc_list = assoc_cards cards folder_name in
    if List.length assoc_list = 0 then (
      print_endline "Folder empty, please try another folder.\n";
      multi ())
    else test_multi false assoc_list 0 assoc_list
