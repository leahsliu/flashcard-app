open OUnit2
(** Test plan: Firstly, commands were tested with OUnit, where the parse
    function in the cards module is used to identify the commands. Corner cases
    of commands are tested as well. The tests for the commands in our program is
    called parse_test. Next, there were OUnit tests created for folders and
    cards, including some functions that were testable. Since our program relies
    hugely on commandline input from the user, it was difficult to create OUnit
    tests. Thus, many functionalities that required a lot of user input were
    tested manually, including conrer cases of those functions. Our function's
    pattern matching allows wrong inputs to be caught, so the program is
    unlikely to fail. The functions of creating, deleting, viewing, and editing
    folders tested with OUnit. How we approached the testing was creating some
    new functions that mirror the original functions were the user input was
    changed to the function variable. We also divided functions from cards.ml
    into smaller functions to test functionality. Thus, we could use OUnit
    testing to make sure those functions were correct. Our OUnit testing
    employed blackbox testing, because we only examined the functionality of the
    functions rather than the internal design. Functions that required too many
    user inputs were tested manually to ensure system correctness. *)

open Cards
open Yojson.Basic.Util

let data_dir_prefix = "data" ^ Filename.dir_sep
let folder_file = data_dir_prefix ^ "folders.json"

let parse_tests =
  [
    ( "parse Quit command" >:: fun _ ->
      assert_equal Cards.Quit (Cards.parse "quit") );
    ( "parse wrong input" >:: fun _ ->
      assert_equal Cards.WrongInput (Cards.parse "") );
    ( "parse wrong input not blank" >:: fun _ ->
      assert_equal Cards.WrongInput (Cards.parse "random input") );
    ( "parse create command" >:: fun _ ->
      assert_equal (Cards.Create [ "thing" ]) (Cards.parse "create thing") );
    ( "parse Quit command different casing" >:: fun _ ->
      assert_equal Cards.Quit (Cards.parse "qUiT") );
    ( "parse Quit command different casing and spaces before " >:: fun _ ->
      assert_equal Cards.Quit (Cards.parse "        qUiT") );
    ( "parse create command different casing" >:: fun _ ->
      assert_equal (Cards.Create [ "ThiNg" ]) (Cards.parse "CreAte ThiNg") );
    ( "parse Add nothing" >:: fun _ ->
      assert_equal (Cards.Add []) (Cards.parse "add") );
    ( "parse Add multiple things" >:: fun _ ->
      assert_equal
        (Cards.Add [ "some"; "thing" ])
        (Cards.parse "add some thing") );
    ( "parse Delete random commands after" >:: fun _ ->
      assert_equal
        (Cards.Delete [ "some"; "thing" ])
        (Cards.parse "delete some thing") );
    ( "parse Delete card" >:: fun _ ->
      assert_equal (Cards.Delete [ "card" ]) (Cards.parse "delete card") );
    ( "parse Delete folder with multiple spaces afterwards" >:: fun _ ->
      assert_equal (Cards.Delete [ "folder" ]) (Cards.parse "delete folder    ")
    );
    ( "parse Delete folder with multiple spaces before real command" >:: fun _ ->
      assert_equal (Cards.Delete [ "folder" ])
        (Cards.parse "       delete folder    ") );
    ( "parse Test multiple spaces before and after; different casing"
    >:: fun _ -> assert_equal Cards.Test (Cards.parse "       TeSt    ") );
    ( "parse test normal casing" >:: fun _ ->
      assert_equal Cards.Test (Cards.parse "test") );
    ( "parse Test multiple spaces before and after; different casing"
    >:: fun _ -> assert_equal Cards.Help (Cards.parse "       HELP    ") );
    ( "parse Test multiple spaces and random word ignored from command"
    >:: fun _ ->
      assert_equal Cards.Help (Cards.parse "       HELP    random stuff after")
    );
    ( "parse test all caps casing" >:: fun _ ->
      assert_equal Cards.Help (Cards.parse "HELP") );
    ( "parse move on (wrong command) 1" >:: fun _ ->
      assert_equal Cards.WrongInput (Cards.parse "move on") );
    ( "parse move on (wrong command) 2" >:: fun _ ->
      assert_equal Cards.WrongInput (Cards.parse "move-on") );
    ( "parse move on right command" >:: fun _ ->
      assert_equal Cards.MoveOn (Cards.parse "moveon") );
    ( "parse move on unidentifable commands at head of list" >:: fun _ ->
      assert_equal Cards.WrongInput
        (Cards.parse "random stuff here but moveon please") );
    ( "parse move on unidentifable commands at tail of list" >:: fun _ ->
      assert_equal Cards.MoveOn
        (Cards.parse "moveon please but random stuff here ") );
    ( "standard try command" >:: fun _ ->
      assert_equal Cards.Try (Cards.parse "try") );
    ( "standard try command with spaces and bad casing" >:: fun _ ->
      assert_equal Cards.Try (Cards.parse " TRY") );
    ( "standard multiplechoice command" >:: fun _ ->
      assert_equal Cards.MultipleChoice (Cards.parse "multiplechoice") );
    ( "multiple choice wrong format 1" >:: fun _ ->
      assert_equal Cards.WrongInput (Cards.parse "multiple-choice") );
    ( "multiple choice wrong format 2" >:: fun _ ->
      assert_equal Cards.WrongInput (Cards.parse "multiple choice") );
    ( "multiple choice verbose command, but valid input with spaces before \
       command"
    >:: fun _ ->
      assert_equal Cards.MultipleChoice
        (Cards.parse "    multiplechoice game start now add card testing") );
    ( "multiple choice verbose command, and invalid input with spaces before \
       command"
    >:: fun _ ->
      assert_equal Cards.WrongInput
        (Cards.parse "    multiple-choice game start now add card testing") );
    ( "unicode detected as wrong input" >:: fun _ ->
      assert_equal Cards.WrongInput (Cards.parse "测试") );
    ( "unicode detected as wrong input with spaces first" >:: fun _ ->
      assert_equal Cards.WrongInput (Cards.parse "  测试") );
    ( "normal try command with unicode after it" >:: fun _ ->
      assert_equal Cards.Try (Cards.parse "try 测试") );
    ( "normal try command with unicode before it" >:: fun _ ->
      assert_equal Cards.WrongInput (Cards.parse "测试 try ") );
  ]

(* [folder f] helper for [parse_json_file filename] to convert Yosjon.t to
   folder type*)
let folder f = to_string f

(* [parse_json_file filename] returns a folder list by reading json file *)
let parse_json_file (filename : string) : string list =
  let json = Yojson.Basic.from_file filename in
  json |> member "folders" |> to_list |> List.map folder

(* [create_folder_test name folder_name command expected_result] creates a Ounit
   test [name] for Cards.create_folder by checking if new folder exists in JSON
   file*)
let create_folder_test (name : string) (folder_name : string)
    (command : command) expected_result : test =
  name >:: fun _ ->
  Cards.create_folder command;

  assert_equal expected_result
    (List.mem folder_name (parse_json_file folder_file))

let folders = [ "home1"; "home2"; "home3" ]
let folders1 = [ "home1"; "home2"; "home3"; "测" ]

let test_find_home =
  [
    ( "Home should be found" >:: fun _ ->
      assert_equal true (Cards.find_home folders "home2") );
    ( "Home should be not found" >:: fun _ ->
      assert_equal false (Cards.find_home folders "home5") );
    ( "find_home spaces before real home" >:: fun _ ->
      assert_equal false (Cards.find_home folders "  home1") );
    ( "Home should be not found; find_home spaces after real home" >:: fun _ ->
      assert_equal false (Cards.find_home folders "home1  ") );
    ( "find_home unicode" >:: fun _ ->
      assert_equal true (Cards.find_home folders1 "测") );
    ( "find_home unicode with spaces before name" >:: fun _ ->
      assert_equal false (Cards.find_home folders1 " 测") );
    ( "find_home unicode with spaces after name" >:: fun _ ->
      assert_equal false (Cards.find_home folders1 "测   ") );
  ]

let folder_tests =
  [
    ( "delete folder '测' exists; special unicode characterss" >:: fun _ ->
      assert_equal
        "{\"folders\":[\"test-folder-1\",\"home1\",\"home2\",\"home3\"]}"
        (Cards.delete_folder_json "测"
           [ "测"; "test-folder-1"; "home1"; "home2"; "home3" ]) );
    ( "delete folder '测' does not exist" >:: fun _ ->
      assert_equal
        "{\"folders\":[\"test-folder-1\",\"home1\",\"home2\",\"home3\"]}"
        (Cards.delete_folder_json "测"
           [ "test-folder-1"; "home1"; "home2"; "home3" ]) );
    ( "delete folder 'test-folder-1'" >:: fun _ ->
      assert_equal "{\"folders\":[\"home1\",\"home2\",\"home3\"]}"
        (Cards.delete_folder_json "test-folder-1"
           [ "test-folder-1"; "home1"; "home2"; "home3" ]) );
    ( "delete folder, folder doesnt exist" >:: fun _ ->
      assert_equal
        "{\"folders\":[\"test-folder-1\",\"home1\",\"home2\",\"home3\"]}"
        (Cards.delete_folder_json "home4"
           [ "test-folder-1"; "home1"; "home2"; "home3" ]) );
    ( "delete folder, folder empty name" >:: fun _ ->
      assert_equal
        "{\"folders\":[\"test-folder-1\",\"home1\",\"home2\",\"home3\"]}"
        (Cards.delete_folder_json ""
           [ "test-folder-1"; "home1"; "home2"; "home3" ]) );
    ( "delete folder, empty folder list" >:: fun _ ->
      assert_equal "{\"folders\":[]}"
        (Cards.delete_folder_json "test-folder-1" []) );
    ( "Delete the first folder in a list" >:: fun _ ->
      assert_equal "{\"folders\":[\"home2\",\"home3\"]}"
        (Cards.delete_folder_json "home1" [ "home1"; "home2"; "home3" ]) );
    ( "Delete the last folder in a list" >:: fun _ ->
      assert_equal "{\"folders\":[\"home1\",\"home2\"]}"
        (Cards.delete_folder_json "home3" [ "home1"; "home2"; "home3" ]) );
    ( "Delete a folder from a list with only one folder" >:: fun _ ->
      assert_equal "{\"folders\":[]}"
        (Cards.delete_folder_json "home1" [ "home1" ]) );
    ( "Delete a folder with special characters in the name" >:: fun _ ->
      assert_equal "{\"folders\":[\"home1\",\"home2\"]}"
        (Cards.delete_folder_json "special@folder"
           [ "home1"; "special@folder"; "home2" ]) );
    ( "Delete a folder with leading or trailing spaces in the name" >:: fun _ ->
      assert_equal "{\"folders\":[\"home1\",\"home2\"]}"
        (Cards.delete_folder_json "  home3  " [ "home1"; "  home3  "; "home2" ])
    );
    create_folder_test "create test folder" "test_folder"
      (Create [ "test_folder" ]) true;
    create_folder_test "folder does not exist" "ranDom"
      (Create [ "test_folder" ]) false;
    ( "Create folder 'test-folder-1'" >:: fun _ ->
      assert_equal
        "{\"folders\":[\"test-folder-1\",\"home1\",\"home2\",\"home3\"]}"
        (Cards.create_folder_json "test-folder-1" folders) );
    ( "Create folder 'test-folder-1' from empty json list of folders"
    >:: fun _ ->
      assert_equal "{\"folders\":[\"test-folder-1\"]}"
        (Cards.create_folder_json "test-folder-1" []) );
    ( "Create folder 'test-folder-1' with leading spaces in front" >:: fun _ ->
      assert_equal "{\"folders\":[\"  test-folder-1\"]}"
        (Cards.create_folder_json "  test-folder-1" []) );
    ( "Create folder 'test-folder-1' with leading spaces in back; no deleting \
       white space in this section of code"
    >:: fun _ ->
      assert_equal "{\"folders\":[\"test-folder-1  \"]}"
        (Cards.create_folder_json "test-folder-1  " []) );
    ( "Add a folder with an existing name, dont allow" >:: fun _ ->
      let existing_folders = [ "home1"; "test-folder-1"; "home2" ] in
      assert_equal "{\"folders\":[\"home1\",\"test-folder-1\",\"home2\"]}"
        (Cards.create_folder_json "test-folder-1" existing_folders) );
    ( "Add a folder with no name, dont allow" >:: fun _ ->
      let existing_folders = [ "home1"; "test-folder-1"; "home2" ] in
      assert_equal "{\"folders\":[\"home1\",\"test-folder-1\",\"home2\"]}"
        (Cards.create_folder_json "" existing_folders) );
    ( "Add a folder with unicode characters in the name" >:: fun _ ->
      assert_equal "{\"folders\":[\"测试\",\"home1\",\"home2\"]}"
        (Cards.create_folder_json "测试" [ "home1"; "home2" ]) );
    ( "View folders" >:: fun _ ->
      let folders : string list = [ "home1"; "home2" ] in
      assert_equal
        ("\n" ^ List.nth folders 0 ^ "\n" ^ List.nth folders 1)
        (Cards.print_folders_string folders "") );
    ( "View folders one folder" >:: fun _ ->
      let folders : string list = [ "home1" ] in
      assert_equal
        ("\n" ^ List.nth folders 0)
        (Cards.print_folders_string folders "") );
    ( "View folders no folder" >:: fun _ ->
      let folders : string list = [] in
      assert_equal "" (Cards.print_folders_string folders "") );
    ( "View folders with leading and trailing spaces in names" >:: fun _ ->
      let folders : string list = [ "  home1  "; "  home2  " ] in
      assert_equal
        ("\n" ^ List.nth folders 0 ^ "\n" ^ List.nth folders 1)
        (Cards.print_folders_string folders "") );
    ( "View folders with special characters in names" >:: fun _ ->
      let folders : string list = [ "home@1"; "home#2" ] in
      assert_equal
        ("\n" ^ List.nth folders 0 ^ "\n" ^ List.nth folders 1)
        (Cards.print_folders_string folders "") );
    ( "View folders with empty string names" >:: fun _ ->
      let folders : string list = [ ""; "" ] in
      assert_equal
        ("\n" ^ List.nth folders 0 ^ "\n" ^ List.nth folders 1)
        (Cards.print_folders_string folders "") );
    ( "View folders with unicode characters in names" >:: fun _ ->
      let folders : string list = [ "家"; "事务" ] in
      assert_equal
        ("\n" ^ List.nth folders 0 ^ "\n" ^ List.nth folders 1)
        (Cards.print_folders_string folders "") );
    ( "edit folder name" >:: fun _ ->
      let existing_folders = [ "home1"; "test-folder-1"; "home2" ] in
      assert_equal "{\"folders\":[\"home01\",\"test-folder-1\",\"home2\"]}"
        (Cards.edit_folder_json "home1" "home01" existing_folders) );
    ( "edit folder name one folder" >:: fun _ ->
      let existing_folders = [ "home1" ] in
      assert_equal "{\"folders\":[\"home01\"]}"
        (Cards.edit_folder_json "home1" "home01" existing_folders) );
    ( "edit folder name no folders" >:: fun _ ->
      let existing_folders = [] in
      assert_equal "{\"folders\":[]}"
        (Cards.edit_folder_json "home1" "home01" existing_folders) );
    ( "edit folder name not first one" >:: fun _ ->
      let existing_folders = [ "home1"; "test-folder-1"; "home2" ] in
      assert_equal "{\"folders\":[\"home1\",\"test-folder-1\",\"home02\"]}"
        (Cards.edit_folder_json "home2" "home02" existing_folders) );
    ( "edit folder special unicode words changed to standard English characters"
    >:: fun _ ->
      let existing_folders = [ "home1"; "test-folder-1"; "home2"; "事务" ] in
      assert_equal
        "{\"folders\":[\"home1\",\"test-folder-1\",\"home2\",\"hi\"]}"
        (Cards.edit_folder_json "事务" "hi" existing_folders) );
    ( "edit folder special unicode words changed to other special unicode \
       characters"
    >:: fun _ ->
      let existing_folders = [ "home1"; "test-folder-1"; "home2"; "事务" ] in
      assert_equal
        "{\"folders\":[\"home1\",\"test-folder-1\",\"home2\",\"家事\"]}"
        (Cards.edit_folder_json "事务" "家事" existing_folders) );
    ( "edit folder name that originally has spaces in it" >:: fun _ ->
      let existing_folders =
        [ "home1 has many spaces"; "test-folder-1"; "home2"; "事务" ]
      in
      assert_equal
        "{\"folders\":[\"home1\",\"test-folder-1\",\"home2\",\"事务\"]}"
        (Cards.edit_folder_json "home1 has many spaces" "home1" existing_folders)
    );
    ( "edit folder name to one that has spaces in it" >:: fun _ ->
      let existing_folders = [ "home1"; "test-folder-1"; "home2"; "事务" ] in
      assert_equal
        "{\"folders\":[\"home1 has many \
         spaces\",\"test-folder-1\",\"home2\",\"事务\"]}"
        (Cards.edit_folder_json "home1" "home1 has many spaces" existing_folders)
    );
  ]

let card_tests =
  [
    ( "Create identical card (should work because this part of code does not \
       check for duplicates)"
    >:: fun _ ->
      assert_equal
        "{\"cards\":[{\"front\":\"statler\",\"back\":\"hotel\",\"home\":\"cornell\"},{\"front\":\"statler\",\"back\":\"hotel\",\"home\":\"cornell\"}]}"
        (Cards.json_all_new_cards "statler" "hotel" "cornell"
           (data_dir_prefix ^ "mock_cards.json")) );
    ( "Create different card" >:: fun _ ->
      assert_equal
        "{\"cards\":[{\"front\":\"random\",\"back\":\"card\",\"home\":\"randomFolder\"},{\"front\":\"statler\",\"back\":\"hotel\",\"home\":\"cornell\"}]}"
        (Cards.json_all_new_cards "random" "card" "randomFolder"
           (data_dir_prefix ^ "mock_cards.json")) );
    ( "Create card with spaces before name" >:: fun _ ->
      assert_equal
        "{\"cards\":[{\"front\":\"  \
         cardName\",\"back\":\"card\",\"home\":\"randomFolder\"},{\"front\":\"statler\",\"back\":\"hotel\",\"home\":\"cornell\"}]}"
        (Cards.json_all_new_cards "  cardName" "card" "randomFolder"
           (data_dir_prefix ^ "mock_cards.json")) );
    ( "Create card with unicode characters" >:: fun _ ->
      assert_equal
        "{\"cards\":[{\"front\":\"测试\",\"back\":\"字符\",\"home\":\"测试夹\"},{\"front\":\"statler\",\"back\":\"hotel\",\"home\":\"cornell\"}]}"
        (Cards.json_all_new_cards "测试" "字符" "测试夹"
           (data_dir_prefix ^ "mock_cards.json")) );
    ( "Create card with numeric values" >:: fun _ ->
      assert_equal
        "{\"cards\":[{\"front\":\"123\",\"back\":\"456\",\"home\":\"789\"},{\"front\":\"statler\",\"back\":\"hotel\",\"home\":\"cornell\"}]}"
        (Cards.json_all_new_cards "123" "456" "789"
           (data_dir_prefix ^ "mock_cards.json")) );
    ( "Create card with empty strings" >:: fun _ ->
      assert_equal
        "{\"cards\":[{\"front\":\"\",\"back\":\"\",\"home\":\"\"},{\"front\":\"statler\",\"back\":\"hotel\",\"home\":\"cornell\"}]}"
        (Cards.json_all_new_cards "" "" ""
           (data_dir_prefix ^ "mock_cards.json")) );
    ( "make new card list with old_home exists" >:: fun _ ->
      assert_equal
        [
          { front = "ho"; back = "plaza"; home = "another-home" };
          { front = "apple"; back = "tree"; home = "new-home" };
          { front = "statler"; back = "hotel"; home = "new-home" };
        ]
        (Cards.make_new_card_list
           [
             { front = "statler"; back = "hotel"; home = "test" };
             { front = "apple"; back = "tree"; home = "test" };
             { front = "ho"; back = "plaza"; home = "another-home" };
           ]
           "test" "new-home" []) );
    ( "make new card list with old_home does not exist" >:: fun _ ->
      assert_equal
        [
          { front = "ho"; back = "plaza"; home = "another-home" };
          { front = "apple"; back = "tree"; home = "test" };
          { front = "statler"; back = "hotel"; home = "test" };
        ]
        (Cards.make_new_card_list
           [
             { front = "statler"; back = "hotel"; home = "test" };
             { front = "apple"; back = "tree"; home = "test" };
             { front = "ho"; back = "plaza"; home = "another-home" };
           ]
           "random-home" "new-home" []) );
    ( "make new card list with old_home empty string" >:: fun _ ->
      assert_equal
        [
          { front = "ho"; back = "plaza"; home = "another-home" };
          { front = "apple"; back = "tree"; home = "" };
          { front = "statler"; back = "hotel"; home = "" };
        ]
        (Cards.make_new_card_list
           [
             { front = "statler"; back = "hotel"; home = "test" };
             { front = "apple"; back = "tree"; home = "test" };
             { front = "ho"; back = "plaza"; home = "another-home" };
           ]
           "test" "" []) );
    ( "make new card list with new_home empty string" >:: fun _ ->
      assert_equal
        [
          { front = "ho"; back = "plaza"; home = "another-home" };
          { front = "apple"; back = "tree"; home = "test" };
          { front = "statler"; back = "hotel"; home = "test" };
        ]
        (Cards.make_new_card_list
           [
             { front = "statler"; back = "hotel"; home = "" };
             { front = "apple"; back = "tree"; home = "" };
             { front = "ho"; back = "plaza"; home = "another-home" };
           ]
           "" "test" []) );
    ( "Make new card list edit existing card back" >:: fun _ ->
      assert_equal
        [
          { front = "ho"; back = "plaza"; home = "another-home" };
          { front = "apple"; back = "fruit"; home = "test" };
        ]
        (Cards.make_new_card_list_back
           [
             { front = "apple"; back = "tree"; home = "test" };
             { front = "ho"; back = "plaza"; home = "another-home" };
           ]
           "test" "apple" "fruit" []) );
    ( "Make new card list edit existing card back; multiple card with same home"
    >:: fun _ ->
      assert_equal
        [
          { front = "ho"; back = "plaza"; home = "another-home" };
          { front = "apple"; back = "fruit"; home = "test" };
          { front = "statler"; back = "hotel"; home = "test" };
        ]
        (Cards.make_new_card_list_back
           [
             { front = "statler"; back = "hotel"; home = "test" };
             { front = "apple"; back = "tree"; home = "test" };
             { front = "ho"; back = "plaza"; home = "another-home" };
           ]
           "test" "apple" "fruit" []) );
    ( "Make new card list edit a card back of card that does not exist"
    >:: fun _ ->
      assert_equal
        [
          { front = "ho"; back = "plaza"; home = "another-home" };
          { front = "apple"; back = "tree"; home = "test" };
          { front = "statler"; back = "hotel"; home = "test" };
        ]
        (Cards.make_new_card_list_back
           [
             { front = "statler"; back = "hotel"; home = "test" };
             { front = "apple"; back = "tree"; home = "test" };
             { front = "ho"; back = "plaza"; home = "another-home" };
           ]
           "test" "does not exist" "fruit" []) );
    ( "Make new card list edit a card back special char" >:: fun _ ->
      assert_equal
        [
          { front = "ho"; back = "plaza"; home = "another-home" };
          { front = "apple"; back = "测试"; home = "test" };
          { front = "statler"; back = "hotel"; home = "test" };
        ]
        (Cards.make_new_card_list_back
           [
             { front = "statler"; back = "hotel"; home = "test" };
             { front = "apple"; back = "tree"; home = "test" };
             { front = "ho"; back = "plaza"; home = "another-home" };
           ]
           "test" "apple" "测试" []) );
    ( "Make new card list edit a card back with empty string" >:: fun _ ->
      assert_equal
        [
          { front = "ho"; back = "plaza"; home = "another-home" };
          { front = "apple"; back = ""; home = "test" };
          { front = "statler"; back = "hotel"; home = "test" };
        ]
        (Cards.make_new_card_list_back
           [
             { front = "statler"; back = "hotel"; home = "test" };
             { front = "apple"; back = "tree"; home = "test" };
             { front = "ho"; back = "plaza"; home = "another-home" };
           ]
           "test" "apple" "" []) );
    ( "Make new card list edit a card back detect empty string" >:: fun _ ->
      assert_equal
        [
          { front = ""; back = "bob"; home = "test" };
          { front = "ho"; back = "plaza"; home = "another-home" };
          { front = "apple"; back = "tree"; home = "test" };
          { front = "statler"; back = "hotel"; home = "test" };
        ]
        (Cards.make_new_card_list_back
           [
             { front = "statler"; back = "hotel"; home = "test" };
             { front = "apple"; back = "tree"; home = "test" };
             { front = "ho"; back = "plaza"; home = "another-home" };
             { front = ""; back = "gym"; home = "test" };
           ]
           "test" "" "bob" []) );
    ( "Make new card list edit a card detect apostrophe" >:: fun _ ->
      assert_equal
        [
          { front = "gold's"; back = "best gym"; home = "test" };
          { front = "ho"; back = "plaza"; home = "another-home" };
          { front = "apple"; back = "tree"; home = "test" };
          { front = "statler"; back = "hotel"; home = "test" };
        ]
        (Cards.make_new_card_list_back
           [
             { front = "statler"; back = "hotel"; home = "test" };
             { front = "apple"; back = "tree"; home = "test" };
             { front = "ho"; back = "plaza"; home = "another-home" };
             { front = "gold's"; back = "gym"; home = "test" };
           ]
           "test" "gold's" "best gym" []) );
    ( "Make new card list edit a card add quotation mark in string" >:: fun _ ->
      assert_equal
        [
          { front = "gold's"; back = "best said, \" gym"; home = "test" };
          { front = "ho"; back = "plaza"; home = "another-home" };
          { front = "apple"; back = "tree"; home = "test" };
          { front = "statler"; back = "hotel"; home = "test" };
        ]
        (Cards.make_new_card_list_back
           [
             { front = "statler"; back = "hotel"; home = "test" };
             { front = "apple"; back = "tree"; home = "test" };
             { front = "ho"; back = "plaza"; home = "another-home" };
             { front = "gold's"; back = "gym"; home = "test" };
           ]
           "test" "gold's" "best said, \" gym" []) );
  ]

let test =
  "card tests"
  >::: List.flatten [ parse_tests; folder_tests; test_find_home; card_tests ]

let _ = run_test_tt_main test
