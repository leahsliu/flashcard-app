open Cards

(** [start_quizlet valid_prompt] prompts user to type in commands to run program *)
let rec start_quizlet (valid_prompt : bool) : unit =
  if not valid_prompt then
    ANSITerminal.print_string [ ANSITerminal.magenta ] "\nType in a command: "
  else
    ANSITerminal.print_string [ ANSITerminal.magenta ] "\nTry another command: ";

  let x = Cards.parse (read_line ()) in
  match x with
  | View lst ->
      let _ = Cards.view_folder (View lst) in
      if valid_prompt then start_quizlet false else start_quizlet true
  | Create lst ->
      let _ = Cards.create_folder (Create lst) in
      if valid_prompt then start_quizlet false else start_quizlet true
  | WrongInput ->
      let _ = ANSITerminal.print_string [ ANSITerminal.green ] "Failed\n" in
      if valid_prompt then start_quizlet false else start_quizlet true
  | Add lst ->
      let _ = Cards.add_card (Add lst) in
      if valid_prompt then start_quizlet false else start_quizlet true
  | Delete lst ->
      let _ = Cards.delete_something (Delete lst) in
      if valid_prompt then start_quizlet false else start_quizlet true
  | Help ->
      let _ = Cards.help () in
      if valid_prompt then start_quizlet false else start_quizlet true
  | Quit ->
      let _ = ANSITerminal.print_string [ ANSITerminal.green ] "Good bye!\n" in
      exit 0
  | Test ->
      Cards.test_intro ();
      let _ = Cards.test () in
      if valid_prompt then start_quizlet false else start_quizlet true
  | MultipleChoice ->
      Cards.multiple_choice_intro ();
      let _ = Cards.multi () in
      if valid_prompt then start_quizlet false else start_quizlet true
  | Edit lst ->
      let _ = Cards.edit_something (Edit lst) in
      if valid_prompt then start_quizlet false else start_quizlet true
  | _ -> if valid_prompt then start_quizlet false else start_quizlet true

[@@@ocamlformat "disable"]


(** [main ()] starts the quizlet app *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "
    ██╗    ██╗███████╗██╗      ██████╗ ██████╗ ███╗   ███╗███████╗    ████████╗ ██████╗     
    ██║    ██║██╔════╝██║     ██╔════╝██╔═══██╗████╗ ████║██╔════╝    ╚══██╔══╝██╔═══██╗    
    ██║ █╗ ██║█████╗  ██║     ██║     ██║   ██║██╔████╔██║█████╗         ██║   ██║   ██║    
    ██║███╗██║██╔══╝  ██║     ██║     ██║   ██║██║╚██╔╝██║██╔══╝         ██║   ██║   ██║    
    ╚███╔███╔╝███████╗███████╗╚██████╗╚██████╔╝██║ ╚═╝ ██║███████╗       ██║   ╚██████╔╝    
     ╚══╝╚══╝ ╚══════╝╚══════╝ ╚═════╝ ╚═════╝ ╚═╝     ╚═╝╚══════╝       ╚═╝    ╚═════╝     
                                                                                            
     ██████╗ ██╗   ██╗██╗███████╗██╗     ███████╗████████╗██╗                               
    ██╔═══██╗██║   ██║██║╚══███╔╝██║     ██╔════╝╚══██╔══╝██║                               
    ██║   ██║██║   ██║██║  ███╔╝ ██║     █████╗     ██║   ██║                               
    ██║▄▄ ██║██║   ██║██║ ███╔╝  ██║     ██╔══╝     ██║   ╚═╝                               
    ╚██████╔╝╚██████╔╝██║███████╗███████╗███████╗   ██║   ██╗                               
     ╚══▀▀═╝  ╚═════╝ ╚═╝╚══════╝╚══════╝╚══════╝   ╚═╝   ╚═╝                               
                                                              ";
  start_quizlet false
[@@@ocamlformat "enable"]

(* Execute the quizlet program. *)
let () = main ()
