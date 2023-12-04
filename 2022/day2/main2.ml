type play = Rock | Paper | Scissor
type outcome = Lost | Draw | Win


let read_line_aux () =
    try (Some (read_line ())) with
    | End_of_file -> None


let opponent_to_play = function
      "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissor
    |_ -> raise (Failure "Invalid move")


let outcome_from_string = function
      "X" -> Lost
    | "Y" -> Draw
    | "Z" -> Win
    |_ -> raise (Failure "Invalid move")


let calculate_my_play opp outcome =
    match opp, outcome with
    | Scissor, Win | Paper, Lost -> Rock
    | Rock, Win | Scissor, Lost -> Paper
    | Paper, Win | Rock, Lost -> Scissor
    | a, Draw -> a 

let calculate_points pl outcom = 
    let pl_points = (match pl with | Rock -> 1 | Paper -> 2 | Scissor -> 3) in
    let outcom_points = (match outcom with | Lost -> 0 | Draw -> 3 | Win -> 6) in
    pl_points + outcom_points


let calculate_round_points opp_play_str outcome_of_play =
    let opp_play = opponent_to_play opp_play_str in
    let outcome = outcome_from_string outcome_of_play in
    let me_play = calculate_my_play opp_play outcome in
    calculate_points me_play outcome

let rec read_rounds_points () = 
    match read_line_aux () with
    | None -> 0
    | Some a -> let (opp_play_str, outcome_play_str) = 
                (match String.split_on_char ' ' a with 
                | a::b::_ -> (a,b)
                | _ -> raise (Failure "Wrong input")
                )  in
                (calculate_round_points opp_play_str outcome_play_str) + read_rounds_points () 


let () = 
    read_rounds_points () |> print_int 
                    
