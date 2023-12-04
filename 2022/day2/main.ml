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


let me_to_play = function
      "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissor
    |_ -> raise (Failure "Invalid move")


let calculate_outcome opp me =
    match opp, me with
    | Rock, Rock | Paper, Paper | Scissor, Scissor -> Draw
    | Rock, Paper | Paper, Scissor | Scissor, Rock -> Win
    | Paper, Rock | Scissor, Paper | Rock, Scissor -> Lost

let calculate_points pl outcom = 
    let pl_points = (match pl with | Rock -> 1 | Paper -> 2 | Scissor -> 3) in
    let outcom_points = (match outcom with | Lost -> 0 | Draw -> 3 | Win -> 6) in
    pl_points + outcom_points


let calculate_round_points opp_play_str me_play_str =
    let opp_play = opponent_to_play opp_play_str in
    let me_play = me_to_play me_play_str in
    let outcome = calculate_outcome opp_play me_play in
    calculate_points me_play outcome 

let rec read_rounds_points () = 
    match read_line_aux () with
    | None -> 0
    | Some a -> let (opp_play_str, me_play_str) = 
                (match String.split_on_char ' ' a with 
                | a::b::_ -> (a,b)
                | _ -> raise (Failure "Wrong input")
                )  in
                (calculate_round_points opp_play_str me_play_str) + read_rounds_points () 


let () = 
    read_rounds_points () |> print_int 
                    
