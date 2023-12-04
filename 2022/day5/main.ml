(*Potato stack*)
open Str
type 'a stack = Elem of 'a * 'a stack | Empty

let new_stack () = Empty

let push x stack = Elem (x, stack)

let pop = function
    | Elem (x, xs) -> Some (x, xs)
    | Empty -> None

let peek = function
  | Elem (x, _) -> Some x
  | Empty -> None

let rec stack_of_char_to_string = function
    | Elem(a, st) -> Printf.sprintf "%c" a ^ stack_of_char_to_string st
    | Empty -> ""


let read_line_aux () =
    try (Some (read_line ())) with
    | End_of_file -> None

(*Index is the key*)
module Rearrangement = Map.Make(Int);;

let rearrangement_of_char_to_string rearregement = 
    Rearrangement.fold (fun i v res_string ->
        res_string ^ Printf.sprintf "\n%d %s" i  (stack_of_char_to_string v)
    ) rearregement ""

let add_char_to_rearrangement c pos rearrangement = 
    Rearrangement.update pos ( function
        | Some a -> Some (push c a)
        | None -> Some (new_stack () |> push c)
    ) rearrangement

let init_rearrarrangement_pos pos rearrangement = 
    Rearrangement.add pos Empty rearrangement

let pop_rearrarrangement_pos pos rearrangement = 
    let (popped, updated_stack) = 
        (match Rearrangement.find_opt pos rearrangement with
        | Some (Elem(popped, stc)) -> popped, stc
        | Some (Empty) -> Failure "Empty stack" |> raise
        | None -> Failure "Empty stack" |> raise
    ) in
    let (popped, final_rear) = (popped, Rearrangement.update pos ( function
        | Some a -> Some updated_stack
        | None -> assert false
    ) rearrangement) in
    (popped, final_rear)


let push_rearrarrangement_pos v pos rearrangement = 
    (Rearrangement.update pos ( function
        | Some a -> Some (push v a)
        | None -> assert false
    ) rearrangement)


let read_arregement () =
    (*Starting from 2, every 4 chars should be a letter*)
    let letter_pos_to_stack_number pos = 
         ((pos - 2)/4 )+ 1
    in
    let rec read_til_blank buff =
        match read_line_aux () with
        | Some "" -> buff
        | Some a -> read_til_blank (a :: buff)
        | None -> End_of_file |> raise
    in
    let count_stacks stack_line =
        let spaces = (stack_line 
        |> String.trim 
        |> String.fold_left (fun init a -> match a with | ' ' -> init + 1 | _ -> init) 0) in
        spaces mod 3
    in
    let rec init_stacks = function
        | 0 ->  Rearrangement.empty
        | i when i < 0 -> Rearrangement.empty
        | i -> init_rearrarrangement_pos (i) (init_stacks (i-1))

    in
    let push_line_to_stacks line rear =
        let line_length = String.length line in
        let rec aux i rear_temp = 
            if i < line_length then
                match String.get line i with
                | '[' | ']' | ' ' -> aux (i + 1) rear_temp
                | a -> aux (i + 1) (add_char_to_rearrangement a (letter_pos_to_stack_number (i+1)) rear_temp)
            else
                rear_temp
        in
        aux 0 rear
    in
    let buff = read_til_blank [] in
    let inited_rearragement = init_stacks (count_stacks (List.hd buff)) in
    List.fold_left (fun rear line -> push_line_to_stacks line rear) (inited_rearragement) (List.tl buff)

let execute_moves rear =
    let parse_move line = 
        match String.split_on_char ' ' (String.trim line) with
        | "move" :: amount :: "from" :: source :: "to" :: destination :: []-> (int_of_string amount, int_of_string source, int_of_string destination)
        | _ -> Failure "Invalid move" |> raise
    in
    let rec execute_move amount source destination rear =
        if amount > 0 then
            let (popped, new_rear) = pop_rearrarrangement_pos source rear in
            execute_move (amount - 1) source destination (push_rearrarrangement_pos popped destination new_rear)
        else
            rear
    in
    let rec read_til_eof rear =
        match read_line_aux () with
        | None -> rear
        | Some a -> let (amount, source, destination) = parse_move a in
                    read_til_eof (execute_move amount source destination rear)
    in
    read_til_eof rear
            

let () = let rear = ( read_arregement () |> execute_moves) in 
        Rearrangement.iter (fun _ st -> match peek st with |Some a -> a |> Printf.printf "%c" | None -> assert false) rear;
