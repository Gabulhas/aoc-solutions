type packet = INT of int
            | LIST of packet list

let read_line_aux () =
    try (Some (read_line ())) with
    | End_of_file -> None

let rec packet_to_string = function 
    | INT a -> string_of_int a
    | LIST l -> 
            "[" ^ (l |> List.map packet_to_string |> String.concat ",") ^ "]"
    
let is_digit = function '0' .. '9' -> true | _ -> false
let digit_char_to_int d = d |> Char.escaped |> int_of_string

let rec parse_string l = 
    let (fpacket, _) = consume_list l 0 [] in
    match fpacket with
    | [LIST a]  -> a
    | _ -> assert false
    
and consume_list l pos res: (packet list * int) =
    let ch = String.get l pos in
    match ch with
    | ']' -> (List.rev res, pos + 1)
    | '[' -> 
             let (new_list, new_pos) = consume_list l (pos + 1) [] in
             let new_res = ((LIST new_list) :: res) in
             if new_pos == String.length l then
                (new_res, new_pos)
             else
                consume_list l new_pos new_res
    | ',' -> consume_list l (pos + 1) res
    | _ when is_digit ch -> 
            let (new_int, new_pos) = consume_digits l pos in
            consume_list l new_pos ((INT new_int) :: res)
    | _ -> assert false

and consume_digits l pos =
    let rec aux cpos res_number =
        let ch = String.get l cpos in
        if is_digit ch then
            let new_number = res_number * 10 +  (digit_char_to_int ch) in
            aux (cpos + 1) new_number
        else 
            (res_number, cpos)
    in
    aux pos 0



let rec cmp left right: bool option = 
    match left, right with
    | INT l :: lefttl, INT r :: righttl -> 
                if l < r then Some true
                else if l > r then Some false
                else cmp lefttl righttl

    | LIST l :: lefttl, LIST r :: righttl ->
                (
                    match cmp l r with
                    | None -> cmp lefttl righttl 
                    | Some a -> Some a
                )
    | LIST l :: lefttl, INT r :: righttl -> 
            cmp left (LIST ([INT r]) :: righttl)
    | INT l :: lefttl , LIST r :: righttl -> 
            cmp (LIST ([INT l]) :: lefttl) right
    | [], [] -> None 
    | [], a -> Some true
    | a, [] -> Some false


let part_one () =
    let rec part_one_aux linecount = 

        match read_line_aux (), read_line_aux (), read_line_aux ()  with
        | Some a, Some b, Some _ 
        | Some a, Some b, None -> (if get_result a b then 
                        linecount + 1
                    else
                        0) + part_one_aux (linecount + 1)
        | _, _ ,_  -> 0
    and get_result first_line second_line =
        (match cmp (parse_string first_line) (parse_string second_line) with
        | Some true -> true
        | _ -> false)

    in
    part_one_aux 0

let part_two () =
    let divider_1 = parse_string "[[2]]" in
    let divider_2 = parse_string "[[6]]" in

    let rec read_all_lines () =
        match read_line_aux () with
        | Some "" -> read_all_lines () 
        | Some a -> (parse_string a) :: read_all_lines ()
        | None -> [divider_1; divider_2]
    in
    let rec calculate_thing pos result = function
        | [] -> result
        | hd :: tl -> let new_result = 
            if hd == divider_1 || hd == divider_2 then
                result + pos
            else 
                result
        in
        calculate_thing pos new_result tl

    in

    read_all_lines ()
    |> List.sort (fun a b -> match cmp a b with | Some true -> -1 | _ -> 1)  
    |> calculate_thing 0 1





let () =
    part_two () |> print_int
