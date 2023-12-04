let digit_char_to_int d = d |> Char.escaped |> int_of_string

let line_to_number_list s =
    List.init (String.length s) (fun i -> String.get s i |> digit_char_to_int)

(*Não usado*)
let rec list_to_string = function
    | h :: tl -> (string_of_int h)  ^ list_to_string tl
    | [] -> ""

let outloud line_of_numbers =
    let rec outloud_aux prev repetion_count result = function 
        | h::tl -> if h == prev then 
                        outloud_aux prev (repetion_count + 1) result tl 
                   else 
                       outloud_aux h 1 (prev::repetion_count::result) tl
        | [] ->  List.rev (prev:: repetion_count :: result)
    in
    outloud_aux (List.hd line_of_numbers) 1 [] (List.tl line_of_numbers)

let rec do_n_times count inp =
    if count > 0 then
        do_n_times (count - 1) (outloud inp)
    else
        inp

let () =
    read_line () 
    |> line_to_number_list 
    |> do_n_times 40 
    |> List.length
    |> print_int

    
