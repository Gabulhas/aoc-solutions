let read_line_aux () = 
    try (read_line ()) with
    | End_of_file -> "\000"


let read_elves_calories () =
    let rec read_loop elf_calories all_calories = 
        let inp = read_line_aux () in
        match inp with
        | "\000" -> all_calories
        | "" -> read_loop 0 (elf_calories :: all_calories)
        | _ ->  let calories = int_of_string inp in
                read_loop (elf_calories + calories) (all_calories)
        
    in

     read_loop 0 []


let get_sorted_calories calories = 
    List.sort (fun a b -> a - b) calories


let () = 
    let all_calories= (read_elves_calories () |> get_sorted_calories) in
    List.iteri (fun i v -> Printf.printf "%d - %d\n" i v) all_calories

                


