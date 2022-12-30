let read_line_aux () = 
    try (read_line ()) with
    | End_of_file -> "\000"


let read_elves_calories () =
    let rec read_loop elf elf_calories (max_elf, max_calories) = 
        let inp = read_line_aux () in
        match inp with
        | "\000" -> (max_elf, max_calories)
        | "" -> if elf_calories > max_calories then read_loop (elf + 1) 0 (elf, elf_calories) else read_loop (elf + 1) 0 (max_elf, max_calories)
        | _ ->  let calories = int_of_string inp in
                read_loop elf (elf_calories + calories) (max_elf, max_calories)
        
    in

     read_loop 0 0 (-1, -1)


let () = 
    let max_elf, max_calories = read_elves_calories () in
    Printf.printf "%d, %d" max_elf max_calories
                


