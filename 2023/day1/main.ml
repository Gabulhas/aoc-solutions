let explode s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let read_line_aux () = try Some (read_line ()) with End_of_file -> None
let is_number = function '0' .. '9' -> true | _ -> false

let process_document () =
  let get_numbers_in_line line =
    let first, last =
      List.fold_left
        (fun (first, last) this ->
          if is_number this then
            match first with
            | None -> (Some this, Some this)
            | _ -> (first, Some this)
          else (first, last))
        (None, None) (explode line)
    in
    (*Code lmao what*)
    ((first |> Option.get |> Char.escaped |> int_of_string) * 10)
    + (last |> Option.get |> Char.escaped |> int_of_string)
  in

  let rec read_stuff () =
    match read_line_aux () with
    | None -> 0
    | Some line -> read_stuff () + get_numbers_in_line line
  in

  read_stuff ()

let () = process_document () |> print_int
