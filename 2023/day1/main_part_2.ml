module CharMap = Map.Make (Char)

type loltree = Leaf of int | Node of loltree CharMap.t

let rec loltree_compare x y =
  match (x, y) with
  | Leaf a, Leaf b -> Int.compare a b
  | Node _, Leaf _ | Leaf _, Node _ -> -1
  | Node a, Node b -> if a = b then 0 else 1

module LolTreeSet = Set.Make (struct
  type t = loltree

  let compare = loltree_compare
end)

let words_to_tree () =
  let words =
    [
      ("one", 1);
      ("two", 2);
      ("three", 3);
      ("four", 4);
      ("five", 5);
      ("six", 6);
      ("seven", 7);
      ("eight", 8);
      ("nine", 9);
    ]
  in

  let insert_word tree (word, num) =
    let rec insert chars tree =
      match (chars, tree) with
      | [], _ -> Leaf num
      | c :: cs, Node map ->
          let subtree =
            match CharMap.find_opt c map with
            | Some t -> t
            | None -> Node CharMap.empty
          in
          Node (CharMap.add c (insert cs subtree) map)
      | _, Leaf _ -> failwith "Unexpected leaf encountered while building tree"
    in
    insert (String.to_seq word |> List.of_seq) tree
  in

  let initial_tree = Node CharMap.empty in
  List.fold_left insert_word initial_tree words

let print_tree tree =
  let rec print_subtree depth = function
    | Leaf num -> Printf.printf "Leaf(%d) at depth %d\n" num depth
    | Node map ->
        CharMap.iter
          (fun key subtree ->
            Printf.printf "Node('%c') at depth %d\n" key depth;
            print_subtree (depth + 1) subtree)
          map
  in
  print_subtree 0 tree

(* Example usage after building the tree: *)
let tree = words_to_tree ()

let explode s =
  let rec exp i l = if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let read_line_aux () = try Some (read_line ()) with End_of_file -> None
let is_number = function '0' .. '9' -> true | _ -> false

(*overengineering btw*)

(*
one
two
three
four
five
six
seven
eight
nine

e
f
n
o
s
t
*)

let process_document () =
  let tree = words_to_tree () in
  let tree_content =
    match tree with Leaf _ -> failwith "impossible" | Node a -> a
  in

  let add_if_first_letter current_found c =
    match CharMap.find_opt c tree_content with
    | Some a -> LolTreeSet.add a current_found
    | None -> current_found
  in

  let remove_step_finish_if_next_letter current_found c =
    LolTreeSet.fold
      (fun this_tree (result_found_set, leaf_found) ->
        let result_found_set = LolTreeSet.remove this_tree result_found_set in

        let this_tree_content =
          match this_tree with
          | Leaf _ -> failwith "Impossible, the current' three is a leaf."
          | Node this_tree_content -> this_tree_content
        in

        match CharMap.find_opt c this_tree_content with
        | None -> (result_found_set, leaf_found)
        | Some (Node a) -> (LolTreeSet.add (Node a) result_found_set, leaf_found)
        | Some (Leaf a) -> (result_found_set, Some a))
      current_found (current_found, None)
  in

  let rec get_numbers_in_line line (first : int option) last current_found =
    match line with
    | [] -> (
        match (first, last) with
        | Some a, None -> (a * 10) + a
        | Some a, Some b -> (a * 10) + b
        | None, _ -> failwith "Impossible, not first nor second")
    | c :: tl ->
        (* match first with None -> (Some c, Some c) | _ -> (first, Some c)*)
        let number, current_found =
          if is_number c then
            (Some (c |> Char.escaped |> int_of_string), LolTreeSet.empty)
          else
            let current_found, leaf_opt =
              remove_step_finish_if_next_letter current_found c
            in
            let current_found = add_if_first_letter current_found c in
            (leaf_opt, current_found)
        in

        let first, last =
          match number with
          | Some n -> (
              match first with None -> (Some n, Some n) | _ -> (first, Some n))
          | None -> (first, last)
        in
        get_numbers_in_line tl first last current_found
  in

  let rec read_stuff () =
    match read_line_aux () with
    | None -> 0
    | Some line ->
        read_stuff ()
        + get_numbers_in_line (explode line) None None LolTreeSet.empty
  in

  read_stuff ()

let () = process_document () |> print_int
