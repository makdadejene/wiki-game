open! Core

module Position = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp, hash]
  end

  include T
  include Comparable.Make (T)
end

let offsets y x = [ y + 1, x; y - 1, x; y, x + 1; y, x - 1 ]

let dfs graph curr_node =
  let solution = ref [] in
  let rec dfs
    graph
    (curr_node : Position.T.t)
    (visited : Position.T.t Hash_set.t)
    =
    Hash_set.add visited curr_node;
    let y = fst curr_node in
    let x = snd curr_node in
    let directions = offsets y x in
    let sol =
      List.find_map directions ~f:(fun key ->
        if Map.mem graph key && not (Hash_set.mem visited key)
        then (
          let curr_char = Map.find_exn graph key in
          match curr_char with
          | '.' -> dfs graph key visited
          | 'E' -> Some key
          | _ -> None)
        else None)
    in
    match sol with
    | None -> None
    | Some pos ->
      (* print_s [%message (pos : Position.t)]; *)
      solution := pos :: !solution;
      Some curr_node
  in
  let set = Hash_set.create (module Position) in
  match dfs graph curr_node set with
  | Some pos -> pos :: !solution
  | None -> []
;;

(* Hash_set.add visited current_node; let adjacent_nodes = Map.find_exn graph
   current_node in List.iter adjacent_nodes ~f:(fun next_node -> if not
   (Hash_set.mem visited next_node) then dfs graph next_node visited) *)

let solve_maze input_file =
  let curr_file = In_channel.read_lines (File_path.to_string input_file) in
  let height = List.length curr_file in
  let width = String.length (List.nth_exn curr_file 0) in
  let maze_map =
    List.init height ~f:(fun y -> List.init width ~f:(fun x -> [ y, x ]))
  in
  let curr_maze_map = List.concat (List.concat maze_map) in
  let all_char = List.map curr_file ~f:(fun line -> String.to_list line) in
  let curr_chars = List.concat all_char in
  let old_map =
    List.map2_exn curr_maze_map curr_chars ~f:(fun a b -> a, b)
  in
  let curr_map = Position.Map.of_alist_exn old_map in
  let key, _ =
    Map.to_alist curr_map
    |> List.find_exn ~f:(fun (_key, data) -> Char.equal data 'S')
  in
  dfs curr_map key
;;

(* List.concat_map(List.range 0 ) *)

(* List.mapi curr_file ~f:(fun y_index line -> let curr_list = String.to_list
   line in line @ [y_index] List.mapi curr_list ~f:(fun x_index new_char ->
   )) *)

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let solution = solve_maze input_file in
        print_s [%message (solution : Position.t list)]
      (* ignore (input_file : File_path.t); failwith "TODO"] *)]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
