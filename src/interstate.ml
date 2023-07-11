open! Core


let load_file input_file = 
  let curr_file = In_channel.read_lines (File_path.to_string input_file) in
  let curr_list = List.fold curr_file ~init:[] ~f:(fun curr_city s ->
    match String.split s ~on: ',' with 
    | interstate:: highway -> curr_city
     @ List.foldi ~init:index ~f:(fun curr_city acc new_city -> 
        if(index<(List.length curr_city)-1) then 
          @[(highway, interstate, List.nth _exn highway(index+1))]
      else
        acc)
    |_-> []
  ) in
  
  List.concat_map curr_file curr_list
    ;;



let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        ignore (input_file : File_path.t);
        failwith "TODO"]
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        ignore (input_file : File_path.t);
        ignore (output_file : File_path.t);
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
