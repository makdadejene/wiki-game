open! Core
module City = String

module Highway = struct
  include String

  let default = ""
end

(* We separate out the [Network] module to represent our social network in
   OCaml types. *)
module Network = struct
  (* We can represent our social network graph as a set of connections, where
     a connection represents a friendship between two people. *)
  module Connection = struct
    module T = struct
      type t = City.t * Highway.t * City.t [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s.
       This is needed to defined our [Network.t] type later. Using this
       [Comparable.Make] functor also gives us immutable maps, which might
       come in handy later. *)
    include Comparable.Make (T)
    (* let of_string s = match String.split s ~on:',' with | [ x; y; z ] ->
       Some (City.of_string x, Highway.of_string y, City.of_string z) | _ ->
       None ;; *)
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  let load_file input_file =
    let curr_file = In_channel.read_lines (File_path.to_string input_file) in
    List.concat_map curr_file ~f:(fun s ->
      let line = String.split s ~on:',' in
      let highway =
        String.substr_replace_all
          (String.substr_replace_all
             (List.hd_exn line)
             ~pattern:"."
             ~with_:"")
          ~pattern:" "
          ~with_:""
      in
      let cities = List.tl_exn line in
      List.foldi cities ~init:[] ~f:(fun index result _city ->
        if index < List.length cities - 1
        then (
          let first =
            String.substr_replace_all
              (String.substr_replace_all
                 (List.nth_exn cities index)
                 ~pattern:"."
                 ~with_:"")
              ~pattern:" "
              ~with_:""
          in
          let second =
            String.substr_replace_all
              (String.substr_replace_all
                 (List.nth_exn cities (index + 1))
                 ~pattern:"."
                 ~with_:"")
              ~pattern:" "
              ~with_:""
          in
          result @ [ first, highway, second ])
        else result))
  ;;

  let of_file input_file =
    let curr_tuple =
      load_file input_file
      |> List.concat_map ~f:(fun (a, b, c) ->
           (* Friendships are mutual; a connection between a and b means we
              should also consider the connection between b and a. *)
           [ a, b, c ])
    in
    Connection.Set.of_list curr_tuple
  ;;
end

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

module G =
  Graph.Imperative.Digraph.ConcreteBidirectionalLabeled (City) (Highway)

module Dot = Graph.Graphviz.Dot (struct
  include G

  (* These functions can be changed to tweak the appearance of the generated
     graph. Check out the ocamlgraph graphviz API
     (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
     for examples of what values can be set here. *)
  let edge_attributes _ = [ `Dir `None ]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

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
        let network = Network.of_file input_file in
        let graph = G.create () in
        Set.iter network ~f:(fun (city1, highway, city2) ->
          (* [G.add_edge] auomatically adds the endpoints as vertices in the
             graph if they don't already exist. *)
          let edge = G.E.create city1 highway city2 in
          G.add_edge_e graph edge);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        (* ignore (input_file : File_path.t); ignore (output_file :
           File_path.t); *)
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
