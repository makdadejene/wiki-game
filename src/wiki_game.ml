open! Core
module Link = String

(* We separate out the [Network] module to represent our social network in
   OCaml types. *)
(* [get_linked_articles] should return a list of wikipedia article lengths
   contained in the input.

   Note that [get_linked_articles] should ONLY return things that look like
   wikipedia articles. In particular, we should discard links that are: -
   Wikipedia pages under special namespaces that are not articles (see
   https://en.wikipedia.org/wiki/Wikipedia:Namespaces) - other Wikipedia
   internal URLs that are not articles - resources that are external to
   Wikipedia - page headers

   One nice think about Wikipedia is that stringent content moderation
   results in uniformity in article format. We can expect that all Wikipedia
   article links parsed from a Wikipedia page will have the form
   "/wiki/<TITLE>". *)

(* let filter_wiki check: (* match // compare *) ;; *)

let get_linked_articles contents : string list =
  let open Soup in
  parse contents
  $$ "a[href]"
  |> filter (fun x ->
       let curr_link =
         R.attribute "href" x
         (* String.substr_replace_all (R.attribute "href" x)
            ~pattern:"https://en.wikipedia.org" ~with_:"" *)
       in
       let is_string = String.is_prefix curr_link ~prefix:"/wiki/" in
       if is_string
       then (
         match Wikipedia_namespace.namespace curr_link with
         | None -> true
         | Some _ -> false)
       else false)
  |> to_list
  |> List.map ~f:(fun x -> R.attribute "href" x)
  |> List.dedup_and_sort ~compare:String.compare
;;

(* ignore (contents : string); failwith "TODO" *)

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

(* [visualize] should explore all linked articles up to a distance of
   [max_depth] away from the given [origin] article, and output the result as
   a DOT file. It should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory. *)

let rec find_pages origin ~(curr_depth : int) ~how_to_fetch =
  if not (curr_depth = 0)
  then (
    let new_file = File_fetcher.fetch_exn how_to_fetch ~resource:origin in
    let linked_articles = get_linked_articles new_file in
    List.concat_map linked_articles ~f:(fun link ->
      [ origin, link ]
      @ find_pages link ~curr_depth:(curr_depth - 1) ~how_to_fetch))
  else []
;;

(* let actual_link link = if String.is_substring link
   ~substring:"https://en.wikipedia.org" then link else
   "https://en.wikipedia.org" ^ link ;; *)

let dfs start_node destination visited max_depth how_to_fetch =
  let solution = ref [] in
  let rec dfs curr_node destination visited max_depth how_to_fetch =
    if not (max_depth = 0)
    then (
      let new_file =
        File_fetcher.fetch_exn how_to_fetch ~resource:curr_node
      in
      let linked_articles = get_linked_articles new_file in
      Hash_set.add visited curr_node;
      let sol =
        List.find_map linked_articles ~f:(fun link ->
          if not (Hash_set.mem visited link)
          then
            if String.equal link destination
            then Some link
            else dfs link destination visited (max_depth - 1) how_to_fetch
          else None)
      in
      match sol with
      | None -> None
      | Some pos ->
        (* print_s [%message (pos : Position.t)]; *)
        solution := pos :: !solution;
        Some curr_node)
    else None
  in
  match dfs start_node destination visited max_depth how_to_fetch with
  | Some pos -> pos :: !solution
  | None -> []
;;

let find_path ~(max_depth : int) ~origin ~destination ~how_to_fetch =
  let visited = String.Hash_set.create () in
  dfs origin destination visited max_depth how_to_fetch
;;

(* ignore (max_depth : int); ignore (origin : string); ignore (destination :
   string); ignore (how_to_fetch : File_fetcher.How_to_fetch.t); failwith
   "TODO" *)
module G = Graph.Imperative.Graph.Concrete (Link)

(* We extend our [Graph] structure with the [Dot] API so that we can easily
   render constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
  include G

  (* These functions can be changed to tweak the appearance of the generated
     graph. Check out the ocamlgraph graphviz API
     (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli)
     for examples of what values can be set here. *)
  let edge_attributes _ = [ `Dir `Forward ]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

(* let clean_string old_string = String. String.substr_replace_all
   (String.substr_replace_all (String.substr_replace_all
   (String.substr_replace_all (String.substr_replace_all old_string
   ~pattern:"/" ~with_:"") ~pattern:"." ~with_:"") ~pattern:" " ~with_:"")
   ~pattern:"(" ~with_:"") ~pattern:")" ~with_:""

   ; *)

let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let pages = find_pages origin ~curr_depth:max_depth ~how_to_fetch in
  let graph = G.create () in
  List.iter pages ~f:(fun (link1, link2) ->
    let link1 =
      String.substr_replace_all
        (String.substr_replace_all
           (String.substr_replace_all
              (String.substr_replace_all
                 (String.substr_replace_all link1 ~pattern:"/" ~with_:"")
                 ~pattern:"."
                 ~with_:"")
              ~pattern:" "
              ~with_:"")
           ~pattern:"("
           ~with_:"")
        ~pattern:")"
        ~with_:""
    in
    let link2 =
      String.substr_replace_all
        (String.substr_replace_all
           (String.substr_replace_all
              (String.substr_replace_all
                 (String.substr_replace_all link2 ~pattern:"/" ~with_:"")
                 ~pattern:"."
                 ~with_:"")
              ~pattern:" "
              ~with_:"")
           ~pattern:"("
           ~with_:"")
        ~pattern:")"
        ~with_:""
    in
    (* [G.add_edge] auomatically adds the endpoints as vertices in the graph
       if they don't already exist. *)
    G.add_edge graph link1 link2);
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph
;;

(* ignore (max_depth : int); ignore (origin : string); ignore (output_file :
   File_path.t); ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
   failwith "TODO" *)

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and
   the destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with
   [File_fetcher] to fetch the articles so that the implementation can be
   tested locally on the small dataset in the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the
   graph. *)

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        let path = find_path ~max_depth ~origin ~destination ~how_to_fetch in
        if List.is_empty path
        then print_endline "No path found!"
        else List.iter path ~f:print_endline
      (* | None -> print_endline "No path found!" | Some trace -> List.iter
         trace ~f:print_endline] *)]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
