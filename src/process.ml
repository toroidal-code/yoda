open Core.Std
open Re2.Std
open Pandoc
module JSON = Yojson.Basic

let pandoc_started = ref false

let ( *.* ) f g = (fun x -> f (g x))
let (>>>) f g = (fun x -> g (f x))

(** The type to represent errors when determining
  * filetype. *)
type file_err = [
  | `Missing_file      of string
  | `Missing_extension of string
  | `Unknown_extension of string
]

(** Filetypes that YODA supports.*)
type filetype =
  | OCaml_implementation
  | OCaml_interface
  | Markdown
  | LaTeX
  | HTML
  | CSS

let ext_re = Re2.create_exn "[^\\\\]*\\.(\\w+)$" (* Guranteed to compile *)

let determine_file_type filename =
  let ext = Re2.find_first ~sub:(`Index 1) ext_re filename in
  match ext with
  | Error e -> begin
      match Error.to_exn e with
      | Re2.Exceptions.Regex_match_failed _ -> Error(`Missing_extension filename)
      | _ -> assert false
    end
  | Ok "ml"   -> Ok OCaml_implementation
  | Ok "mli"  -> Ok OCaml_interface
  | Ok "md"   -> Ok Markdown
  | Ok "tex"  -> Ok LaTeX
  | Ok "html" -> Ok HTML
  | Ok "css"  -> Ok CSS
  | Ok e      -> Error(`Unknown_extension e)

let check_file_exists filename =
  match Sys.file_exists ~follow_symlinks:false filename with
  | `No | `Unknown -> Error (`Missing_file filename)
  | `Yes -> Ok filename

let check_and_determine filename =
  let open Result in
  return filename >>=
  check_file_exists >>=
  determine_file_type

let to_toc content =
  Pygments.pygmentize content

let settings : JSON.json =
  let open JSON in
  `Assoc [
    "writerOptions", `Assoc [
        "writerHTMLMathMethod", `Assoc [
            "MathJax", `String "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
          ];
      ];
  ]

let toc_settings : JSON.json =
  let open JSON in
  `Assoc [
    "writerOptions", `Assoc [
        "writerStandalone", `Bool true;
        "writerTemplate", `String "$toc$\n";
        "writerTableOfContents", `Bool true
      ];
  ]
  

open Sedlexing
let prev_buffer_offset = ref 0
let prev_line_offset = ref 0

let current_column (lexbuf:lexbuf) =
  (* printf "buf_offset: %d \tline_offset: %d\n" lexbuf.curr_pos.buffer_offset lexbuf.curr_pos.line_offset; *)
  prev_buffer_offset := lexbuf.curr_pos.buffer_offset;
  prev_line_offset := lexbuf.curr_pos.line_offset;
  lexbuf.curr_pos.buffer_offset - lexbuf.curr_pos.line_offset

(* Because the column of a newline is apparently the first character of the next line.
   TODO: Probably a sedlex bug
   Assumes that current_column is called for every character (or really the character before the newline). It is. *)
let prev_column () = (!prev_buffer_offset) - (!prev_line_offset)

type raw =
  | Text of string
  | Comment of string
  [@@deriving show]

type highlighter =
  | Pygments
  | Higlo
  | Kate

let many_asterisk_newline = [%re? Plus '*', '\n']
let start_doc_comment = [%re? "(*", many_asterisk_newline]
let many_asterisk_end = [%re? Plus '*',')']

(** This tokenizes an entire OCaml file in one pass, making it easier to deal with_return
  * later on. This *is* memory heavy, but shouldn't be too bad. If it becomes an issue,
  * I'll reevaluate the strategy and might switch to a stream-based system. *)
let lex_ocaml lexbuf =
  let comment_column = ref 0 in
  (* Before, we ran the entire file through pygments, got its tokenized IR,
   * and then worked with that. It was *very* slow. This uses table-based scanning.
   * It's about 500x faster. lol *)
  let rec lexer blocks acc lexbuf = match%sedlex lexbuf with
    (* This is the beginning of a valid rich comment *)
    | "(**" ->
        comment_column := (current_column lexbuf) - 1;

        (* Consume all remaining asterisks on this line *)
        begin match%sedlex lexbuf with
        | many_asterisk_newline -> ()
        | _ -> ()
        end;

        (* Squash the text up until now and prep it for stream insertion *)
        let squashed = List.rev acc
                       |> String.concat
                       |> String.rstrip
                       |> String.lstrip ~drop:(fun c -> c = '\n')
        in
        let text = if squashed = "" then None else Some (Text squashed) in

        (* Consumes the entire comment and returns it as a @block option@ *)
        let comment = comment 0 None [] lexbuf in

        let blocks = match comment, text with
          | Some(c), Some(t) -> (c::t::blocks)
          | None   , Some(t) -> (t::blocks)
          | Some(c), None    -> (c::blocks)
          | None   , None    -> blocks
        in
        lexer blocks [] lexbuf (* Continue with a clean text accumulator *)

    (* At the end of file, return the entire collection *)
    | eof ->
        (* Squash the text up until now and prep it for stream insertion *)
        let squashed = List.rev acc
                       |> String.concat
                       |> String.rstrip
                       |> String.lstrip ~drop:(fun c -> c = '\n')
        in
        let blocks = if squashed = "" then blocks else (Text squashed)::blocks in

        List.rev blocks

    (* Otherwise, add the character to the accumulator *)
    | any -> lexer blocks ((Utf8.lexeme lexbuf)::acc) lexbuf
    | _ -> assert false

  and comment depth indent acc lexbuf =
    (* (match indent with None -> print_endline "None" | Some c -> printf "Some (%d)\n" c); *)
    match%sedlex lexbuf with
    (* In the event of an asterisk, we have to check the column that the comment started on,
       since comments like
      (**
       * text *)
      and
      (**
        * text *)
      should ignore the leading asterisk. However, we can't just discard leading asterisks,
      because Markdown uses them to denote lists, thus, we check the column and discard
      accordingly *)
    | '*' ->
        if current_column lexbuf = !comment_column ||
           current_column lexbuf = !comment_column + 1 then
          comment depth indent acc lexbuf
        else
          comment depth indent ("*"::acc) lexbuf

    | many_asterisk_end ->
        if depth = 0 then begin
          comment_column := 0;
          let raw = (String.concat @@ List.rev @@ acc) in
          let stripped = String.strip ~drop:(function '*' -> true | _ -> false) raw in
          if String.is_empty stripped then None else Some (Comment stripped)
        end else
          comment (depth - 1) indent acc lexbuf

    (* Nesting documentation is not allowed, but comments are allowed inside. *)
    | "(*" -> comment (depth + 1) indent acc lexbuf
    (* | many_asterisk_newline -> *)
    (*     (\* if (current_column lexbuf) - 1 = !comment_column || *\) *)
    (*     (\*    current_column lexbuf = !comment_column then *\) *)
    (*       (\* comment depth indent (" \\\n"::acc) lexbuf *\) *)
    (*     (\* else *\) *)
    (*       comment depth indent acc lexbuf *)

    | alphabetic ->
        let curr_col = current_column lexbuf in begin
          match indent with
          | None -> comment depth (Some(curr_col - 1)) ((Utf8.lexeme lexbuf)::acc) lexbuf
          | Some(c) when curr_col < c -> comment depth (Some curr_col) ((Utf8.lexeme lexbuf)::acc) lexbuf
          | _ -> comment depth indent ((Utf8.lexeme lexbuf)::acc) lexbuf
        end
    | ' ' -> begin
        match indent with
        | Some c when c < current_column lexbuf ->
            comment depth indent (" "::acc) lexbuf
        | _ -> comment depth indent acc lexbuf (* If we aren't at our indent column, discard whitespace *)
      end
    | '\n'->
        let prefix =
          if (!comment_column) + 1 < (prev_column ()) then "  \n" else "\n"
        in comment depth indent (prefix::acc) lexbuf
    | any -> comment depth indent ((Utf8.lexeme lexbuf)::acc) lexbuf
    | eof -> raise (Failure "We hit the End of File while in a comment.")
    | _ -> assert false
  in
  lexer [] [] lexbuf

let extra_scripts =
 "<script type=\"text/javascript\" src=\"https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"


let ocaml_to_html ?(template="basic") ?(highlighter=Pygments) chan file_name =
  let module_name =
    let open Re2.Infix in
    Filename.basename file_name |> String.capitalize
    |> Re2.replace_exn ~/"\\.(\\w+)$" ~f:(fun _ -> "")
  in
  let tokenized = lex_ocaml (Sedlexing.Utf8.from_channel chan) in
  (* List.iter ~f:(print_endline <.> show_raw) tokenized; *)
  let convert_pandoc c =
    (* Converted will always be at least the length of the original *)
    let conversion_buffer = Buffer.create (String.length c) in
    pandoc ~size:(String.length c) ~settings:(JSON.to_string settings)
      ~input_format:"markdown" ~output_format:"html"
      (reader_of_bytes c) (buffer_writer conversion_buffer);
    Buffer.to_bytes conversion_buffer
  in
  let process = function
    | Text t -> begin match highlighter with
        (* | Kate -> *)
        (*     let buf = Buffer.create (String.length t) in *)
        (*     highlight ~block:true ~lang:"ocaml" (reader_of_bytes t) (buffer_writer buf); *)
        (*     Buffer.to_bytes buf *)

        | _ (* Pygments *) -> (Pygments.pygmentize ~lang:"ocaml" t)

        (* | Higlo -> *)
        (*     let xml = Xtmpl_xml.to_string @@ Higlo.to_xml ~lang:"ocaml" t in *)
        (*     sprintf "<div class=\"highlight\"><pre>%s</pre></div>" xml *)
      end
    | Comment c -> convert_pandoc c

  in
  let processed = List.map tokenized ~f:process in
  let content = String.concat processed in
  let toc =
    tokenized
    |> List.filter_map ~f:(function Text t -> None | Comment c -> Some c)
    |> String.concat ~sep:"\n" |> String.split_lines
    |> List.filter ~f:(fun s -> (not @@ String.is_empty s) &&
                                (Char.equal '#' @@ String.get s 0))
    |> String.concat ~sep:"\n"
    |> (fun c ->
        let conversion_buffer = Buffer.create (String.length c) in
         pandoc ~size:(String.length c) ~settings:(JSON.to_string toc_settings)
           ~input_format:"markdown" ~output_format:"html"
           (reader_of_bytes c) (buffer_writer conversion_buffer);
         Buffer.to_bytes conversion_buffer)
  in
  let template = In_channel.read_all ("templates/" ^ template ^ ".html") in
  let substitution = function
    | "pagetitle" -> module_name
    | "highlighting_css" -> In_channel.read_all "css/prism_pyg.css"
    | "toc" -> toc
    | "content" -> content
    | "extra_scripts" -> extra_scripts
    | _ -> ""
  in
  let html = Buffer.create (String.length content) in
  Buffer.add_substitute html substitution template;
  (Buffer.to_bytes html, [])
  (* ("",[]) *)
