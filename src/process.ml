open Core.Std
open Re2.Std
open Pandoc
open PygmentsHelper
module JSON = Yojson.Basic

let pandoc_started = ref false

let (<.>) f g = (fun x -> f (g x))
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

let determine_file_type filename =
  let ext_re = Re2.create_exn "[^\\\\]*\\.(\\w+)$" in (* Guranteed to compile *)
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
  pygmentize_tokens content 

let settings : JSON.json =
  let open JSON in
  `Assoc [
    "writerOptions", `Assoc [
        "writerHTMLMathMethod", `Assoc [
            "MathJax", `String "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
          ];
      ];
  ]

let ocaml_to_html code =
  let tokenized = tokenize ~lang:"ocaml" code in
  let depth = ref 0 in
  let unescape s =
    Scanf.sscanf ("\"" ^ s ^ "\"") "%S" Fn.id
  in
  let rec collect_comment acc = function
    | (t::ts) -> begin
        match (t.tok_kind,t.tok_text) with
        | ("Comment","*)") -> begin
            decr depth;
            if !depth = 0 then
              let comment = String.concat ~sep:"" @@ List.rev acc in
              (comment,ts) (* Base case *)
            else
              collect_comment acc ts
          end

        | ("Comment","(*") -> incr depth; collect_comment acc ts            (* Descend into the comment *)
        | ("Comment","*") -> collect_comment acc ts                         (* This discards extraneous '*' used for plain-text formatting *)
        | ("Comment", s) ->
            let s = if String.length s > 1 && s.[0] = ' ' && s.[1] <> ' '
              then unescape @@ String.drop_prefix t.tok_text 1
              else unescape s
            in collect_comment (s::acc) ts
        | _ -> raise (Failure "Well, we _thought_ we were in a comment, but apparently not. Check your file for missing comment terminators")
      end
    | _ -> assert false
  in let rec loop acc_tokens acc_html acc_toc =
       function
       | [] ->
           let toc = [](* pandoc ~ @@ String.concat ~sep:"\n" @@ List.rev acc_toc *\) "" *) in
           let html = (pygmentize_tokens @@ List.rev acc_tokens)::acc_html |> List.rev |> String.concat ~sep:"\n"
           in (html,toc)             (* Base case *)
       | x::xn::xs -> begin match (x.tok_kind,x.tok_text,xn.tok_text) with
           | ("Comment","(*","*") ->     (* This is a '(**' comment *) aka a docstring *)
               let converted_code = pygmentize_tokens @@ List.rev acc_tokens in
               let (raw_comment, tokens) = collect_comment [] (x::xn::xs) in
               (* let raw_comment = String.concat_map raw_comment ~f:(function '\n' -> "\\\n" | s -> String.of_char s) in *)
               let len = String.length raw_comment in
               let comment_buf = Buffer.create len in (* Converted will always be at least the length of the original *)
               ignore @@ pandoc ~settings:(JSON.to_string settings) ~input_format:"markdown" ~output_format:"html" (reader_of_bytes raw_comment) (buffer_writer comment_buf);
               let converted_comment = Buffer.to_bytes comment_buf in
               loop [] (converted_comment::converted_code::acc_html) (raw_comment::acc_toc) tokens
           | _ -> loop (x::acc_tokens) acc_html acc_toc (xn::xs)
         end
       | x::xs -> loop (x::acc_tokens) acc_html acc_toc (xs)
  in loop [] [] [] tokenized
