open Core.Std
open Re2.Std
open Pygments

(* Basically, a higher-order |> *)
let (|>>) f g = fun x -> g (f x);;

type token = {
  tok_raw: string;
  tok_kind : string;
  tok_text : string;
}

(* Top-level so we only compile it once *)
let token_regex = Re2.create_exn "^Token.([^\\s]+)\\s*u['\"](.*)['\"]$"

let parse_token str =
  let subs =
    Re2.find_submatches token_regex str 
    |> function Error e -> raise (Failure (Format.sprintf "Could not parse %s" str))
              | Ok e -> e
  in
  if (Array.length subs <> 3) || not (Array.for_all ~f:Option.is_some subs)
  then None
  else Some {
      tok_raw = Option.value_exn subs.(0);
      tok_kind = Option.value_exn subs.(1);
      tok_text = Option.value_exn subs.(2);
    }
  
let parse_tokens ts = List.filter_opt @@ List.map ts ~f:parse_token

let tokenize ~lang =
  pygmentize ~format:"raw" ~lang
  |>> String.split_lines
  |>> parse_tokens


let pygmentize_tokens ?(format="html") =
  List.map ~f:(fun t -> t.tok_raw)
  |>> String.concat ~sep:"\n"
  |>> pygmentize ~lang:"raw" ~format


