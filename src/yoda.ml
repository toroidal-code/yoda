open Core.Std

let pp = Format.fprintf
let strf = Format.str_formatter

(* Implementations, just print the args *)
type verb = Normal | Quiet | Verbose
type copts = { debug : bool; verb : verb }

let ident ppf s = pp ppf "%s" s
let kwd ppf s = pp ppf "%s" s

let pp_opt ppf sv = function Some v -> pp ppf "Some(%s)" (sv v)
                           | None   -> pp ppf "None"

let pp_opt_id ppf = pp_opt ppf Fn.id

let pp_verb ppf = function
  | Normal  -> pp ppf "normal"
  | Quiet   -> pp ppf "quiet"
  | Verbose -> pp ppf "verbose"

let pp_copts ppf copts = pp ppf
    "@[debug@ = %B@.vebosity@ =@ %a@.@]"
    copts.debug pp_verb copts.verb

let process copts format files =
  (* Format.printf "%aformat = %a\n" pp_copts copts pp_opt_id format; *)
  Pandoc.start ();
  List.iter files ~f:(fun file ->
      let open Process in
      let file_contents = In_channel.read_all file in
      (* (match check_and_determine file with *)
      (*  | `Ok t -> *)
      (*  | _ -> assert false) *)
      let html,toc = Process.ocaml_to_html file_contents in
      
      print_endline("<html><head> 
<script type=\"text/javascript\" src=\"https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\">
</script> </head><body><div class=\"container\">" ^ html ^"</div></body></html>")
    );
  Pandoc.stop ()


let help (ppf:Format.formatter) copts man_format cmds = function
  | None -> `Help (`Pager, None) (* help about the program *)
  | Some topic ->
    let topics = "topics" :: "patterns" :: "environment" :: cmds in
    let conv, _ = Cmdliner.Arg.enum @@ List.rev_map ~f:(fun s -> (s,s)) topics in
    match conv topic with
    | `Error e -> `Error (false, e)
    | `Ok t when t = "topics"    -> List.iter ~f:(fun t -> pp ppf "%s\n" t) topics; `Ok ()
    | `Ok t when List.mem cmds t -> `Help (man_format, Some t)
    | `Ok t ->
      let page = (topic, 7, "", "", ""), [`S topic; `P "Say something";] in
      `Ok (Cmdliner.Manpage.print man_format ppf page)

open Cmdliner

let copts_sect = "COMMON OPTIONS"
let help_secs = [
  `S copts_sect;
  `P "These options are common to all commands.";
  `S "MORE HELP";
  `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";`Noblank;
  `P "Use `$(mname) help environment' for help on environment variables.";
  `S "BUGS"; `P "Check bug reports at https://github.org/toroidal-code/yoda/issues.";]

(* Options common to all commands *)

let copts debug verb = { debug; verb;}
let copts_t =
  let docs = copts_sect in
  let debug =
    let doc = "Give only debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc)
  in
  let verb =
    let doc = "Suppress informational output." in
    let quiet = Quiet, Arg.info ["q"; "quiet"] ~docs ~doc in
    let doc = "Give verbose output." in
    let verbose = Verbose, Arg.info ["v"; "verbose"] ~docs ~doc in
    Arg.(last & vflag_all [Normal] [quiet; verbose])
  in
  Term.(pure copts $ debug $ verb)

(* Commands *)

let process_doc = "Run YODA over one or more files"
let process_cmd =
  let output_format =
    let doc = "The output format for the documentation." in
    Arg.(value & opt (some string) None & info ["f"; "format"] ~docv:"FORMAT" ~doc)
  in
  let files = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE or DIR") in
  let man = [
    `S "DESCRIPTION";
    `P "Process a set of ml/mli files and output their documentation."] @ help_secs
  in
  Term.(pure process $ copts_t $ output_format $ files),
  Term.info "process" ~sdocs:copts_sect ~doc:process_doc ~man

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "display help about YODA and YODA commands" in
  let man =
    [`S "DESCRIPTION";
     `P "Prints help about YODA commands and other subjects..."] @ help_secs
  in
  Term.(ret (pure (help Format.std_formatter) $
             copts_t $ Term.man_format $
             Term.choice_names $topic)),
  Term.info "help" ~doc ~man

let default_cmd =
  let doc = "Yet another OCaml Documentation Authoring tool" in
  let man = help_secs in
  let usage () = Printf.ksprintf (fun s -> print_string (Misc.reformat s); flush stdout)
    "usage: yoda [--version][--help]\n\
    \            <command> [<args>]\n\
     \n\
     The most commonly used yoda commands are:\n\
    \    process      %s\n\
     \n\
     See 'yoda help <command>' for more information on a specific command.\n"
    process_doc
  in
  Term.(pure usage $ (pure ())),
  Term.info "yoda"
    ~version:"0.1"
    ~sdocs:copts_sect
    ~doc
    ~man

let cmds = [process_cmd; help_cmd]

let () = match Term.eval_choice default_cmd cmds with
  |`Error _ -> exit 1
  | _       -> exit 0
