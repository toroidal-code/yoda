
(* Taken from the opamMisc.ml file in the OPAM
 * source tree. These functions are licensed under the
 * GPLv3 with linking exception. Copyright INRIA and OCamlPro *)

let visual_length_substring s ofs len =
  let rec aux s i =
    try
      let i = String.index_from s i '\027' in
      let j = String.index_from s (i+1) 'm' in
      if j > ofs + len then 0 else
        j - i + 1 + aux s (j+1)
    with Not_found | Invalid_argument _ -> 0
  in
  len - aux s ofs

let visual_length s = visual_length_substring s 0 (String.length s)

let default_columns = 100

let with_process_in cmd args f =
  let path = ["/bin";"/usr/bin"] in
  let cmd =
    List.find Sys.file_exists (List.map (fun d -> Filename.concat d cmd) path)
  in
  let ic = Unix.open_process_in (cmd^" "^args) in
  try
    let r = f ic in
    ignore (Unix.close_process_in ic) ; r
  with exn ->
    ignore (Unix.close_process_in ic) ; raise exn


(* Calculate the number of terminal columns.
 * First, try using `tput cols` which gives us straight answer.
 * Next, try GNU's `stty size` which returns the number of rows and
 * columns reported by the kernel.
 * As a last ditch resort, try using the 'COLUMNS' envvar  *)
let get_terminal_columns () =
  try (* terminfo *)
    with_process_in "tput" "cols"
      (fun ic -> int_of_string (input_line ic))
  with Unix.Unix_error _ | Sys_error _ | Failure _ | End_of_file | Not_found ->
    try (* GNU stty *)
      with_process_in "stty" "size"
        (fun ic ->
           match Core.Std.String.split (input_line ic) ' ' with
           | [_ ; v] -> int_of_string v
           | _ -> failwith "stty")
    with
      Unix.Unix_error _ | Sys_error _ | Failure _  | End_of_file | Not_found ->
      try (* shell envvar *)
        int_of_string (Sys.getenv "COLUMNS")
      with Not_found | Failure _ ->
        default_columns

let tty_out = Unix.isatty Unix.stdout

(* If we're a true tty, then fetch the number of 
 * columns, else return the default VT100 number of columns: 80 *)
let terminal_columns =
  let v = ref (lazy (get_terminal_columns ())) in
  let () =
    try Sys.set_signal 28 (* SIGWINCH *)
          (Sys.Signal_handle
             (fun _ -> v := lazy (get_terminal_columns ())))
    with Invalid_argument _ -> ()
  in
  fun () ->
    if tty_out
    then Lazy.force !v
    else 80

let reformat ?(start_column=0) ?(indent=0) s =
  let slen = String.length s in
  let buf = Buffer.create 1024 in
  let rec find_nonsp i =
    if i >= slen then i else
      match s.[i] with ' ' -> find_nonsp (i+1) | _ -> i
  in
  let rec find_split i =
    if i >= slen then i else
      match s.[i] with ' ' | '\n' -> i | _ -> find_split (i+1)
  in
  let newline i =
    Buffer.add_char buf '\n';
    if i+1 < slen && s.[i+1] <> '\n' then
      for _i = 1 to indent do Buffer.add_char buf ' ' done
  in
  let rec print i col =
    if i >= slen then () else
    if s.[i] = '\n' then (newline i; print (i+1) indent) else
      let j = find_nonsp i in
      let k = find_split j in
      let len_visual = visual_length_substring s i (k - i) in
      if col + len_visual >= terminal_columns () && col > indent then
        (newline i;
         Buffer.add_substring buf s j (k - j);
         print k (indent + len_visual - j + i))
      else
        (Buffer.add_substring buf s i (k - i);
         print k (col + len_visual))
  in
  print 0 start_column;
  Buffer.contents buf
