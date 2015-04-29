open Ctypes
open PosixTypes
open Foreign

exception PandocError of string

let started = ref false                

let foreign name typ = foreign name typ
    ~from:Dl.(dlopen ~filename:"libpandoc.so" ~flags:[RTLD_NOW])

let start = foreign "pandoc_init" (void @-> returning void)
let stop = foreign "pandoc_exit" (void @-> returning void)

type reader = char ptr -> int -> unit Ctypes.ptr -> int
let reader_t = ( ptr char @-> int @-> ptr void @-> returning int)

type writer = string option -> int -> unit Ctypes.ptr -> unit
let writer_t = (string_opt @-> int @-> ptr void @-> returning void)

let _pandoc = foreign "pandoc"
    (int @-> string @-> string @-> string @-> funptr reader_t
     @-> funptr writer_t @-> ptr void @-> returning string_opt)

(* WARNING: UNSAFE CAN CAUSE SEGFAULTS length must always be minus one of expected to account for null-term *)
let unsafe_char_ptr_blit_bytes src src_off dest dest_off len  =
  let src = Bytes.sub src src_off len in
  let dest = ref @@ dest +@ dest_off in
  Bytes.iter (fun c ->
      !dest <-@ c;
      dest := !dest +@ 1) src;
  !dest <-@ (Char.chr 0)

let pandoc ?(size=1024) ?(user_data=null) ?(settings="{}") ~input_format ~output_format (reader : reader) (writer : writer) =
  if not !started then start ();
  let err = _pandoc size input_format output_format settings reader writer user_data in
  match err with
  | Some s -> raise (PandocError s)
  | None   -> ()

let _valid_language = foreign "valid_language" (string @-> returning int)
let valid_language language =
  match _valid_language language with
  | 0 -> false
  | _ -> true


let _highlight =
  foreign "highlight"
    (int @-> string @-> string @-> int @-> funptr reader_t
     @-> funptr writer_t @-> ptr void @-> returning void)

let highlight ?(size=1024) ?(user_data=null) language ?(output_format="html") ?(block=true) (reader : reader) (writer : writer) =
  if not !started then start ();
  _highlight size language output_format (match block with true -> 1 | false -> 0) reader writer user_data

(* WARNING: UNSAFE CAN CAUSE SEGFAULTS length must always be minus one of expected to account for null-term *)
let unsafe_char_ptr_blit_bytes src src_off dest dest_off len  =
  let src = Bytes.sub src src_off len in
  let dest = ref @@ dest +@ dest_off in
  Bytes.iter (fun c ->
      !dest <-@ c;
      dest := !dest +@ 1) src;
  !dest <-@ (Char.chr 0)

let reader_of_bytes bts : reader =
  let len = ref @@ Bytes.length bts in
  (fun out buffer_len _ -> 
     if !len = 0 then           (* If our length is zero, then we're done reading from the string *)
       0                        (* So return 0, indicating we placed nothing in the buffer *)
     else if !len >= buffer_len then begin
       let buffer_len = buffer_len - 1 in
       unsafe_char_ptr_blit_bytes bts 0 out 0 buffer_len;
       len := !len - buffer_len;
       buffer_len
     end
     else if !len < buffer_len then begin
       let old_len = !len in
       unsafe_char_ptr_blit_bytes bts 0 out 0 !len;
       len := 0;
       old_len + 1
     end
     else raise (Failure ("Failed to pass string to Pandoc.")))

let reader_of_channel chan : reader =
  (fun out buffer_len _ ->
     let buffer_len = buffer_len - 1 in              (* Prepare for null-term injection *)
     let bts = Bytes.make buffer_len (Char.chr 0) in (* Temporary buffer *)
     let len = input chan bts 0 buffer_len in
     if len = 0 then
       0
     else begin
       unsafe_char_ptr_blit_bytes bts 0 out 0 len;
       len + 1                  (* Compensate for previous offset *)
     end)

let stdout_writer : writer = (fun in_buf len _ ->
    match in_buf with
    | Some s ->
      Printf.printf "%.*s" len s
    | None -> ()
  )

let buffer_writer buffer : writer =
  (fun in_buf len _ ->
     match in_buf with
     | Some s ->
       Buffer.add_bytes buffer s
     | None -> ()
  )


let fprintf_writer ~(f : 'b -> ('a, 'b, unit) format -> 'a) output : writer =
  (fun in_buf len _ ->
    match in_buf with
    | Some s ->
      let bts = Bytes.create len in
      Bytes.blit_string s 0 bts 0 len;
      f output "%.*s" len bts
    | None -> ()
  )

let printf_writer ~(f : ('a, 'b, unit) format -> 'a) output : writer =
  (fun in_buf len _ ->
    match in_buf with
    | Some s ->
      let bts = Bytes.create len in
      Bytes.blit_string s 0 bts 0 len;
      f output "%.*s" len bts
    | None -> ()
  )

let ksprintf_writer ~(f : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b) cont : writer =
  (fun in_buf len _ ->
     match in_buf with
     | Some s ->
       let bts = Bytes.create len in
       Bytes.blit_string s 0 bts 0 len;
       f cont "%.*s" len bts
     | None -> ()
  )

let kfprintf_writer ~(f : ('c -> 'a) -> 'c -> ('b, 'c, unit, 'a) format4 -> 'b) cont output : writer =
  (fun in_buf len _ ->
     match in_buf with
     | Some s ->
       let bts = Bytes.create len in
       Bytes.blit_string s 0 bts 0 len;
       f cont output "%.*s" len bts
     | None -> ()
  )
