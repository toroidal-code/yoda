
exception PandocError of string

(** Initializes the Haskell runtime. Every call to this function
  * should be matched with exactly one call to `pandoc_exit` *)
val start : unit -> unit

(** Shuts down the Haskell runtime *)
val stop : unit -> unit

(** The type of the reader function.
  *
  * The reader function must fill the buffer up with input to
  * to be converted using Pandoc.
  * 
  * @param buf [bytes] The buffer to be filled.
  * @param len [int] The size of the buffer to copy into.
  * @param ?user_data [ptr void] The same pointer passed as the 
  *                             last argument of the `pandoc` function.
  * @return The number of characters read into the buffer. *)
type reader = char Ctypes.ptr -> int -> unit Ctypes.ptr -> int

(** The type of the writer function.
  * 
  * The writer function must write the contents of the passed buffer
  * as the output of the conversion by Pandoc.
  *  
  * @param buf [string] The buffer to be written.
  * @param len [int] The number of elements in the buffer.
  * @param ?user_data [ptr void] The same pointer passed as the last 
  *                             argument of the `pandoc` function.
  * @return void  *)
type writer = char Ctypes.ptr option -> int -> unit Ctypes.ptr -> unit

(** Calls `pandoc` with given input and output formats and streams.
  * @param ?size The size of the buffer in bytes. Defaults to 1024.
  * @param ?settings Settings is an XML string conforming to a schema
  *                  distributed with `libpandoc`. Empty by default.
  * @param ?user Custom user data. Currently unused.
  * @param ~input The format of input. See `pandoc --help` for more information.
  * @param ~output The format of output. See `pandoc --help` for more information.
  * @param ~reader The function to read data into pandoc.
  * @param ~writer The function to write data out of pandoc
  * @return `NULL` on success, or a string containing an error message
  *         on failure. *)
val pandoc :
  ?size:int ->
  ?user_data:unit Ctypes.ptr ->
  ?settings:string ->
  input_format:string ->
  output_format:string ->
  reader ->
  writer -> unit

(* val valid_language : string -> bool  *)

(* val highlight : *)
(*   ?size:int -> *)
(*   ?user_data:unit Ctypes.ptr -> *)
(*   string -> *)
(*   ?output_format:string -> *)
(*   ?block:bool -> reader -> writer -> unit *)

(* val reader_of_string : string -> reader *)
val reader_of_bytes : bytes -> reader
(* val reader_of_channel : in_channel -> reader *)
val stdout_writer : writer
val buffer_writer : Buffer.t -> writer
val fprintf_writer : f:('b -> (int -> bytes -> unit, 'b, unit) format -> int -> bytes -> unit) -> 'b -> writer
val printf_writer : f:((string -> int -> bytes -> unit, 'b, unit) format -> string -> int -> bytes -> unit) -> (string -> int -> bytes -> unit, 'b, unit) format -> writer
val ksprintf_writer : f:((string -> unit) -> (int -> bytes -> unit, unit, string, unit) format4 -> int -> bytes -> unit) -> (string -> unit) -> writer
val kfprintf_writer : f: (('c -> unit) -> 'c -> (int -> bytes -> unit, 'c, unit, unit) format4 -> int -> bytes -> unit) -> ('c -> unit) -> 'c  -> writer
