(* OASIS_START *)
(* DO NOT EDIT (digest: d41d8cd98f00b204e9800998ecf8427e) *)
(* OASIS_STOP *)

let syscall cmd =
  let ic, oc = Unix.open_process cmd in
  let buf = Buffer.create 16 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  let _ = Unix.close_process (ic, oc) in
  (Buffer.contents buf)
;;

Unix.putenv "OCAMLFIND_IGNORE_DUPS_IN"
  ((String.trim @@ syscall "opam config var lib") ^ "/ocaml/compiler-libs")
;;

open Ocamlbuild_plugin;;
let () =
  dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
       | After_rules ->
           let env = BaseEnvLight.load ~allow_empty:true ~filename:MyOCamlbuildBase.env_filename () in

           (* Determine extension of CompiledObject: best *)
           let native_suffix =
             if BaseEnvLight.var_get "is_native" env = "true"
             then "native" else "byte"
           in
           
           (* Sedlex extensions *)
           flag ["ocaml"; "compile"; "ppx_sedlex"] &
           S [A "-ppx"; A ("sedlex/src/syntax/ppx_sedlex." ^ native_suffix)];
       | _ -> ())
