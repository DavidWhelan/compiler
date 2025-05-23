(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let gen_annot = ref false
let gen_ml = ref false
let print_info_arg = ref false
let target_filename = ref None
let save_cmt_info = ref false

let print_version () =
  Format.printf "ocamlcmt, version %s@." Sys.ocaml_version;
  exit 0

let print_version_num () =
  Format.printf "%s@." Sys.ocaml_version;
  exit 0

let arg_list = Arg.align [
  "-o", Arg.String (fun s -> target_filename := Some s),
    "<file> Dump to file <file> (or stdout if -)";
  "-annot", Arg.Set gen_annot,
    " Generate the corresponding .annot file";
  "-save-cmt-info", Arg.Set save_cmt_info,
    " Encapsulate additional cmt information in annotations";
  "-src", Arg.Set gen_ml,
    " Convert .cmt or .cmti back to source code (without comments)";
  "-info", Arg.Set print_info_arg, " : print information on the file";
  "-version", Arg.Unit print_version,
              "     Print version and exit";
  "-vnum", Arg.Unit print_version_num,
           "        Print version number and exit";
  "-args", Arg.Expand Arg.read_arg,
    "<file> Read additional newline separated command line arguments \n\
    \      from <file>";
  "-args0", Arg.Expand Arg.read_arg0,
    "<file> Read additional NUL separated command line arguments from \n\
    \      <file>";
  "-I", Arg.String (fun s ->
    Clflags.include_dirs := s :: !Clflags.include_dirs),
    "<dir> Add <dir> to the list of include directories";
  ]

let arg_usage =
  "Read FILE.cmt and print related information\n\
   Usage: ocamlcmt [options] FILE.cmt\n\
   Options are:"

let dummy_crc = String.make 32 '-'

let print_info cmt =
  let oc = match !target_filename with
    | None -> stdout
    | Some filename -> open_out filename
  in
  let open Cmt_format in
  Printf.fprintf oc "module name: %s\n" cmt.cmt_modname;
  begin match cmt.cmt_annots with
    Packed (_, list) ->
      Printf.fprintf oc "pack: %s\n" (String.concat " " list)
  | Implementation _ -> Printf.fprintf oc "kind: implementation\n"
  | Interface _ -> Printf.fprintf oc "kind: interface\n"
  | Partial_implementation _ ->
    Printf.fprintf oc "kind: implementation with errors\n"
  | Partial_interface _ -> Printf.fprintf oc "kind: interface with errors\n"
  end;
  Printf.fprintf oc "command: %s\n"
    (String.concat " " (Array.to_list cmt.cmt_args));
  begin match cmt.cmt_sourcefile with
    None -> ()
  | Some name ->
    Printf.fprintf oc "sourcefile: %s\n" name;
  end;
  Printf.fprintf oc "build directory: %s\n" cmt.cmt_builddir;
  List.iter (Printf.fprintf oc "load path (visible): %s\n%!")
    cmt.cmt_loadpath.visible;
  List.iter (Printf.fprintf oc "load path (hidden): %s\n%!")
    cmt.cmt_loadpath.hidden;
  begin
    match cmt.cmt_source_digest with
      None -> ()
    | Some digest ->
      Printf.fprintf oc "source digest: %s\n" (Digest.to_hex digest);
  end;
  begin
    match cmt.cmt_interface_digest with
      None -> ()
    | Some digest ->
      Printf.fprintf oc "interface digest: %s\n" (Digest.to_hex digest);
  end;
  List.iter (fun (name, crco) ->
    let crc =
      match crco with
        None -> dummy_crc
      | Some crc -> Digest.to_hex crc
    in
    Printf.fprintf oc "import: %s %s\n" name crc;
  ) (List.sort compare cmt.cmt_imports);
  Printf.fprintf oc "%!";
  begin match !target_filename with
  | None -> ()
  | Some _ -> close_out oc
  end;
  ()

let generate_ml target_filename filename cmt =
  let (printer, ext) =
    match cmt.Cmt_format.cmt_annots with
      | Cmt_format.Implementation typedtree ->
          (fun ppf -> Pprintast.structure ppf
                                        (Untypeast.untype_structure typedtree)),
          ".ml"
      | Cmt_format.Interface typedtree ->
          (fun ppf -> Pprintast.signature ppf
                                        (Untypeast.untype_signature typedtree)),
          ".mli"
      | _ ->
        Printf.fprintf stderr "File was generated with an error\n%!";
          exit 2
  in
  let target_filename = match target_filename with
      None -> Some (filename ^ ext)
    | Some "-" -> None
    | Some _ -> target_filename
  in
  let oc = match target_filename with
      None -> None
    | Some filename -> Some (open_out filename) in
  let ppf = match oc with
      None -> Format.std_formatter
    | Some oc -> Format.formatter_of_out_channel oc in
  printer ppf;
  Format.pp_print_flush ppf ();
  match oc with
      None -> flush stdout
    | Some oc -> close_out oc

(* Save cmt information as faked annotations, attached to
   Location.none, on top of the .annot file. Only when -save-cmt-info is
   provided to ocaml_cmt.
*)
let record_cmt_info cmt =
  let location_none = {
    Location.none with Location.loc_ghost = false }
  in
  let location_file file = {
    Location.none with
      Location.loc_start = {
        Location.none.Location.loc_start with
          Lexing.pos_fname = file }}
  in
  let record_info name value =
    let ident = Printf.sprintf ".%s" name in
    Stypes.record (Stypes.An_ident (location_none, ident,
                                    Annot.Idef (location_file value)))
  in
  let open Cmt_format in
  List.iter (fun dir -> record_info "include" dir) cmt.cmt_loadpath.visible;
  List.iter (fun dir -> record_info "include" dir) cmt.cmt_loadpath.hidden;
  record_info "chdir" cmt.cmt_builddir;
  (match cmt.cmt_sourcefile with
    None -> () | Some file -> record_info "source" file)

let main () =
  Clflags.annotations := true;

  Arg.parse_expand arg_list  (fun filename ->
    if
      Filename.check_suffix filename ".cmt" ||
        Filename.check_suffix filename ".cmti"
    then begin
      let open Cmt_format in
      Compmisc.init_path ();
      let cmt = read_cmt filename in
      if !gen_annot then begin
        if !save_cmt_info then record_cmt_info cmt;
        let target_filename =
          match !target_filename with
          | None -> Some (filename ^ ".annot")
          | Some "-" -> None
          | Some _ as x -> x
        in
        Envaux.reset_cache ();
        List.iter (Load_path.add_dir ~hidden:false) cmt.cmt_loadpath.visible;
        List.iter (Load_path.add_dir ~hidden:true) cmt.cmt_loadpath.hidden;
        Cmt2annot.gen_annot target_filename
          ~sourcefile:cmt.cmt_sourcefile
          ~use_summaries:cmt.cmt_use_summaries
          cmt.cmt_annots
      end;
      if !gen_ml then generate_ml !target_filename filename cmt;
      if !print_info_arg || not (!gen_ml || !gen_annot) then print_info cmt;
    end else begin
      Printf.fprintf stderr
                     "Error: the file's extension must be .cmt or .cmti.\n%!";
      Arg.usage arg_list arg_usage
    end
  ) arg_usage

let main () =
  try
    main ()
  with x ->
    Printf.eprintf "Exception in main ()\n%!";
    Location.report_exception Format.err_formatter x;
    Format.fprintf Format.err_formatter "@.";
    exit 2

let _ = main ()
