open Wassail
open Core

let load_bounds path =
  let bounds = Hashtbl.create 64 in
  let ic = open_in path in
  try
    while true do
      let line = input_line ic in
      match String.split_on_char '\t' line with
      | [ modname; func; bb; instr; lineno; addr; bound ] ->
          let key =
            Printf.sprintf "%s~%s~%s~%s~%s" modname func bb instr lineno
          in
          Hashtbl.add bounds key bound
      | _ -> ()
    done;
    bounds
  with End_of_file ->
    close_in ic;
    bounds

let insert_upper_bound_check instrs bound_func_idx bound =
  match instrs with
  | addr_instr :: value_instr :: Store memop :: rest ->
      [
        Const (Values.I32 (Int32.of_string bound));
        Call bound_func_idx;
        addr_instr;
        value_instr;
        Store memop;
      ]
      @ rest
  | _ -> instrs

let transform_module (m : module_) ~bound_func_idx ~bounds =
  List.map
    (fun f ->
      match f with
      | FuncDef fd ->
          let updated_instrs = ref [] in
          List.iteri
            (fun i instr ->
              let key =
                (* Construct your unique key here, e.g. using function + instr index *)
                ""
              in
              match instr with
              | Store _ when Hashtbl.mem bounds key ->
                  let bound = Hashtbl.find bounds key in
                  updated_instrs :=
                    !updated_instrs
                    @ insert_upper_bound_check
                        [ List.nth fd.body i ]
                        bound_func_idx bound
              | _ -> updated_instrs := !updated_instrs @ [ instr ])
            fd.body;
          FuncDef { fd with body = !updated_instrs }
      | _ -> f)
    m
