open Core
open Wassail

(* Helper to write function call edge facts *)
let write_func_edge_fact out_channel src_func dst_func =
  Out_channel.output_string out_channel
    (Printf.sprintf "\"%s\"\t\"%s\"\n" src_func dst_func)

(* Helper to write instruction facts *)
let write_instr_fact out_channel func_name bb_name instr_name instr_str =
  Out_channel.output_string out_channel
    (Printf.sprintf "\"%s\"\t\"%s\"\t\"%s\"\t\"%s\"\n" func_name bb_name
       instr_name instr_str)

(* Helper to write CFG edge facts *)
let write_cfg_edge_fact out_channel func_name src dst =
  Out_channel.output_string out_channel
    (Printf.sprintf "\"%s\"\t\"bb_%d\"\t\"bb_%d\"\n" func_name src dst)

(* Function to generate Datalog facts from Wasm module and CFGs *)
let generate_datalog_facts (_wasm_module : Wasm_module.t)
    (cfgs : unit Cfg.t Int32Map.t) =
  (* Open channels for separate fact files *)
  let instruction_out_channel = Out_channel.create "instruction.facts" in
  let cfg_edge_out_channel = Out_channel.create "cfg_edge.facts" in
  let func_edge_out_channel = Out_channel.create "func_edge.facts" in
  (* Open channel for variable definitions and uses *)
  let var_def_use_out_channel = Out_channel.create "var_def_use.facts" in

  (* Iterate over functions and CFGs *)
  Int32Map.iteri cfgs ~f:(fun ~key:fid ~data:cfg ->
      let func_name = Printf.sprintf "func_%ld" fid in

      (* Generate function call facts *)
      IntMap.iteri cfg.edges ~f:(fun ~key:src ~data:edges ->
          Cfg.Edge.Set.iter edges ~f:(fun (dst, edge_data) ->
              match edge_data with
              | Some _ ->
                  (* Handle function calls here *)
                  let called_func_name = Printf.sprintf "func_%d" dst in
                  write_func_edge_fact func_edge_out_channel func_name
                    called_func_name
              | None ->
                  (* Handle normal intra-function control flow *)
                  write_cfg_edge_fact cfg_edge_out_channel func_name src dst));

      (* Iterate over basic blocks and their content *)
      IntMap.iteri cfg.basic_blocks ~f:(fun ~key:bb_idx ~data:bb ->
          let bb_name = Printf.sprintf "bb_%d" bb_idx in
          (* Check if the block is Data or Control and iterate over instructions *)
          match bb.content with
          | Data instrs ->
              List.iteri instrs ~f:(fun instr_id instr_labelled ->
                  let instr_name = Printf.sprintf "instr_%d" instr_id in
                  let instr_str =
                    Instr.to_mnemonic (Instr.Data instr_labelled)
                  in
                  write_instr_fact instruction_out_channel func_name bb_name
                    instr_name instr_str)
          | Control instr ->
              let instr_name = "control_instr" in
              let instr_str = Instr.control_to_short_string instr.instr in
              write_instr_fact instruction_out_channel func_name bb_name
                instr_name instr_str));

  Out_channel.close instruction_out_channel;
  Out_channel.close cfg_edge_out_channel;
  Out_channel.close func_edge_out_channel

(* Define the command for generating Datalog facts *)
let facts =
  Command.basic ~summary:"Generate Datalog facts from Wasm module"
    (let open Command.Let_syntax in
     let%map_open wasm_file = anon ("wasm-file" %: string) in
     fun () ->
       let start_time = Time_float.now () in
       let wasm_module = Wasm_module.of_file wasm_file in
       (* Generate CFGs for all functions in the module *)
       let cfgs = Cfg_builder.build_all wasm_module in
       (* Generate the Datalog facts *)
       generate_datalog_facts wasm_module cfgs;
       let end_time = Time_float.now () in
       Printf.printf "Datalog facts written to '.'.\n";
       Printf.printf "Time_float for 'Datalog facts generation': %s\n%!"
         (Time_float.Span.to_string (Time_float.diff end_time start_time)))
