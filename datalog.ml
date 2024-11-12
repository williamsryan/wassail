open Core
open Wassail

(* Helper to write function call edge facts *)
let write_func_edge_fact out_channel src_func dst_func =
  Out_channel.output_string out_channel
    (Printf.sprintf "%s\t%s\n" src_func dst_func)

(* Helper to write instruction facts *)
let write_instr_fact out_channel func_name bb_name instr_name instr_str =
  Out_channel.output_string out_channel
    (Printf.sprintf "%s\t%s\t%s\t%s\n" func_name bb_name instr_name instr_str)

(* Helper to write CFG edge facts *)
let write_cfg_edge_fact out_channel func_name src dst annotation =
  Out_channel.output_string out_channel
    (Printf.sprintf "%s\tbb_%d\tbb_%d\t%s\n" func_name src dst annotation)

(* Helper to write variable def-use facts *)
let write_var_def_use_fact out_channel use_or_def func_name bb_name instr_index
    offset =
  Out_channel.output_string out_channel
    (Printf.sprintf "%s\t%s\t%s\t%d\t%ld\n" use_or_def func_name bb_name
       instr_index offset)

(* Helper to write memory access facts *)
let write_memory_access_fact out_channel kind func_name bb_name instr_name
    memop_str effective_offset =
  Out_channel.output_string out_channel
    (Printf.sprintf "%s\t%s\t%s\t%s\t%s\t%ld\n" kind func_name bb_name
       instr_name memop_str effective_offset)

(* Function to calculate effective offset by combining base address and inline offset *)
let calculate_effective_offset base_address (memop : Memoryop.t) : int32 =
  let instr_offset = Int32.of_int_exn memop.offset in
  let effective_offset = Int32.(base_address + instr_offset) in
  Printf.printf
    "Calculating effective offset:\n\
    \  Base address: %ld\n\
    \  Memop offset: %d\n\
    \  Effective offset: %ld\n"
    base_address memop.offset effective_offset;
  effective_offset

(* Function to generate Datalog facts from Wasm module and CFGs *)
let generate_datalog_facts (_wasm_module : Wasm_module.t)
    (cfgs : unit Cfg.t Int32Map.t) =
  (* Open channels for separate fact files *)
  let instruction_out_channel = Out_channel.create "instruction.facts" in
  let cfg_edge_out_channel = Out_channel.create "cfg_edge.facts" in
  let func_edge_out_channel = Out_channel.create "func_edge.facts" in
  let var_def_use_out_channel = Out_channel.create "var_def_use.facts" in
  let memory_access_out_channel = Out_channel.create "memory_access.facts" in

  (* Track the last two relevant instructions for Store operations *)
  let stack = ref [] in

  (* Helper to print the current stack for debugging *)
  let print_stack () =
    Printf.printf "Current stack (top to bottom): %s\n"
      (String.concat ~sep:", " (List.map !stack ~f:Int32.to_string))
  in

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
                    called_func_name;
                  (* Also write CFG edge with annotation *)
                  write_cfg_edge_fact cfg_edge_out_channel func_name src dst
                    "call"
              | None ->
                  (* Write CFG edge without annotation *)
                  write_cfg_edge_fact cfg_edge_out_channel func_name src dst ""));

      (* Iterate over basic blocks and their content *)
      IntMap.iteri cfg.basic_blocks ~f:(fun ~key:bb_id ~data:bb ->
          let bb_name = Printf.sprintf "bb_%d" bb_id in

          (* Print the string representation of the whole block *)
          (* let bb_str = Basic_block.to_string bb in *)
          (* Printf.printf "Basic Block: %s\n" bb_str; *)

          (* Check if the block is Data or Control and iterate over instructions *)
          match bb.content with
          | Data instrs ->
              List.iteri instrs ~f:(fun instr_index instr_labelled ->
                  let instr_name = Printf.sprintf "instr_%d" instr_index in
                  let instr_str =
                    Instr.to_mnemonic (Instr.Data instr_labelled)
                  in
                  write_instr_fact instruction_out_channel func_name bb_name
                    instr_name instr_str;

                  let instr = instr_labelled.instr in

                  (* Handle stack updates for i32.const instructions *)
                  (match instr with
                  | Const (I32 x) ->
                      Printf.printf "Pushing to stack: %ld\n" x;
                      stack := x :: !stack;
                      print_stack ()
                  | _ -> ());

                  (* Generate memory access facts based on instruction type *)
                  match instr with
                  | Load memop -> (
                      Printf.printf
                        "\n--- Load Instruction Encountered (%s) ---\n"
                        (Instr.data_to_string instr);
                      print_stack ();
                      match !stack with
                      | base :: _ ->
                          let effective_offset =
                            calculate_effective_offset base memop
                          in
                          let memop_str = Memoryop.to_string memop in
                          Printf.printf
                            "Load instruction:\n\
                            \  Base address: %ld\n\
                            \  Memop offset: %d\n\
                            \  Effective offset: %ld\n"
                            base memop.offset effective_offset;
                          write_memory_access_fact memory_access_out_channel
                            "load" func_name bb_name instr_name memop_str
                            effective_offset;
                          (* Pop the address from the stack after use *)
                          stack := List.tl_exn !stack
                      | _ ->
                          Printf.printf
                            "No base address set for Load instruction.\n")
                  | Store memop -> (
                      Printf.printf
                        "\n--- Store Instruction Encountered (%s) ---\n"
                        (Instr.data_to_string instr);
                      print_stack ();
                      match !stack with
                      | base :: value :: _ ->
                          let effective_offset =
                            calculate_effective_offset base memop
                          in
                          let memop_str = Memoryop.to_string memop in
                          Printf.printf
                            "Store instruction:\n\
                            \  Base address: %ld\n\
                            \  Value: %ld\n\
                            \  Memop offset: %d\n\
                            \  Effective offset: %ld\n"
                            base value memop.offset effective_offset;
                          write_memory_access_fact memory_access_out_channel
                            "store" func_name bb_name instr_name memop_str
                            effective_offset;
                          (* Pop the value and address from the stack after use *)
                          stack := List.drop !stack 2
                      | _ ->
                          Printf.printf
                            "Insufficient values on stack for Store instruction.\n"
                      )
                  | _ -> ())
          | Control instr ->
              let instr_name = "control_instr" in
              let instr_str = Instr.control_to_short_string instr.instr in
              write_instr_fact instruction_out_channel func_name bb_name
                instr_name instr_str));

  Out_channel.close instruction_out_channel;
  Out_channel.close cfg_edge_out_channel;
  Out_channel.close func_edge_out_channel;
  Out_channel.close var_def_use_out_channel;
  Out_channel.close memory_access_out_channel

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
