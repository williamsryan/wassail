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

(* Helper to write memory access facts *)
let write_memory_access_fact out_channel kind func_name bb_name instr_name
    effective_offset instr_location =
  Out_channel.output_string out_channel
    (Printf.sprintf "%s\t%s\t%s\t%s\t%ld\t%s\n" kind func_name bb_name
       instr_name effective_offset instr_location)

(* Helper to write constant facts *)
let write_constant_fact out_channel instr_name value purpose =
  Out_channel.output_string out_channel
    (Printf.sprintf "%s\t%ld\t%s\n" instr_name value purpose)

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

(* Helper to format instruction location *)
let format_instr_location (instr_labelled : ('a, 'b) Instr.T.labelled) =
  let label = instr_labelled.Instr.T.label in
  let line_number = instr_labelled.Instr.T.line_number in
  Printf.sprintf "%s:%d" (Instr.Label.to_string label) line_number

(* Retrieve function names and indices from wasm_module *)
let get_function_index_map (wasm_module : Wasm_module.t) =
  List.foldi wasm_module.funcs ~init:String.Map.empty ~f:(fun idx map func ->
      match func.name with
      | Some name -> Map.set map ~key:name ~data:(Int32.of_int_exn idx)
      | None -> map)

(* Helper to classify constant based on context *)
let classify_instruction instr =
  match instr with
  | Instr.Binary binop -> (
      match binop.op with
      | Add | Sub | Mul | Div | And | Or | Xor -> "computation"
      | _ -> "unknown")
  | Instr.Load _ | Instr.Store _ -> "memory_operation"
  | _ -> "unknown"

(* Function to generate Datalog facts from Wasm module and CFGs *)
let generate_datalog_facts (_wasm_module : Wasm_module.t)
    (cfgs : unit Cfg.t Int32Map.t) (output_dir : string) =
  (* Safely create the output directory and any missing parent directories *)
  let create_directory dir =
    try Core_unix.mkdir_p dir
    with Core_unix.Unix_error (err, _, _) ->
      Printf.eprintf "Error creating directory '%s': %s\n" dir
        (Core_unix.Error.message err);
      exit 1
  in

  (* Ensure the output directory exists *)
  create_directory output_dir;

  (* Open channels for separate fact files *)
  let instruction_out_channel =
    Out_channel.create (Filename.concat output_dir "instruction.facts")
  in
  let cfg_edge_out_channel =
    Out_channel.create (Filename.concat output_dir "cfg_edge.facts")
  in
  let func_edge_out_channel =
    Out_channel.create (Filename.concat output_dir "func_edge.facts")
  in
  let memory_access_out_channel =
    Out_channel.create (Filename.concat output_dir "memory_access.facts")
  in
  let constant_out_channel =
    Out_channel.create (Filename.concat output_dir "constant.facts")
  in

  (* Map to store function name to index for resolving symbolic calls *)
  let _func_name_to_index = get_function_index_map _wasm_module in

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

      (* Generate CFG edges without annotation for non-call edges *)
      IntMap.iteri cfg.edges ~f:(fun ~key:src ~data:edges ->
          Cfg.Edge.Set.iter edges ~f:(fun (dst, _) ->
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
                  let instr_name =
                    Printf.sprintf "instr_%ld_%d" fid instr_index
                  in
                  let instr_str =
                    Instr.to_mnemonic (Instr.Data instr_labelled)
                  in
                  write_instr_fact instruction_out_channel func_name bb_name
                    instr_name instr_str;

                  let instr = instr_labelled.instr in
                  let next_instr = List.nth instrs (instr_index + 1) in

                  (* Handle stack updates for i32.const instructions *)
                  match instr with
                  | Const (I32 x) ->
                      Printf.printf "Pushing to stack: %ld\n" x;
                      stack := x :: !stack;
                      print_stack ();
                      (* Classify the constant based on next_instr *)
                      let const_purpose =
                        match next_instr with
                        | Some next -> classify_instruction next.instr
                        | None -> "unknown"
                      in
                      Printf.printf "Classifying constant %ld as %s\n" x
                        const_purpose;

                      (* Write constant facts *)
                      write_constant_fact constant_out_channel instr_name x
                        const_purpose
                  (* Generate memory access facts based on instruction type *)
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
                          let instr_location =
                            format_instr_location instr_labelled
                          in
                          let _memop_str = Memoryop.to_string memop in
                          Printf.printf
                            "Load instruction:\n\
                            \  Base address: %ld\n\
                            \  Memop offset: %d\n\
                            \  Effective offset: %ld\n\
                            \  Location: %s\n"
                            base memop.offset effective_offset instr_location;
                          write_memory_access_fact memory_access_out_channel
                            "load" func_name bb_name instr_name effective_offset
                            instr_location;
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
                      | value :: base :: _ ->
                          let effective_offset =
                            calculate_effective_offset base memop
                          in
                          let instr_location =
                            format_instr_location instr_labelled
                          in
                          let _memop_str = Memoryop.to_string memop in
                          Printf.printf
                            "Store instruction:\n\
                            \  Base address: %ld\n\
                            \  Value: %ld\n\
                            \  Memop offset: %d\n\
                            \  Effective offset: %ld\n\
                            \  Location: %s\n"
                            base value memop.offset effective_offset
                            instr_location;
                          write_memory_access_fact memory_access_out_channel
                            "store" func_name bb_name instr_name
                            effective_offset instr_location;
                          (* Pop the value and address from the stack after use *)
                          stack := List.drop !stack 2
                      | _ ->
                          Printf.printf
                            "Insufficient values on stack for Store instruction.\n"
                      )
                  | _ -> ())
          | Control instr_labelled -> (
              (* Process a single control instruction, specifically handling Call *)
              let instr_name = "control_instr" in
              let instr_str =
                Instr.control_to_short_string instr_labelled.instr
              in
              write_instr_fact instruction_out_channel func_name bb_name
                instr_name instr_str;

              let instr = instr_labelled.instr in

              (* Handling Call instructions within control flow *)
              match instr with
              | Call (_, _, target_func_id) ->
                  let target_func_name =
                    Printf.sprintf "func_%ld" target_func_id
                  in
                  Printf.printf "Detected call from %s to %s\n" func_name
                    target_func_name;
                  write_func_edge_fact func_edge_out_channel func_name
                    target_func_name
              | _ -> ())));

  Out_channel.close instruction_out_channel;
  Out_channel.close cfg_edge_out_channel;
  Out_channel.close func_edge_out_channel;
  Out_channel.close memory_access_out_channel

(* Define the command for generating Datalog facts *)
let facts =
  Command.basic ~summary:"Generate Datalog facts from Wasm module"
    (let open Command.Let_syntax in
     let%map_open wasm_file = anon ("wasm-file" %: string)
     and output_dir =
       flag "output-dir" (optional string)
         ~doc:
           "DIR Directory to store the generated fact files (default: current \
            directory)"
     in
     fun () ->
       let output_dir = Option.value output_dir ~default:"." in
       let start_time = Time_float.now () in
       let wasm_module = Wasm_module.of_file wasm_file in
       (* Generate CFGs for all functions in the module *)
       let cfgs = Cfg_builder.build_all wasm_module in
       (* Generate the Datalog facts *)
       generate_datalog_facts wasm_module cfgs output_dir;
       let end_time = Time_float.now () in
       Printf.printf "Datalog facts written to '%s'.\n" output_dir;
       Printf.printf "Time_float for 'Datalog facts generation': %s\n%!"
         (Time_float.Span.to_string (Time_float.diff end_time start_time)))
