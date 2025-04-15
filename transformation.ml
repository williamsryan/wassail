open Core

let rewrite_static =
  Command.basic
    ~summary:
      "Rewrite Wasm binary to relocate constant loads to new data segment"
    (let%map_open.Command input = anon ("input" %: string)
     and output =
       flag "-o" (optional string) ~doc:"Output path for transformed Wasm"
     and facts_file =
       flag "-facts" (optional string) ~doc:"Path to memory_access.facts"
     in
     fun () ->
       let input_wasm = input in
       let output_path =
         Option.value output ~default:(input_wasm ^ ".rewritten.wasm")
       in

       let accesses =
         match facts_file with
         | Some path -> Transformation_logic.load_static_memory_entries path
         | None -> Db_interface.get_static_memory_accesses input_wasm
       in

       Transformation_logic.rewrite input_wasm accesses output_path)
