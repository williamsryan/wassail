open Core

let codegen (cfg : unit Cfg.t) : unit Instr.t list =
  List.map cfg.instructions ~f:(fun label ->
      match Instr.Label.Map.find cfg.label_to_instr label with
      | Some instr -> instr
      | None ->
          failwith
            (Printf.sprintf "codegen: instruction not found: %s"
               (Instr.Label.to_string label)))
(* fst (codegen_until cfg cfg.entry_block (-1) IntSet.empty) *)

let cfg_to_func_inst (cfg : unit Cfg.t) : Func_inst.t =
  let body : unit Instr.t list = codegen cfg in
  {
    idx = cfg.idx;
    name = Some cfg.name;
    type_idx = cfg.type_idx;
    typ = (cfg.arg_types, cfg.return_types);
    code = { locals = cfg.local_types; body };
  }

module Test = struct
  let output_exactly_matches_input (module_str : string) : bool =
    let module_ = Wasm_module.of_string module_str in
    let cfg = Cfg_builder.build module_ 0l in
    let module_ = Wasm_module.replace_func module_ 0l (cfg_to_func_inst cfg) in
    let printed = Wasm_module.to_string module_ in
    if String.equal printed module_str then true
    else (
      Printf.printf "Output does not match, got:\n%s\n" printed;
      false)

  let%test "codegen for trivial module should generate all the code" =
    output_exactly_matches_input
      "(module\n\
      \  (type (;0;) (func (param i32) (result i32)))\n\
      \  (func (;0;) (type 0) (param i32) (result i32)\n\
      \    memory.size\n\
      \    memory.size\n\
      \    i32.store\n\
      \    memory.size\n\
      \    memory.size\n\
      \    i32.store)\n\
      \  (table (;0;) 1 1 funcref)\n\
      \  (memory (;0;) 2)\n\
      \  (global (;0;) (mut i32) (i32.const 66560))\n\
       )"

  let%test "codegen for nested blocks should generate all the code" =
    output_exactly_matches_input
      "(module\n\
      \  (type (;0;) (func (param i32) (result i32)))\n\
      \  (func (;0;) (type 0) (param i32) (result i32)\n\
      \    block\n\
      \      i32.const 0\n\
      \      drop\n\
      \      block\n\
      \        i32.const 1\n\
      \        br_if 0\n\
      \        i32.const 2\n\
      \        br 1\n\
      \      end\n\
      \      i32.const 3\n\
      \    end)\n\
      \  (table (;0;) 1 1 funcref)\n\
      \  (memory (;0;) 2)\n\
      \  (global (;0;) (mut i32) (i32.const 66560))\n\
       )"

  let%test "codegen for memory operations should produce the correct offset \
            and alignment when printed" =
    output_exactly_matches_input
      "(module\n\
      \  (type (;0;) (func (param i32) (result i32)))\n\
      \  (func (;0;) (type 0) (param i32) (result i32)\n\
      \    i32.const 1812\n\
      \    i32.const -1\n\
      \    i32.store offset=12\n\
      \    i32.const 1812\n\
      \    i32.const -1\n\
      \    i64.store offset=8 align=4\n\
      \    i32.const 1812\n\
      \    i32.const -1\n\
      \    i32.store16 align=1\n\
      \    i32.const 1812\n\
      \    i32.load16_u align=1)\n\
      \  (table (;0;) 1 1 funcref)\n\
      \  (memory (;0;) 2)\n\
      \  (global (;0;) (mut i32) (i32.const 66560))\n\
       )"

  let%test "codegen for consecutive blocks should produce the same result" =
    output_exactly_matches_input
      "(module\n\
      \  (type (;0;) (func (param i32) (result i32)))\n\
      \  (func (;0;) (type 0) (param i32) (result i32)\n\
      \    block\n\
      \      loop\n\
      \        i32.const 0\n\
      \        br_if 1\n\
      \        i32.const 0\n\
      \        br_if 0\n\
      \        block\n\n\
      \        end\n\
      \        block\n\
      \          i32.const 0\n\
      \          i32.const 1\n\
      \          i32.add\n\
      \          drop\n\
      \          i32.const 0\n\
      \          br_if 0\n\
      \        end\n\
      \      end\n\
      \    end\n\
      \    i32.const 0\n\
      \    drop\n\
      \    i32.const 0\n\
      \    drop)\n\
      \  (table (;0;) 1 1 funcref)\n\
      \  (memory (;0;) 2)\n\
      \  (global (;0;) (mut i32) (i32.const 66560))\n\
       )"

  let%test "codegen should support infinite loops" =
    output_exactly_matches_input
      "(module\n\
      \  (type (;0;) (func (param i32) (result i32)))\n\
      \  (func (;0;) (type 0) (param i32) (result i32)\n\
      \    (local i32)\n\
      \    i32.const 1\n\
      \    local.set 1\n\
      \    block\n\
      \      loop\n\
      \        local.get 0\n\
      \        i32.eqz\n\
      \        br_if 1\n\
      \        local.get 1\n\
      \        local.get 0\n\
      \        i32.mul\n\
      \        local.set 1\n\
      \        local.get 0\n\
      \        i32.const 1\n\
      \        i32.sub\n\
      \        local.set 0\n\
      \        br 0\n\
      \      end\n\
      \    end\n\
      \    local.get 1)\n\
       )"

  let%test "codegen should support empty ifs" =
    output_exactly_matches_input
      "(module\n\
      \  (type (;0;) (func (param i32) (result i32)))\n\
      \  (func (;0;) (type 0) (param i32) (result i32)\n\
      \    local.get 0\n\
      \    i32.const -1\n\
      \    i32.eq\n\
      \    if\n\n\
      \    end\n\
      \    local.get 0)\n\
       )"

  let%test "codegen should support if branches that jump to different places" =
    output_exactly_matches_input
      "(module\n\
      \  (type (;0;) (func (param i32) (result i32)))\n\
      \  (func (;0;) (type 0) (param i32) (result i32)\n\
      \    block\n\
      \      local.get 0\n\
      \      block\n\
      \        if\n\
      \          br 0\n\
      \        else\n\
      \          br 1\n\
      \        end\n\
      \      end\n\
      \      local.get 0\n\
      \    end)\n\
       )"
end
