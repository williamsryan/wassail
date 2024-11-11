open Helpers

let analyze_intra : Wasm_module.t -> Int32.t list -> Spec.t Cfg.t Int32Map.t =
  Analysis_helpers.mk_intra
    (fun _cfgs _wasm_mod -> Int32Map.empty)
    (fun _summaries _wasm_mod cfg -> cfg)

let analyze_intra1 (module_ : Wasm_module.t) (idx : Int32.t) : Spec.t Cfg.t =
  let results = analyze_intra module_ [ idx ] in
  match Int32Map.find results idx with
  | Some res -> res
  | None -> failwith "Spec_analysis.analyze_intra did not actually analyze"

module Test = struct
  let does_not_fail (module_str : string) : unit =
    let module_ = Wasm_module.of_string module_str in
    let _ : Spec.t Cfg.t = analyze_intra1 module_ 0l in
    ()

  let%test_unit "spec analysis does not error on trivial code" =
    does_not_fail
      "(module\n\
      \  (type (;0;) (func (param i32) (result i32)))\n\
      \  (func (;test;) (type 0) (param i32) (result i32)\n\
      \    i32.const 256\n\
      \    i32.const 512\n\
      \    i32.const 0\n\
      \    select)\n\
      \  (table (;0;) 1 1 funcref)\n\
      \  (memory (;0;) 2)\n\
      \  (global (;0;) (mut i32) (i32.const 66560)))"

  let%test_unit "spec analysis suceeds on simple program with if and globals" =
    does_not_fail
      "(module\n\
      \  (type (;0;) (func))\n\
      \  (func (;test;) (type 0)\n\
      \    global.get 0\n\
      \    if\n\
      \      global.get 0\n\
      \      i32.const 1\n\
      \      i32.sub\n\
      \      global.set 0\n\
      \    end)\n\
      \  (table (;0;) 1 1 funcref)\n\
      \  (memory (;0;) 2)\n\
      \  (global (;0;) (mut i32) (i32.const 66560)))"

  let%test_unit "spec analysis suceeds with imported globals" =
    does_not_fail
      "(module\n\
      \  (type (;0;) (func))\n\
      \  (import \"env\" \"DYNAMICTOP_PTR\" (global (;0;) i32))\n\
      \  (func (;test;) (type 0)\n\
      \    global.get 1\n\
      \    global.set 0)\n\
      \  (table (;0;) 1 1 funcref)\n\
      \  (memory (;0;) 2)\n\
      \  (global (;0;) (mut i32) (i32.const 66560)))"

  let%test_unit "spec analysis succeeds with blocks" =
    does_not_fail
      "(module\n\
      \  (type (;0;) (func (param i32) (result i32)))\n\
      \  (func (;test;) (type 0) (param i32) (result i32)\n\
      \    block\n\
      \      i32.const 1 ;; This is a branch condition\n\
      \      br_if 0     ;; The condition depends on var 'Const 1', and this \
       block has index 3\n\
      \      i32.const 2\n\
      \      local.get 0\n\
      \      i32.add\n\
      \      drop\n\
      \    end\n\
      \    local.get 0))"

  let%test_unit "spec analysis succeeds even in the presence of unreachable \
                 code" =
    does_not_fail
      "(module\n\
       (type (;0;) (func (param i32) (result i32)))\n\
       (func (;0;) (type 0) (param i32) (result i32)\n\
      \    block\n\
      \      local.get 0\n\
      \      br_if 0\n\
      \      unreachable ;; stack length here is 0, and it is connected to the \
       exit of the function\n\
      \    end\n\
      \    local.get 0 ;; stack length here is 1, hence there is a length \
       mismatch\n\
       ))"

  let%test_unit "spec analysis succeeds even in the presence of unreachable \
                 code" =
    does_not_fail
      "(module\n\
       (type (;0;) (func (param i32) (result i32)))\n\
       (func (;0;) (type 0) (param i32) (result i32)\n\
      \    (local i32 i32)\n\
      \    block\n\
      \      i32.const 3\n\
      \      local.get 0\n\
      \      local.set 1\n\
      \      br 0\n\
      \      i32.const 1 ;; unreachable\n\
      \      local.set 0 ;; also unreachable\n\
      \    end\n\
      \    local.get 1))"

  let%test_unit "spec analysis succeeds even in the presence of \
                 stack-polymorphic instructions" =
    does_not_fail
      "(module\n\
       (type (;0;) (func (param i32) (result i32)))\n\
       (func (;0;) (type 0) (param i32) (result i32)\n\
      \    (local i32 i32)\n\
      \    block (result i32)\n\
      \      i32.const 0\n\
      \      if\n\
      \        i32.const 1\n\
      \        i32.const 2\n\
      \        i32.const 3\n\
      \        br 0\n\
      \      else\n\
      \      end\n\
      \      i32.const 4\n\
      \    end))"

  let%test_unit "spec analysis succeeds on example from the wild" =
    does_not_fail
      "(module\n\
       (type (;0;) (func (param i32 i32) (result i32)))\n\
       (func (;0;) (type 0) (param i32 i32) (result i32)\n\
      \    (local i32)\n\
      \    local.get 0 ;; [_]\n\
      \    if (result i32) ;; [] INSTR 1\n\
      \      block (result i32) ;; INSTR 2\n\
      \        i32.const 8 ;; [_]\n\
      \        local.tee 2 ;; [_]\n\
      \        drop ;; []\n\
      \        local.get 2 ;; [_]\n\
      \        local.tee 1  ;; [_]\n\
      \      end\n\
      \      if (result i32) ;; [] ;; INSTR 8\n\
      \        local.get 0 ;; [_]\n\
      \        local.get 1 ;; [_, _]\n\
      \        i32.store ;; []\n\
      \        i32.const 0 ;; [_]\n\
      \      else\n\
      \        i32.const -16 ;; [_]\n\
      \      end\n\
      \    else\n\
      \      i32.const -10420289 ;; [_]\n\
      \    end)\n\
       )"
end
