open Core_kernel

module Make = functor (Spec : Spec_inference.SPEC) -> struct
  (* TODO: Make summaries. A summary is the final state restricted to only reachable vars: args, globals, mems and return value. *)

  type state = Taint_domain.t
  [@@deriving compare]

  module SummaryManager = Summary.MakeManager(Taint_summary)
  type summary = Taint_summary.t

  (** In the initial state, we only set the taint for for parameters and the globals. *)
  let init_state (cfg : Cfg.t) : state =
    Spec_inference.VarMap.of_alist_exn
      ((List.mapi cfg.arg_types ~f:(fun i _ -> (Spec_inference.Local i,
                                                Taint_domain.taint (Spec_inference.Local i)))) @
       (List.mapi cfg.global_types ~f:(fun i _ -> (Spec_inference.Global i,
                                                 Taint_domain.taint (Spec_inference.Global i)))))

  let bottom_state (_cfg : Cfg.t) : state = Taint_domain.bottom

  let state_to_string (s : state) : string = Taint_domain.to_string s

  let join_state _mod _cfg _block (s1 : state) (s2 : state) : state = Taint_domain.join s1 s2

  let init_summaries s = SummaryManager.init s

  let data_instr_transfer (_module_ : Wasm_module.t) (_cfg : Cfg.t) (i : Instr.data Instr.labelled) (state : state) : state =
    match i.instr with
    | Nop | MemorySize | Drop | MemoryGrow -> state
    | Select ->
      let ret = Spec.ret i.label in
      let (_c, v2, v1) = Spec.pop3 (Spec.pre i.label).vstack in
      (* TODO: could improve precision by checking the constraints on c: if it is precisely zero/not-zero, we can only include v1 or v2 *)
      Taint_domain.add_taint (Taint_domain.add_taint state ret v1) ret v2
    | LocalGet l ->
      Taint_domain.add_taint state (Spec.ret i.label) (Spec.get_nth (Spec.pre i.label).locals l)
    | LocalSet l ->
      Taint_domain.add_taint state (Spec.get_nth (Spec.pre i.label).locals l) (Spec.pop (Spec.pre i.label).locals)
    | LocalTee l ->
      Taint_domain.add_taint
        (Taint_domain.add_taint state (Spec.get_nth (Spec.pre i.label).locals l) (Spec.pop (Spec.pre i.label).locals))
        (Spec.ret i.label) (Spec.get_nth (Spec.pre i.label).locals l)
    | GlobalGet g ->
        Taint_domain.add_taint state (Spec.ret i.label) (Spec.get_nth (Spec.pre i.label).globals g)
    | GlobalSet g ->
      Taint_domain.add_taint state (Spec.get_nth (Spec.pre i.label).globals g) (Spec.pop (Spec.pre i.label).globals)
    | Const _ -> state
    | Binary _ | Compare _ ->
      let v1, v2 = Spec.pop2 (Spec.pre i.label).vstack in
      Taint_domain.add_taint
        (Taint_domain.add_taint state (Spec.ret i.label) v1)
        (Spec.ret i.label) v2
    | Unary _ | Test _ | Convert _ ->
      Taint_domain.add_taint state (Spec.ret i.label) (Spec.pop (Spec.pre i.label).vstack)
    | Load _ -> failwith "TODO: load"
    | Store _ -> failwith "TODO: store"

  let control_instr_transfer
      (_module_ : Wasm_module.t) (* The wasm module (read-only) *)
      (_cfg : Cfg.t) (* The CFG analyzed *)
      (i : Instr.control Instr.labelled) (* The instruction *)
      (state : state) (* The pre state *)
    : [`Simple of state | `Branch of state * state] =
    let apply_summary (f : int) (arity : int * int) (state : state) : state =
      let summary = SummaryManager.get f in
      let args = List.take (Spec.pre i.label).vstack (fst arity) in
      let ret = if snd arity = 1 then List.hd (Spec.post i.label).vstack else None in
      let globals = (Spec.post i.label).globals in
      Taint_summary.apply summary state args globals ret
    in
    match i.instr with
    | Call (arity, f) ->
      `Simple (apply_summary f arity state)
    | CallIndirect (_arity, _typ) ->
      (* TODO: we could rely on the constraints to know which function is called *)
      failwith "taint_transfer: call_indirect"
    | Br _ -> `Simple state
    | BrIf _ | If _ -> `Branch (state, state)
    | Return -> `Simple state
    | Unreachable -> `Simple state
    | _ -> `Simple state

  let merge_flows (module_ : Wasm_module.t) (cfg : Cfg.t) (block : Basic_block.t) (states : (int * state) list) : state =
    match states with
    | [] -> init_state cfg
    | _ ->
      (* one or multiple states *)
        begin match block.content with
          | ControlMerge ->
            (* block is a control-flow merge *)
            let spec = Spec.post_block block.idx in
            let states' = List.map states ~f:(fun (idx, s) ->
                (* get the spec after that state *)
                let spec' = Spec.post_block idx in
                (* equate all different variables in the post-state with the ones in the pre-state *)
                List.fold_left (Spec_inference.extract_different_vars spec spec')
                  ~init:s
                  ~f:(fun s (x, y) ->
                      (* TODO: should it be x y or y x? *)
                      Taint_domain.add_taint s x y)) in
            (* And finally joins all the states *)
            List.reduce_exn states' ~f:(join_state module_ cfg block)
          | _ ->
            (* Not a control-flow merge, there should be a single predecessor *)
            begin match states with
              | (_, s) :: [] -> s
              | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
            end
        end

  let summary (cfg : Cfg.t) (out_state : state) : summary =
    Taint_summary.make cfg out_state
      (if List.length cfg.return_types = 1 then List.hd (Spec.post_block cfg.exit_block).vstack else None)
      (Spec.post_block cfg.exit_block).globals
end