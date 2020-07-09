open Core_kernel
open Helpers

module Make = functor (Spec : Spec_inference.SPEC) -> struct
  let summaries : Summary.t IntMap.t ref = ref IntMap.empty

  type state = Domain.state
  [@@deriving compare]

type result =
  | Uninitialized
  | Simple of state
  | Branch of state * state
[@@deriving compare]

let init_state (cfg : Cfg.t) = Domain.init cfg Spec.vars

let bottom_state (cfg : Cfg.t) = Domain.bottom cfg (List.map ~f:Spec_inference.var_to_string Spec.vars)

let state_to_string = Domain.to_string

let join_state (_module_ : Wasm_module.t) (_cfg : Cfg.t) (_block : Basic_block.t) =
  Domain.join

(** Merges the entry states before analyzing the given block *)
let merge_flows (module_ : Wasm_module.t) (cfg : Cfg.t) (block : Basic_block.t) (states : (int * state) list) : state =
    match states with
    | [] -> (* no in state, use init *)
      init_state cfg
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
              Domain.add_constraints s (List.map
                                          ~f:(fun (x, y) -> (Spec_inference.var_to_string x, Spec_inference.var_to_string y))
                                          (Spec_inference.extract_different_vars spec spec'))) in
          (* And finally joins all the states *)
          List.reduce_exn states' ~f:(join_state module_ cfg block)
        | _ ->
          (* Not a control-flow merge, there should be a single predecessor *)
          begin match states with
            | (_, s) :: [] -> s
            | _ -> failwith (Printf.sprintf "Invalid block with multiple input states: %d" block.idx)
          end
      end

(** Transfer function for data instructions.
   @param i the instruction for which to compute the transfer function
   @param state the state before the instruction (prestate).
   @param vstack_spec: the specification of what the vstack looks like after execution
   @return the resulting state (poststate).
*)
let data_instr_transfer (module_ : Wasm_module.t) (_cfg : Cfg.t) (i : Instr.data Instr.labelled) (state : state) : state =
  match i.instr with
  | Nop -> state
  | MemorySize ->
    (* memory size is bounded by the initial memory size and the maximum memory size *)
    let mem = List.nth_exn module_.mems 0 in
    (* add ret = [min,max] where min and max are the memory size bounds *)
    Domain.add_interval_constraint state (Spec.ret i.label)
      (mem.min_size,
       (match mem.max_size with
        | Some max -> max
        | None -> failwith "unsupported infinite max size" (* can easily supported, the constraint just becomes ret >= min *)))
  | Drop -> state
  | Select ->
    let ret = Spec.ret i.label in
    let vstack = (Spec.pre i.label).vstack in
    let (c, v2, v1) = Spec.pop3 vstack in
    begin
      match Domain.is_zero state (Spec_inference.var_to_string c) with
      | (true, false) -> (* definitely 0, add ret = v2 *)
        Domain.add_equality_constraint state ret v2
      | (false, true) -> (* definitely not 0, add ret = v2 *)
        Domain.add_equality_constraint state ret v1
      | (true, true) -> (* could be 0 or not 0, join both constraints *)
        Domain.join
          (Domain.add_equality_constraint state ret v1)
          (Domain.add_equality_constraint state ret v2)
      | (false, false) -> (* none, add bottom constraint *)
        Domain.add_interval_constraint state ret (-1, 1)
    end
  | LocalGet l ->
    (* add ret = ln where ln is the local accessed *)
    Domain.add_equality_constraint state (Spec.ret i.label) (Spec.get_nth (Spec.pre i.label).locals l)
  | LocalSet l ->
    let local = Spec.get_nth (Spec.pre i.label).locals l in
    (* add ln' = v where ln' is the variable for the local set and v is the top of the vstack *)
    let v = Spec.pop (Spec.pre i.label).vstack in
    Domain.add_equality_constraint state local v
  | LocalTee l ->
    let local = Spec.get_nth (Spec.pre i.label).locals l in
    (* same as local.set x followed by local.get x *)
    let v = Spec.pop (Spec.pre i.label).vstack in
    Domain.add_equality_constraints state [(Spec.ret i.label, v); (local, v)]
  | GlobalGet g ->
    (* add v = gn where gn is the local accessed *)
    Domain.add_equality_constraint state (Spec.ret i.label) (Spec.get_nth (Spec.pre i.label).globals g)
  | GlobalSet g ->
    let global = Spec.get_nth (Spec.pre i.label).globals g in
    let v = Spec.pop (Spec.pre i.label).vstack in
    Domain.add_equality_constraint state global v
  | Const n ->
    (* add ret = n *)
    Domain.add_constraint state (Spec_inference.var_to_string (Spec.ret i.label)) (Prim_value.to_string n)
  | Compare _ ->
    (* TODO: reflect "rel v1 v2" in the constraints, when possible *)
    (* add ret = [0;1] *)
    Domain.add_interval_constraint state (Spec.ret i.label) (0, 1)
  | Binary _ ->
    (* TODO: reflect "bin v1 v2" the operation in the constraints, when possible *)
    (* don't add any constraint (for now)  *)
    state
  | Test _ ->
    (* TODO: reflect "test v" in the constraints, when possible *)
    (* add ret = [0;1] *)
    Domain.add_interval_constraint state (Spec.ret i.label) (0, 1)
  | Convert _ ->
    (*    let _v, vstack' = Vstack.Spec.pop state.vstack in *)
    (* Don't add any constraint *)
    state
  | Load {offset; _ } ->
    let ret = Spec.ret i.label in
    let vaddr = Spec_inference.var_to_string (Spec.pop (Spec.pre i.label).vstack) in
    (* We assume load/stores are symmetric, i.e., when a load/store operation is made on an address for a specific type and size, all other operations on the same address are made with the same type and size *)
    (* TODO: we need to extract from the memory all values that can be pointed by the memory key. It could be that we are loading key mk1 from memory [mk0: mv0, mk1: mv1], and that from the constraints we have mk0 = mk1.
       Currently in that case, we return mv1 (which is correct), but we should add the constraint mv0 = mv1? Or rather, join (ret = mv0) (ret = mv1) *)
     Domain.add_constraints state [(Spec_inference.var_to_string (Spec_inference.MemoryKey (i.label, 0)), Printf.sprintf "%s+%d" vaddr offset);
                                   (Spec_inference.var_to_string ret, (Spec_inference.var_to_string (Spec_inference.MemoryVal (i.label, 0))))]
    (* If we want to model values as bytes, we will have to do the following
    let (addr0, addr1, addr2, addr3) = (Spec_inference.MemoryKey (i.label, 0),
                                        Spec_inference.MemoryKey (i.label, 1),
                                        Spec_inference.MemoryKey (i.label, 2),
                                        Spec_inference.MemoryKey (i.label, 3)) in
    (* add the constraints on addresses, i.e., addri = vaddr+offset+i where vaddr is the variable from the stack, addri is the address from which we will load *)
    let state' = Domain.add_constraints state [(Spec_inference.var_to_string addr3, (Printf.sprintf "%s+%d" vaddr (offset+3)));
                                               (Spec_inference.var_to_string addr2, (Printf.sprintf "%s+%d" vaddr (offset+2)));
                                               (Spec_inference.var_to_string addr1, (Printf.sprintf "%s+%d" vaddr (offset+1)));
                                               (Spec_inference.var_to_string addr0, (Printf.sprintf "%s+%d" vaddr offset))] in
  | Load ({ typ = I32; offset; sz = Some (Pack8, _) }) ->
    Logging.warn "ImpreciseOperation" "load8 returns top, and ignores sx/zx";
    let vaddr = Spec_inference.var_to_string (Spec.pop (Spec.pre i.label).vstack) in
    let addr0 = Spec_inference.MemoryKey (i.label, 0) in
    let state' = Domain.add_constraint state (Spec_inference.var_to_string addr0) (Printf.sprintf "%s+%d" vaddr offset) in
    (* TODO: add constraints on the return value? *)
    state'
  | Load op ->
    (* TODO: load with sz=8,zx (and others, but this is the most important now *)
       failwith (Printf.sprintf "load not supported with such op argument: %s" (Memoryop.to_string op)) *)
  | Store { offset; _ } ->
    let vval, vaddr = Spec.pop2 (Spec.pre i.label).vstack in
    Domain.add_constraints state [(Spec_inference.var_to_string (Spec_inference.MemoryKey (i.label, 0)), Printf.sprintf "%s+%d" (Spec_inference.var_to_string vaddr) offset);
                                  (Spec_inference.var_to_string (Spec_inference.MemoryValNew (i.label, 0)), Spec_inference.var_to_string vval)]
      (*
  | Store { typ = I32; offset; sz = None } ->
    let (addr0, addr1, addr2, addr3) = (Spec_inference.var_to_string (Spec_inference.MemoryKey (i.label, 0)),
                                        Spec_inference.var_to_string (Spec_inference.MemoryKey (i.label, 1)),
                                        Spec_inference.var_to_string (Spec_inference.MemoryKey (i.label, 2)),
                                        Spec_inference.var_to_string (Spec_inference.MemoryKey (i.label, 3))) in
    let (_vval, vaddr) = Spec.pop2 (Spec.pre i.label).vstack in
    let state' = Domain.add_constraints state [(addr3, (Printf.sprintf "%s+%d" (Spec_inference.var_to_string vaddr) (offset+3)));
                                               (addr2, (Printf.sprintf "%s+%d" (Spec_inference.var_to_string vaddr) (offset+2)));
                                               (addr1, (Printf.sprintf "%s+%d" (Spec_inference.var_to_string vaddr) (offset+1)));
                                               (addr0, (Printf.sprintf "%s+%d" (Spec_inference.var_to_string vaddr) offset))] in
    (* TODO: update memory *)
    state'
  | Store { typ = I64; offset; sz = None } ->
    let (addr0, addr1, addr2, addr3,
         addr4, addr5, addr6, addr7) = (Spec_inference.var_to_string (Spec_inference.MemoryKey (i.label, 0)),
                                        Spec_inference.var_to_string (Spec_inference.MemoryKey (i.label, 1)),
                                        Spec_inference.var_to_string (Spec_inference.MemoryKey (i.label, 2)),
                                        Spec_inference.var_to_string (Spec_inference.MemoryKey (i.label, 3)),
                                        Spec_inference.var_to_string (Spec_inference.MemoryKey (i.label, 4)),
                                        Spec_inference.var_to_string (Spec_inference.MemoryKey (i.label, 5)),
                                        Spec_inference.var_to_string (Spec_inference.MemoryKey (i.label, 6)),
                                        Spec_inference.var_to_string (Spec_inference.MemoryKey (i.label, 7))) in
    let (_vval, vaddr) = Spec.pop2 (Spec.pre i.label).vstack in
    let state' = Domain.add_constraints state
        [(addr7, (Printf.sprintf "%s+%d" (Spec_inference.var_to_string vaddr) (offset+7)));
         (addr6, (Printf.sprintf "%s+%d" (Spec_inference.var_to_string vaddr) (offset+6)));
         (addr5, (Printf.sprintf "%s+%d" (Spec_inference.var_to_string vaddr) (offset+5)));
         (addr4, (Printf.sprintf "%s+%d" (Spec_inference.var_to_string vaddr) (offset+4)));
         (addr3, (Printf.sprintf "%s+%d" (Spec_inference.var_to_string vaddr) (offset+3)));
         (addr2, (Printf.sprintf "%s+%d" (Spec_inference.var_to_string vaddr) (offset+2)));
         (addr1, (Printf.sprintf "%s+%d" (Spec_inference.var_to_string vaddr) (offset+1)));
         (addr0, (Printf.sprintf "%s+%d" (Spec_inference.var_to_string vaddr) offset))] in
    (* TODO: update memory *)
    state'
  | Store { typ = I32; offset; sz = Some (Pack8, SX)} ->
    Logging.warn "ImpreciseOperation" "store8 ignores sx/zx";
    let addr0 = Spec_inference.var_to_string (Spec_inference.MemoryKey (i.label, 0)) in
    let (_vval, vaddr) = Spec.pop2 (Spec.pre i.label).vstack in
    let state' = Domain.add_constraint state addr0 (Printf.sprintf "%s+%d" (Spec_inference.var_to_string vaddr) offset) in
    (* TODO: update memory *)
    state'
  | Store op ->
    (* TODO: store with i64? *)
    failwith (Printf.sprintf "store not supported with such op argument: %s" (Memoryop.to_string op)) *)

let control_instr_transfer
    (module_ : Wasm_module.t) (* The wasm module (read-only) *)
    (_cfg : Cfg.t) (* The CFG analyzed *)
    (i : Instr.control Instr.labelled) (* The instruction *)
    (state : Domain.state) (* The pre state *)
  : result =
  let apply_summary (f : int) (arity : int * int) (state : state) : state =
    let summary = IntMap.find_exn !summaries f in
    let args = List.take (Spec.pre i.label).vstack (fst arity) in
    let ret = if snd arity = 1 then List.hd (Spec.post i.label).vstack else None in
    Summary.apply summary state (List.map ~f:Spec_inference.var_to_string args) (Option.map ~f:Spec_inference.var_to_string ret)
  in
  match i.instr with
  | Call (arity, f) ->
    (* We encounter a function call, retrieve its summary and apply it *)
    (* We assume all summaries are defined *)
    Simple (apply_summary f arity state)
  | CallIndirect (arity, typ) ->
    (* v is the index in the table that points to the called functiion *)
    let v = Spec_inference.var_to_string (Spec.pop (Spec.pre i.label).vstack) in
    (* Get table 0 *)
    let table = List.nth_exn module_.tables 0 in
    (* Get all indices that v could be equal to *)
    let funids = List.filter (Table_inst.indices table) ~f:(fun idx ->
        fst (Domain.is_equal state v idx)) in
    let funs = List.map funids ~f:(fun idx -> Table_inst.get table idx) in
    (* The type of the function that should be applied *)
    let ftype = Wasm_module.get_type module_ typ in
    assert (snd arity <= 1);
    (* Apply the summaries *)
    let resulting_state = List.fold_left funs ~init:None ~f:(fun acc f ->
        match f with
        | Some fa ->
          if Stdlib.(ftype = (Wasm_module.get_func_type module_ fa)) then begin
            Logging.info (Printf.sprintf "call_indirect applies function %d (type: %s)" fa (Type.funtype_to_string ftype));
            (* Types match, apply the summary *)
            Some (Domain.join_opt (apply_summary fa arity state) acc)
          end else
            (* Types don't match, can't apply this function so we ignore it *)
            acc
        | None -> acc) in
    begin match resulting_state with
      | Some st -> Simple st
      | None ->
        (* The call can't be resolved, no constraints to add *)
        Simple state
    end
  | Br _ ->
    Simple state
  | BrIf _
  | If _ ->
    let cond = Spec_inference.var_to_string (Spec.pop (Spec.pre i.label).vstack) in
    (* restrict cond = 1 to the constraints of the true branch, cond = 0 to the constrainst of the false branch *)
      Branch (Domain.meet_interval state cond (1, 1) (* true branch *),
              Domain.meet_interval state cond (0, 0) (* false branch *))
  | Return ->
    (* return does not change anything *)
    Simple state
  | Unreachable ->
    (* Unreachable, so what we return does not really matter *)
    Simple state
  | _ -> failwith (Printf.sprintf "Unsupported control instruction: %s" (Instr.control_to_string i.instr))
end
