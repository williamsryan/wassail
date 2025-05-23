open Core
open Wassail

let page_size = 65536
let label_gen = Instr.Label.maker Instr.Label.Dummy

let load_static_memory_entries (facts_file : string) : (int * int) list =
  let lines = In_channel.read_lines facts_file in
  List.filter_map lines ~f:(fun line ->
      match String.split line ~on:'\t' with
      | [ op; _func; _bb; _instr; offset; _pos ] when String.equal op "load" ->
          (* If you have access to actual value later, use it here instead of 0 *)
          Some (Int.of_string offset, 0)
      | _ -> None)

let round_to_next_page (addr : int) : int =
  (addr + page_size - 1) / page_size * page_size

(* Helper to create a constant I32 instruction *)
let make_const_i32 value =
  Instr.Data
    {
      instr = Instr.Const (Prim_value.I32 value);
      label = label_gen ();
      line_number = -1;
      annotation_before = ();
      annotation_after = ();
    }

let highest_data_segment_index (modul : Wasm_module.t) : int32 =
  if List.is_empty modul.datas then Int32.zero
  else
    List.fold modul.datas ~init:Int32.zero ~f:(fun max_idx data_segment ->
        Int32.max max_idx data_segment.idx)

(** Find the highest offset used by any existing data segment *)
let highest_existing_data_offset (modul : Wasm_module.t) : int =
  if List.is_empty modul.datas then 0
  else
    (* Simply sum up all data segment sizes to get a conservative estimate *)
    let total_data_size =
      List.fold modul.datas ~init:0 ~f:(fun acc data_segment ->
          acc + String.length data_segment.dinit)
    in
    (* Round up to next page boundary to be safe *)
    round_to_next_page total_data_size

let build_static_data_segment (static_data : (int * int) list) ~base_offset =
  let sorted =
    List.sort static_data ~compare:(fun (a, _) (b, _) -> Int.compare a b)
  in
  let bytes = Bytes.create (List.length sorted * 4) in
  let addr_map = ref Int.Map.empty in

  List.iteri sorted ~f:(fun i (offset, value) ->
      let addr = base_offset + (i * 4) in

      (* Write int32 in little-endian format *)
      EndianBytes.LittleEndian.set_int32 bytes (i * 4) (Int32.of_int_exn value);

      addr_map := Map.set !addr_map ~key:offset ~data:addr);

  (* Create the data segment *)
  let data_segment : Data_segment.t =
    {
      idx = Int32.of_int_exn (List.length sorted);
      dinit = Bytes.to_string bytes;
      dmode =
        Segment_mode.Active
          {
            table_index = Int32.zero;
            offset = [ make_const_i32 (Int32.of_int_exn base_offset) ];
          };
    }
  in

  (data_segment, !addr_map)

let rewrite_instr ~label_gen instr addr_map =
  match instr with
  | Instr.Data
      {
        instr = Instr.Load memop;
        label;
        line_number;
        annotation_before;
        annotation_after;
      }
    when Map.mem addr_map memop.offset ->
      let new_offset = Map.find_exn addr_map memop.offset in
      let new_memop = { memop with offset = new_offset } in
      Instr.Data
        {
          instr = Instr.Load new_memop;
          label;
          line_number;
          annotation_before;
          annotation_after;
        }
  | Instr.Data { instr = Instr.Store memop; _ }
    when Map.mem addr_map memop.offset ->
      Instr.Data
        {
          instr = Instr.Nop;
          label = label_gen ();
          line_number = -1;
          annotation_before = ();
          annotation_after = ();
        }
  | _ -> instr

let rewrite_func (func : Func_inst.t) ~addr_map ~label_gen =
  let new_body =
    List.map func.code.body ~f:(fun instr ->
        rewrite_instr ~label_gen instr addr_map)
  in
  { func with code = { func.code with body = new_body } }

let rewrite (input_wasm : string) (accesses : (int * int) list)
    (output_path : string) : unit =
  let modul = Wasm_module.of_file input_wasm in
  let last_offset = highest_existing_data_offset modul in
  let new_base = round_to_next_page last_offset in
  let new_segment, addr_map =
    build_static_data_segment accesses ~base_offset:new_base
  in

  (* let label_gen = Instr.Label.maker Instr.Label.Dummy in *)
  let rewritten_funcs =
    List.map modul.funcs ~f:(fun f -> rewrite_func f ~addr_map ~label_gen)
  in

  let final_module =
    { modul with funcs = rewritten_funcs; datas = new_segment :: modul.datas }
  in

  let oc = Out_channel.create output_path in
  Out_channel.output_string oc (Wasm_module.to_string final_module);
  Out_channel.close oc;
  Printf.printf "✅ Wrote rewritten module to: %s\n%!" output_path
