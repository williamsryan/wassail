open Core

let get_static_memory_accesses (wasm_file : string) : (int * int) list =
  let conn = new Postgresql.connection ~conninfo:"dbname=weisswurst" () in
  let sha256 = Md5.to_hex (Md5.digest_string wasm_file) in
  let res =
    conn#exec ~expect:[ Postgresql.Tuples_ok ] ~params:[| sha256 |]
      "SELECT address, value FROM memory_access_log WHERE wasm_id = $1 AND \
       operation = 'load'"
  in
  let rows = res#get_all in
  Array.to_list
    (Array.map
       ~f:(fun row -> (int_of_string row.(0), int_of_string row.(1)))
       rows)
