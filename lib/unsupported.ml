let vector_type (_ : unit) : 'a =
  failwith "unsupported: vector type from WebAssembly 2.0"

let reference_type (_ : unit) : 'a =
  failwith "unsupported: reference type from WebAssembly 2.0"

let reference_instructions (_ : unit) : 'a =
  failwith "unsupported: reference type instructions from WebAssembly 2.0"

let memory_2_instructions (_ : unit) : 'a =
  failwith "unsupported: memory instructions from WebAssembly 2.0"

let table_instructions (_ : unit) : 'a =
  failwith "unsupported: table instructions from WebAssembly 2.0"

let drop_2_instructions (_ : unit) : 'a =
  failwith "unsupported: drop instructions from WebAssembly 2.0"

let tables_2 (_ : unit) : 'a =
  failwith "unsupported: multiple tables from WebAssembly 2.0"

let table_init (_ : unit) : 'a =
  failwith "unsupported: table initialization method"
