(module
  (type (;0;) (func))
  (type (;1;) (func (result i32)))
  (type (;2;) (func (param i32)))
  (import "env" "print" (func (;0;) (type 2)))
  (func (;1;) (type 0)
    i32.const 66608
    global.set 2
    i32.const 1072
    global.set 1)
  (func (;2;) (type 1) (result i32)
    i32.const 1060)
  (func (;3;) (type 1) (result i32)
    i32.const 1024)
  (func (;4;) (type 0)
    i32.const 1
    call 0)
  (func (;5;) (type 0)
    i32.const 2
    call 0)
  (func (;6;) (type 0)
    i32.const 1040
    i32.load
    call_indirect (type 0))
  (func (;7;) (type 2) (param i32)
    (local i32 i32)
    i32.const 1024
    local.set 2
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.const 1024
        i32.xor
        i32.const 3
        i32.and
        if  ;; label = @3
          local.get 0
          i32.load8_u
          local.set 1
          br 1 (;@2;)
        end
        local.get 0
        i32.const 3
        i32.and
        if  ;; label = @3
          loop  ;; label = @4
            local.get 2
            local.get 0
            i32.load8_u
            local.tee 1
            i32.store8
            local.get 1
            i32.eqz
            br_if 3 (;@1;)
            local.get 2
            i32.const 1
            i32.add
            local.set 2
            local.get 0
            i32.const 1
            i32.add
            local.tee 0
            i32.const 3
            i32.and
            br_if 0 (;@4;)
          end
        end
        i32.const 16843008
        local.get 0
        i32.load
        local.tee 1
        i32.sub
        local.get 1
        i32.or
        i32.const -2139062144
        i32.and
        i32.const -2139062144
        i32.ne
        br_if 0 (;@2;)
        loop  ;; label = @3
          local.get 2
          local.get 1
          i32.store
          local.get 2
          i32.const 4
          i32.add
          local.set 2
          local.get 0
          i32.load offset=4
          local.set 1
          local.get 0
          i32.const 4
          i32.add
          local.set 0
          local.get 1
          i32.const 16843008
          local.get 1
          i32.sub
          i32.or
          i32.const -2139062144
          i32.and
          i32.const -2139062144
          i32.eq
          br_if 0 (;@3;)
        end
      end
      local.get 2
      local.get 1
      i32.store8
      local.get 1
      i32.const 255
      i32.and
      i32.eqz
      br_if 0 (;@1;)
      loop  ;; label = @2
        local.get 2
        local.get 0
        i32.load8_u offset=1
        local.tee 1
        i32.store8 offset=1
        local.get 2
        i32.const 1
        i32.add
        local.set 2
        local.get 0
        i32.const 1
        i32.add
        local.set 0
        local.get 1
        br_if 0 (;@2;)
      end
    end)
  (func (;8;) (type 0)
    call 1)
  (func (;9;) (type 2) (param i32)
    local.get 0
    global.set 0)
  (func (;10;) (type 1) (result i32)
    global.get 0)
  (func (;11;) (type 1) (result i32)
    global.get 2)
  (func (;12;) (type 1) (result i32)
    global.get 1)
  (table (;0;) 4 4 funcref)
  (memory (;0;) 258 258)
  (global (;0;) (mut i32) (i32.const 66608))
  (global (;1;) (mut i32) (i32.const 0))
  (global (;2;) (mut i32) (i32.const 0))
  (export "memory" (memory 0))
  (export "get_data_end" (func 2))
  (export "get_global_base" (func 3))
  (export "foo" (func 4))
  (export "bar" (func 5))
  (export "baz" (func 6))
  (export "__indirect_function_table" (table 0))
  (export "setX" (func 7))
  (export "_initialize" (func 8))
  (export "emscripten_stack_get_base" (func 11))
  (export "emscripten_stack_get_end" (func 12))
  (export "_emscripten_stack_restore" (func 9))
  (export "emscripten_stack_get_current" (func 10))
  (elem (;0;) (i32.const 1) func 4 5 1)
  (data (;0;) (i32.const 1024) "hello")
  (data (;1;) (i32.const 1040) "\01\00\00\00\02"))
