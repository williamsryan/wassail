(module
  (type (;0;) (func (param i32 i32 i32) (result i32)))
  (type (;1;) (func (param i32 i64 i32) (result i64)))
  (type (;2;) (func (param i32) (result i32)))
  (type (;3;) (func (result i32)))
  (type (;4;) (func (param i32)))
  (type (;5;) (func))
  (type (;6;) (func (param i32 i32)))
  (type (;7;) (func (param i32 i32 i32)))
  (type (;8;) (func (param i32 i32) (result i32)))
  (type (;9;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;10;) (func (param i32 i64 i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "proc_exit" (func (;0;) (type 4)))
  (import "wasi_snapshot_preview1" "fd_close" (func (;1;) (type 2)))
  (import "wasi_snapshot_preview1" "fd_seek" (func (;2;) (type 10)))
  (import "wasi_snapshot_preview1" "fd_write" (func (;3;) (type 9)))
  (import "wasi_snapshot_preview1" "fd_fdstat_get" (func (;4;) (type 8)))
  (func (;5;) (type 5)
    (local i32)
    call 7
    local.set 0
    call 21
    local.get 0
    if  ;; label = @1
      local.get 0
      call 0
      unreachable
    end)
  (func (;6;) (type 3) (result i32)
    (local i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 0
    global.set 0
    local.get 0
    i32.const 0
    i32.store offset=12
    block  ;; label = @1
      block (result i32)  ;; label = @2
        i32.const 48
        call 11
        local.tee 1
        i32.eqz
        br_if 0 (;@2;)
        drop
        local.get 0
        i32.const 12
        i32.add
        local.get 1
        i32.store
        i32.const 0
      end
      i32.eqz
      if  ;; label = @2
        local.get 0
        i32.load offset=12
        local.tee 1
        br_if 1 (;@1;)
      end
      i32.const 1076
      i32.load
      call 13
      call 21
      i32.const 1
      call 0
      unreachable
    end
    local.get 0
    i32.const 16
    i32.add
    global.set 0
    local.get 1)
  (func (;7;) (type 3) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
    call 6
    local.tee 8
    local.set 2
    loop  ;; label = @1
      i32.const 0
      local.set 3
      i32.const 0
      local.set 4
      i32.const 1
      local.set 7
      local.get 1
      local.set 0
      local.get 6
      local.set 5
      loop  ;; label = @2
        local.get 2
        local.get 3
        i32.add
        local.get 7
        local.get 4
        i32.const 7
        i32.div_u
        i32.const 7
        i32.mul
        i32.sub
        i32.const 999
        local.get 5
        local.get 0
        i32.const 7
        i32.div_u
        i32.const 7
        i32.mul
        i32.add
        select
        i32.const 999
        local.get 5
        local.get 0
        i32.const 13
        i32.div_u
        i32.const 13
        i32.mul
        i32.add
        select
        i32.const 999
        local.get 5
        local.get 0
        i32.const 11
        i32.div_u
        i32.const 11
        i32.mul
        i32.add
        select
        i32.store
        local.get 1
        local.get 4
        i32.add
        local.set 4
        local.get 1
        local.get 7
        i32.add
        local.set 7
        local.get 0
        i32.const 1
        i32.add
        local.set 0
        local.get 5
        i32.const -1
        i32.add
        local.set 5
        local.get 3
        i32.const 4
        i32.add
        local.tee 3
        i32.const 2000
        i32.ne
        br_if 0 (;@2;)
      end
      local.get 2
      i32.const 2000
      i32.add
      local.set 2
      local.get 6
      i32.const -1
      i32.add
      local.set 6
      local.get 1
      i32.const 1
      i32.add
      local.tee 1
      i32.const 500
      i32.ne
      br_if 0 (;@1;)
    end
    i32.const 0
    local.set 6
    local.get 8
    local.set 1
    loop  ;; label = @1
      local.get 8
      local.set 4
      i32.const 0
      local.set 2
      loop  ;; label = @2
        local.get 8
        local.get 2
        i32.const 2000
        i32.mul
        i32.add
        local.get 6
        i32.const 2
        i32.shl
        i32.add
        local.set 5
        i32.const 0
        local.set 0
        loop  ;; label = @3
          local.get 0
          local.get 4
          i32.add
          local.tee 3
          local.get 3
          i32.load
          local.tee 3
          local.get 0
          local.get 1
          i32.add
          i32.load
          local.get 5
          i32.load
          i32.add
          local.tee 7
          local.get 3
          local.get 7
          i32.lt_s
          select
          i32.store
          local.get 0
          i32.const 4
          i32.add
          local.tee 0
          i32.const 2000
          i32.ne
          br_if 0 (;@3;)
        end
        local.get 4
        i32.const 2000
        i32.add
        local.set 4
        local.get 2
        i32.const 1
        i32.add
        local.tee 2
        i32.const 500
        i32.ne
        br_if 0 (;@2;)
      end
      local.get 1
      i32.const 2000
      i32.add
      local.set 1
      local.get 6
      i32.const 1
      i32.add
      local.tee 6
      i32.const 500
      i32.ne
      br_if 0 (;@1;)
    end
    local.get 8
    call 9
    i32.const 0)
  (func (;8;) (type 3) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 9
    global.set 0
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      i32.const 1332
                      i32.load
                      local.tee 6
                      i32.eqz
                      br_if 0 (;@9;)
                      i32.const -1004128
                      local.set 3
                      block  ;; label = @10
                        block  ;; label = @11
                          block  ;; label = @12
                            i32.const 1724
                            i32.load
                            local.tee 2
                            i32.eqz
                            if  ;; label = @13
                              br 1 (;@12;)
                            end
                            i32.const -728236032
                            local.set 0
                            loop  ;; label = @13
                              block  ;; label = @14
                                local.get 2
                                i32.load offset=4
                                i32.const -8
                                i32.and
                                i32.const 1004128
                                i32.sub
                                local.tee 4
                                local.get 3
                                i32.ge_u
                                br_if 0 (;@14;)
                                local.get 2
                                local.set 5
                                local.get 4
                                local.tee 3
                                br_if 0 (;@14;)
                                i32.const 0
                                local.set 3
                                local.get 2
                                local.set 1
                                br 3 (;@11;)
                              end
                              local.get 1
                              local.get 2
                              i32.const 20
                              i32.add
                              i32.load
                              local.tee 4
                              local.get 4
                              local.get 2
                              local.get 0
                              i32.const 29
                              i32.shr_u
                              i32.const 4
                              i32.and
                              i32.add
                              i32.const 16
                              i32.add
                              i32.load
                              local.tee 2
                              i32.eq
                              select
                              local.get 1
                              local.get 4
                              select
                              local.set 1
                              local.get 0
                              local.get 2
                              i32.const 0
                              i32.ne
                              i32.shl
                              local.set 0
                              local.get 2
                              br_if 0 (;@13;)
                            end
                          end
                          local.get 1
                          local.get 5
                          i32.or
                          i32.eqz
                          if  ;; label = @12
                            local.get 6
                            i32.const -16777216
                            i32.and
                            local.tee 0
                            i32.eqz
                            br_if 3 (;@9;)
                            local.get 0
                            i32.const 0
                            local.get 0
                            i32.sub
                            i32.and
                            i32.const -1
                            i32.add
                            local.tee 0
                            local.get 0
                            i32.const 12
                            i32.shr_u
                            i32.const 16
                            i32.and
                            local.tee 0
                            i32.shr_u
                            local.tee 1
                            i32.const 5
                            i32.shr_u
                            i32.const 8
                            i32.and
                            local.tee 2
                            local.get 0
                            i32.or
                            local.get 1
                            local.get 2
                            i32.shr_u
                            local.tee 0
                            i32.const 2
                            i32.shr_u
                            i32.const 4
                            i32.and
                            local.tee 1
                            i32.or
                            local.get 0
                            local.get 1
                            i32.shr_u
                            local.tee 0
                            i32.const 1
                            i32.shr_u
                            i32.const 2
                            i32.and
                            local.tee 1
                            i32.or
                            local.get 0
                            local.get 1
                            i32.shr_u
                            local.tee 0
                            i32.const 1
                            i32.shr_u
                            i32.const 1
                            i32.and
                            local.tee 1
                            i32.or
                            local.get 0
                            local.get 1
                            i32.shr_u
                            i32.add
                            i32.const 2
                            i32.shl
                            i32.const 1632
                            i32.add
                            i32.load
                            local.set 1
                          end
                          local.get 1
                          i32.eqz
                          br_if 1 (;@10;)
                        end
                        loop  ;; label = @11
                          local.get 1
                          i32.load offset=4
                          i32.const -8
                          i32.and
                          i32.const 1004128
                          i32.sub
                          local.tee 2
                          local.get 3
                          i32.lt_u
                          local.set 0
                          local.get 2
                          local.get 3
                          local.get 0
                          select
                          local.set 3
                          local.get 1
                          local.get 5
                          local.get 0
                          select
                          local.set 5
                          local.get 1
                          i32.load offset=16
                          local.tee 0
                          if (result i32)  ;; label = @12
                            local.get 0
                          else
                            local.get 1
                            i32.const 20
                            i32.add
                            i32.load
                          end
                          local.tee 1
                          br_if 0 (;@11;)
                        end
                      end
                      local.get 5
                      i32.eqz
                      br_if 0 (;@9;)
                      local.get 3
                      i32.const 1336
                      i32.load
                      i32.const 1004128
                      i32.sub
                      i32.ge_u
                      br_if 0 (;@9;)
                      local.get 5
                      i32.load offset=24
                      local.set 7
                      local.get 5
                      local.get 5
                      i32.load offset=12
                      local.tee 0
                      i32.ne
                      if  ;; label = @10
                        i32.const 1344
                        i32.load
                        local.get 5
                        i32.load offset=8
                        local.tee 1
                        i32.le_u
                        if  ;; label = @11
                          local.get 1
                          i32.load offset=12
                          drop
                        end
                        local.get 0
                        local.get 1
                        i32.store offset=8
                        local.get 1
                        local.get 0
                        i32.store offset=12
                        br 8 (;@2;)
                      end
                      local.get 5
                      i32.const 20
                      i32.add
                      local.tee 2
                      i32.load
                      local.tee 1
                      i32.eqz
                      if  ;; label = @10
                        local.get 5
                        i32.load offset=16
                        local.tee 1
                        i32.eqz
                        br_if 2 (;@8;)
                        local.get 5
                        i32.const 16
                        i32.add
                        local.set 2
                      end
                      loop  ;; label = @10
                        local.get 2
                        local.set 4
                        local.get 1
                        local.tee 0
                        i32.const 20
                        i32.add
                        local.tee 2
                        i32.load
                        local.tee 1
                        br_if 0 (;@10;)
                        local.get 0
                        i32.const 16
                        i32.add
                        local.set 2
                        local.get 0
                        i32.load offset=16
                        local.tee 1
                        br_if 0 (;@10;)
                      end
                      local.get 4
                      i32.const 0
                      i32.store
                      br 7 (;@2;)
                    end
                    i32.const 1336
                    i32.load
                    local.tee 1
                    i32.const 1004128
                    i32.ge_u
                    if  ;; label = @9
                      i32.const 1348
                      i32.load
                      local.set 0
                      block  ;; label = @10
                        local.get 1
                        i32.const 1004128
                        i32.sub
                        local.tee 2
                        i32.const 16
                        i32.ge_u
                        if  ;; label = @11
                          local.get 0
                          i32.const 1004128
                          i32.add
                          local.tee 3
                          local.get 2
                          i32.const 1
                          i32.or
                          i32.store offset=4
                          i32.const 1336
                          local.get 2
                          i32.store
                          i32.const 1348
                          local.get 3
                          i32.store
                          local.get 0
                          local.get 1
                          i32.add
                          local.get 2
                          i32.store
                          local.get 0
                          i32.const 1004131
                          i32.store offset=4
                          br 1 (;@10;)
                        end
                        local.get 0
                        local.get 1
                        i32.const 3
                        i32.or
                        i32.store offset=4
                        local.get 0
                        local.get 1
                        i32.add
                        local.tee 1
                        local.get 1
                        i32.load offset=4
                        i32.const 1
                        i32.or
                        i32.store offset=4
                        i32.const 1348
                        i32.const 0
                        i32.store
                        i32.const 1336
                        i32.const 0
                        i32.store
                      end
                      local.get 0
                      i32.const 8
                      i32.add
                      local.set 1
                      br 8 (;@1;)
                    end
                    i32.const 1340
                    i32.load
                    local.tee 0
                    i32.const 1004128
                    i32.gt_u
                    if  ;; label = @9
                      i32.const 1352
                      i32.load
                      local.tee 1
                      i32.const 1004128
                      i32.add
                      local.tee 2
                      local.get 0
                      i32.const 1004128
                      i32.sub
                      local.tee 0
                      i32.const 1
                      i32.or
                      i32.store offset=4
                      i32.const 1340
                      local.get 0
                      i32.store
                      i32.const 1352
                      local.get 2
                      i32.store
                      local.get 1
                      i32.const 1004131
                      i32.store offset=4
                      local.get 1
                      i32.const 8
                      i32.add
                      local.set 1
                      br 8 (;@1;)
                    end
                    i32.const 0
                    local.set 1
                    block (result i32)  ;; label = @9
                      i32.const 1800
                      i32.load
                      if  ;; label = @10
                        i32.const 1808
                        i32.load
                        br 1 (;@9;)
                      end
                      i32.const 1812
                      i64.const -1
                      i64.store align=4
                      i32.const 1804
                      i64.const 281474976776192
                      i64.store align=4
                      i32.const 1800
                      local.get 9
                      i32.const 12
                      i32.add
                      i32.const -16
                      i32.and
                      i32.const 1431655768
                      i32.xor
                      i32.store
                      i32.const 1820
                      i32.const 0
                      i32.store
                      i32.const 1772
                      i32.const 0
                      i32.store
                      i32.const 65536
                    end
                    local.tee 2
                    i32.const 1004199
                    i32.add
                    local.tee 5
                    i32.const 0
                    local.get 2
                    i32.sub
                    local.tee 4
                    i32.and
                    local.tee 2
                    i32.const 1004128
                    i32.le_u
                    if  ;; label = @9
                      i32.const 1824
                      i32.const 48
                      i32.store
                      br 8 (;@1;)
                    end
                    block  ;; label = @9
                      i32.const 1768
                      i32.load
                      local.tee 1
                      i32.eqz
                      br_if 0 (;@9;)
                      i32.const 1760
                      i32.load
                      local.tee 3
                      local.get 2
                      i32.add
                      local.tee 6
                      local.get 3
                      i32.gt_u
                      i32.const 0
                      local.get 6
                      local.get 1
                      i32.le_u
                      select
                      br_if 0 (;@9;)
                      i32.const 0
                      local.set 1
                      i32.const 1824
                      i32.const 48
                      i32.store
                      br 8 (;@1;)
                    end
                    i32.const 1772
                    i32.load8_u
                    i32.const 4
                    i32.and
                    br_if 3 (;@5;)
                    block  ;; label = @9
                      block  ;; label = @10
                        i32.const 1352
                        i32.load
                        local.tee 3
                        if  ;; label = @11
                          i32.const 1776
                          local.set 1
                          loop  ;; label = @12
                            local.get 1
                            i32.load
                            local.tee 6
                            local.get 3
                            i32.le_u
                            if  ;; label = @13
                              local.get 6
                              local.get 1
                              i32.load offset=4
                              i32.add
                              local.get 3
                              i32.gt_u
                              br_if 3 (;@10;)
                            end
                            local.get 1
                            i32.load offset=8
                            local.tee 1
                            br_if 0 (;@12;)
                          end
                        end
                        i32.const 0
                        call 12
                        local.tee 0
                        i32.const -1
                        i32.eq
                        br_if 4 (;@6;)
                        local.get 2
                        local.set 3
                        i32.const 1804
                        i32.load
                        local.tee 1
                        i32.const -1
                        i32.add
                        local.tee 5
                        local.get 0
                        i32.and
                        if  ;; label = @11
                          local.get 2
                          local.get 0
                          i32.sub
                          local.get 0
                          local.get 5
                          i32.add
                          i32.const 0
                          local.get 1
                          i32.sub
                          i32.and
                          i32.add
                          local.set 3
                        end
                        local.get 3
                        i32.const 1004128
                        i32.le_u
                        local.get 3
                        i32.const 2147483646
                        i32.gt_u
                        i32.or
                        br_if 4 (;@6;)
                        i32.const 1768
                        i32.load
                        local.tee 1
                        if  ;; label = @11
                          i32.const 1760
                          i32.load
                          local.tee 5
                          local.get 3
                          i32.add
                          local.tee 4
                          local.get 5
                          i32.le_u
                          local.get 4
                          local.get 1
                          i32.gt_u
                          i32.or
                          br_if 5 (;@6;)
                        end
                        local.get 3
                        call 12
                        local.tee 1
                        local.get 0
                        i32.ne
                        br_if 1 (;@9;)
                        br 6 (;@4;)
                      end
                      local.get 5
                      local.get 0
                      i32.sub
                      local.get 4
                      i32.and
                      local.tee 3
                      i32.const 2147483646
                      i32.gt_u
                      br_if 3 (;@6;)
                      local.get 3
                      call 12
                      local.tee 0
                      local.get 1
                      i32.load
                      local.get 1
                      i32.load offset=4
                      i32.add
                      i32.eq
                      br_if 2 (;@7;)
                      local.get 0
                      local.set 1
                    end
                    i32.const 1004200
                    local.get 3
                    i32.le_u
                    local.get 3
                    i32.const 2147483646
                    i32.gt_u
                    i32.or
                    local.get 1
                    local.tee 0
                    i32.const -1
                    i32.eq
                    i32.or
                    i32.eqz
                    if  ;; label = @9
                      i32.const 1808
                      i32.load
                      local.tee 1
                      i32.const 1004199
                      local.get 3
                      i32.sub
                      i32.add
                      i32.const 0
                      local.get 1
                      i32.sub
                      i32.and
                      local.tee 1
                      i32.const 2147483646
                      i32.gt_u
                      br_if 5 (;@4;)
                      local.get 1
                      call 12
                      i32.const -1
                      i32.ne
                      if  ;; label = @10
                        local.get 1
                        local.get 3
                        i32.add
                        local.set 3
                        br 6 (;@4;)
                      end
                      i32.const 0
                      local.get 3
                      i32.sub
                      call 12
                      drop
                      br 3 (;@6;)
                    end
                    local.get 0
                    i32.const -1
                    i32.ne
                    br_if 4 (;@4;)
                    br 2 (;@6;)
                  end
                  i32.const 0
                  local.set 0
                  br 5 (;@2;)
                end
                local.get 0
                i32.const -1
                i32.ne
                br_if 2 (;@4;)
              end
              i32.const 1772
              i32.const 1772
              i32.load
              i32.const 4
              i32.or
              i32.store
            end
            local.get 2
            i32.const 2147483646
            i32.gt_u
            br_if 1 (;@3;)
            local.get 2
            call 12
            local.tee 0
            i32.const 0
            call 12
            local.tee 1
            i32.ge_u
            local.get 0
            i32.const -1
            i32.eq
            i32.or
            local.get 1
            i32.const -1
            i32.eq
            i32.or
            br_if 1 (;@3;)
            local.get 1
            local.get 0
            i32.sub
            local.tee 3
            i32.const 1004184
            i32.le_u
            br_if 1 (;@3;)
          end
          i32.const 1760
          i32.const 1760
          i32.load
          local.get 3
          i32.add
          local.tee 1
          i32.store
          local.get 1
          i32.const 1764
          i32.load
          i32.gt_u
          if  ;; label = @4
            i32.const 1764
            local.get 1
            i32.store
          end
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                i32.const 1352
                i32.load
                local.tee 4
                if  ;; label = @7
                  i32.const 1776
                  local.set 1
                  loop  ;; label = @8
                    local.get 0
                    local.get 1
                    i32.load
                    local.tee 2
                    local.get 1
                    i32.load offset=4
                    local.tee 5
                    i32.add
                    i32.eq
                    br_if 2 (;@6;)
                    local.get 1
                    i32.load offset=8
                    local.tee 1
                    br_if 0 (;@8;)
                  end
                  br 2 (;@5;)
                end
                i32.const 1344
                i32.load
                local.tee 1
                i32.const 0
                local.get 0
                local.get 1
                i32.ge_u
                select
                i32.eqz
                if  ;; label = @7
                  i32.const 1344
                  local.get 0
                  i32.store
                end
                i32.const 0
                local.set 1
                i32.const 1780
                local.get 3
                i32.store
                i32.const 1776
                local.get 0
                i32.store
                i32.const 1360
                i32.const -1
                i32.store
                i32.const 1364
                i32.const 1800
                i32.load
                i32.store
                i32.const 1788
                i32.const 0
                i32.store
                loop  ;; label = @7
                  local.get 1
                  i32.const 1376
                  i32.add
                  local.get 1
                  i32.const 1368
                  i32.add
                  local.tee 2
                  i32.store
                  local.get 1
                  i32.const 1380
                  i32.add
                  local.get 2
                  i32.store
                  local.get 1
                  i32.const 8
                  i32.add
                  local.tee 1
                  i32.const 256
                  i32.ne
                  br_if 0 (;@7;)
                end
                local.get 0
                i32.const -8
                local.get 0
                i32.sub
                i32.const 15
                i32.and
                i32.const 0
                local.get 0
                i32.const 8
                i32.add
                i32.const 15
                i32.and
                select
                local.tee 1
                i32.add
                local.tee 2
                local.get 3
                i32.const -56
                i32.add
                local.tee 3
                local.get 1
                i32.sub
                local.tee 1
                i32.const 1
                i32.or
                i32.store offset=4
                i32.const 1356
                i32.const 1816
                i32.load
                i32.store
                i32.const 1340
                local.get 1
                i32.store
                i32.const 1352
                local.get 2
                i32.store
                local.get 0
                local.get 3
                i32.add
                i32.const 56
                i32.store offset=4
                br 2 (;@4;)
              end
              local.get 1
              i32.load8_u offset=12
              i32.const 8
              i32.and
              local.get 0
              local.get 4
              i32.le_u
              i32.or
              local.get 2
              local.get 4
              i32.gt_u
              i32.or
              br_if 0 (;@5;)
              local.get 4
              i32.const -8
              local.get 4
              i32.sub
              i32.const 15
              i32.and
              i32.const 0
              local.get 4
              i32.const 8
              i32.add
              i32.const 15
              i32.and
              select
              local.tee 0
              i32.add
              local.tee 2
              i32.const 1340
              i32.load
              local.get 3
              i32.add
              local.tee 6
              local.get 0
              i32.sub
              local.tee 0
              i32.const 1
              i32.or
              i32.store offset=4
              local.get 1
              local.get 3
              local.get 5
              i32.add
              i32.store offset=4
              i32.const 1356
              i32.const 1816
              i32.load
              i32.store
              i32.const 1340
              local.get 0
              i32.store
              i32.const 1352
              local.get 2
              i32.store
              local.get 4
              local.get 6
              i32.add
              i32.const 56
              i32.store offset=4
              br 1 (;@4;)
            end
            local.get 0
            i32.const 1344
            i32.load
            local.tee 5
            i32.lt_u
            if  ;; label = @5
              i32.const 1344
              local.get 0
              i32.store
              local.get 0
              local.set 5
            end
            local.get 0
            local.get 3
            i32.add
            local.set 2
            i32.const 1776
            local.set 1
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        loop  ;; label = @11
                          local.get 2
                          local.get 1
                          i32.load
                          i32.ne
                          if  ;; label = @12
                            local.get 1
                            i32.load offset=8
                            local.tee 1
                            br_if 1 (;@11;)
                            br 2 (;@10;)
                          end
                        end
                        local.get 1
                        i32.load8_u offset=12
                        i32.const 8
                        i32.and
                        i32.eqz
                        br_if 1 (;@9;)
                      end
                      i32.const 1776
                      local.set 1
                      loop  ;; label = @10
                        local.get 1
                        i32.load
                        local.tee 2
                        local.get 4
                        i32.le_u
                        if  ;; label = @11
                          local.get 2
                          local.get 1
                          i32.load offset=4
                          i32.add
                          local.tee 5
                          local.get 4
                          i32.gt_u
                          br_if 3 (;@8;)
                        end
                        local.get 1
                        i32.load offset=8
                        local.set 1
                        br 0 (;@10;)
                      end
                      unreachable
                    end
                    local.get 1
                    local.get 0
                    i32.store
                    local.get 1
                    local.get 1
                    i32.load offset=4
                    local.get 3
                    i32.add
                    i32.store offset=4
                    local.get 0
                    i32.const -8
                    local.get 0
                    i32.sub
                    i32.const 15
                    i32.and
                    i32.const 0
                    local.get 0
                    i32.const 8
                    i32.add
                    i32.const 15
                    i32.and
                    select
                    i32.add
                    local.tee 8
                    i32.const 1004131
                    i32.store offset=4
                    local.get 2
                    i32.const -8
                    local.get 2
                    i32.sub
                    i32.const 15
                    i32.and
                    i32.const 0
                    local.get 2
                    i32.const 8
                    i32.add
                    i32.const 15
                    i32.and
                    select
                    i32.add
                    local.tee 0
                    local.get 8
                    i32.sub
                    i32.const 1004128
                    i32.sub
                    local.set 1
                    local.get 8
                    i32.const 1004128
                    i32.add
                    local.set 6
                    local.get 0
                    local.get 4
                    i32.eq
                    if  ;; label = @9
                      i32.const 1352
                      local.get 6
                      i32.store
                      i32.const 1340
                      i32.const 1340
                      i32.load
                      local.get 1
                      i32.add
                      local.tee 0
                      i32.store
                      local.get 6
                      local.get 0
                      i32.const 1
                      i32.or
                      i32.store offset=4
                      br 3 (;@6;)
                    end
                    local.get 0
                    i32.const 1348
                    i32.load
                    i32.eq
                    if  ;; label = @9
                      i32.const 1348
                      local.get 6
                      i32.store
                      i32.const 1336
                      i32.const 1336
                      i32.load
                      local.get 1
                      i32.add
                      local.tee 0
                      i32.store
                      local.get 6
                      local.get 0
                      i32.const 1
                      i32.or
                      i32.store offset=4
                      local.get 0
                      local.get 6
                      i32.add
                      local.get 0
                      i32.store
                      br 3 (;@6;)
                    end
                    local.get 0
                    i32.load offset=4
                    local.tee 2
                    i32.const 3
                    i32.and
                    i32.const 1
                    i32.eq
                    if  ;; label = @9
                      local.get 2
                      i32.const -8
                      i32.and
                      local.set 10
                      block  ;; label = @10
                        local.get 2
                        i32.const 255
                        i32.le_u
                        if  ;; label = @11
                          local.get 0
                          i32.load offset=8
                          local.tee 3
                          local.get 2
                          i32.const 3
                          i32.shr_u
                          local.tee 5
                          i32.const 3
                          i32.shl
                          i32.const 1368
                          i32.add
                          i32.ne
                          drop
                          local.get 3
                          local.get 0
                          i32.load offset=12
                          local.tee 2
                          i32.eq
                          if  ;; label = @12
                            i32.const 1328
                            i32.const 1328
                            i32.load
                            i32.const -2
                            local.get 5
                            i32.rotl
                            i32.and
                            i32.store
                            br 2 (;@10;)
                          end
                          local.get 2
                          local.get 3
                          i32.store offset=8
                          local.get 3
                          local.get 2
                          i32.store offset=12
                          br 1 (;@10;)
                        end
                        local.get 0
                        i32.load offset=24
                        local.set 7
                        block  ;; label = @11
                          local.get 0
                          local.get 0
                          i32.load offset=12
                          local.tee 3
                          i32.ne
                          if  ;; label = @12
                            local.get 5
                            local.get 0
                            i32.load offset=8
                            local.tee 2
                            i32.le_u
                            if  ;; label = @13
                              local.get 2
                              i32.load offset=12
                              drop
                            end
                            local.get 3
                            local.get 2
                            i32.store offset=8
                            local.get 2
                            local.get 3
                            i32.store offset=12
                            br 1 (;@11;)
                          end
                          block  ;; label = @12
                            local.get 0
                            i32.const 20
                            i32.add
                            local.tee 2
                            i32.load
                            local.tee 4
                            br_if 0 (;@12;)
                            local.get 0
                            i32.const 16
                            i32.add
                            local.tee 2
                            i32.load
                            local.tee 4
                            br_if 0 (;@12;)
                            i32.const 0
                            local.set 3
                            br 1 (;@11;)
                          end
                          loop  ;; label = @12
                            local.get 2
                            local.set 5
                            local.get 4
                            local.tee 3
                            i32.const 20
                            i32.add
                            local.tee 2
                            i32.load
                            local.tee 4
                            br_if 0 (;@12;)
                            local.get 3
                            i32.const 16
                            i32.add
                            local.set 2
                            local.get 3
                            i32.load offset=16
                            local.tee 4
                            br_if 0 (;@12;)
                          end
                          local.get 5
                          i32.const 0
                          i32.store
                        end
                        local.get 7
                        i32.eqz
                        br_if 0 (;@10;)
                        block  ;; label = @11
                          local.get 0
                          local.get 0
                          i32.load offset=28
                          local.tee 2
                          i32.const 2
                          i32.shl
                          i32.const 1632
                          i32.add
                          local.tee 5
                          i32.load
                          i32.eq
                          if  ;; label = @12
                            local.get 5
                            local.get 3
                            i32.store
                            local.get 3
                            br_if 1 (;@11;)
                            i32.const 1332
                            i32.const 1332
                            i32.load
                            i32.const -2
                            local.get 2
                            i32.rotl
                            i32.and
                            i32.store
                            br 2 (;@10;)
                          end
                          local.get 7
                          i32.const 16
                          i32.const 20
                          local.get 7
                          i32.load offset=16
                          local.get 0
                          i32.eq
                          select
                          i32.add
                          local.get 3
                          i32.store
                          local.get 3
                          i32.eqz
                          br_if 1 (;@10;)
                        end
                        local.get 3
                        local.get 7
                        i32.store offset=24
                        local.get 0
                        i32.load offset=16
                        local.tee 2
                        if  ;; label = @11
                          local.get 3
                          local.get 2
                          i32.store offset=16
                          local.get 2
                          local.get 3
                          i32.store offset=24
                        end
                        local.get 0
                        i32.load offset=20
                        local.tee 2
                        i32.eqz
                        br_if 0 (;@10;)
                        local.get 3
                        i32.const 20
                        i32.add
                        local.get 2
                        i32.store
                        local.get 2
                        local.get 3
                        i32.store offset=24
                      end
                      local.get 1
                      local.get 10
                      i32.add
                      local.set 1
                      local.get 0
                      local.get 10
                      i32.add
                      local.set 0
                    end
                    local.get 0
                    local.get 0
                    i32.load offset=4
                    i32.const -2
                    i32.and
                    i32.store offset=4
                    local.get 1
                    local.get 6
                    i32.add
                    local.get 1
                    i32.store
                    local.get 6
                    local.get 1
                    i32.const 1
                    i32.or
                    i32.store offset=4
                    local.get 1
                    i32.const 255
                    i32.le_u
                    if  ;; label = @9
                      local.get 1
                      i32.const 3
                      i32.shr_u
                      local.tee 1
                      i32.const 3
                      i32.shl
                      i32.const 1368
                      i32.add
                      local.set 0
                      block (result i32)  ;; label = @10
                        i32.const 1328
                        i32.load
                        local.tee 2
                        i32.const 1
                        local.get 1
                        i32.shl
                        local.tee 1
                        i32.and
                        i32.eqz
                        if  ;; label = @11
                          i32.const 1328
                          local.get 1
                          local.get 2
                          i32.or
                          i32.store
                          local.get 0
                          br 1 (;@10;)
                        end
                        local.get 0
                        i32.load offset=8
                      end
                      local.tee 1
                      local.get 6
                      i32.store offset=12
                      local.get 0
                      local.get 6
                      i32.store offset=8
                      local.get 6
                      local.get 0
                      i32.store offset=12
                      local.get 6
                      local.get 1
                      i32.store offset=8
                      br 3 (;@6;)
                    end
                    local.get 6
                    block (result i32)  ;; label = @9
                      i32.const 0
                      local.get 1
                      i32.const 8
                      i32.shr_u
                      local.tee 0
                      i32.eqz
                      br_if 0 (;@9;)
                      drop
                      i32.const 31
                      local.get 1
                      i32.const 16777215
                      i32.gt_u
                      br_if 0 (;@9;)
                      drop
                      local.get 0
                      local.get 0
                      i32.const 1048320
                      i32.add
                      i32.const 16
                      i32.shr_u
                      i32.const 8
                      i32.and
                      local.tee 0
                      i32.shl
                      local.tee 2
                      local.get 2
                      i32.const 520192
                      i32.add
                      i32.const 16
                      i32.shr_u
                      i32.const 4
                      i32.and
                      local.tee 2
                      i32.shl
                      local.tee 3
                      local.get 3
                      i32.const 245760
                      i32.add
                      i32.const 16
                      i32.shr_u
                      i32.const 2
                      i32.and
                      local.tee 3
                      i32.shl
                      i32.const 15
                      i32.shr_u
                      local.get 0
                      local.get 2
                      i32.or
                      local.get 3
                      i32.or
                      i32.sub
                      local.tee 0
                      i32.const 1
                      i32.shl
                      local.get 1
                      local.get 0
                      i32.const 21
                      i32.add
                      i32.shr_u
                      i32.const 1
                      i32.and
                      i32.or
                      i32.const 28
                      i32.add
                    end
                    local.tee 0
                    i32.store offset=28
                    local.get 6
                    i64.const 0
                    i64.store offset=16 align=4
                    local.get 0
                    i32.const 2
                    i32.shl
                    i32.const 1632
                    i32.add
                    local.set 3
                    i32.const 1332
                    i32.load
                    local.tee 2
                    i32.const 1
                    local.get 0
                    i32.shl
                    local.tee 5
                    i32.and
                    i32.eqz
                    if  ;; label = @9
                      local.get 3
                      local.get 6
                      i32.store
                      i32.const 1332
                      local.get 2
                      local.get 5
                      i32.or
                      i32.store
                      local.get 6
                      local.get 3
                      i32.store offset=24
                      local.get 6
                      local.get 6
                      i32.store offset=8
                      local.get 6
                      local.get 6
                      i32.store offset=12
                      br 3 (;@6;)
                    end
                    local.get 1
                    i32.const 0
                    i32.const 25
                    local.get 0
                    i32.const 1
                    i32.shr_u
                    i32.sub
                    local.get 0
                    i32.const 31
                    i32.eq
                    select
                    i32.shl
                    local.set 2
                    local.get 3
                    i32.load
                    local.set 0
                    loop  ;; label = @9
                      local.get 0
                      local.tee 3
                      i32.load offset=4
                      i32.const -8
                      i32.and
                      local.get 1
                      i32.eq
                      br_if 2 (;@7;)
                      local.get 2
                      i32.const 29
                      i32.shr_u
                      local.set 0
                      local.get 2
                      i32.const 1
                      i32.shl
                      local.set 2
                      local.get 3
                      local.get 0
                      i32.const 4
                      i32.and
                      i32.add
                      i32.const 16
                      i32.add
                      local.tee 5
                      i32.load
                      local.tee 0
                      br_if 0 (;@9;)
                    end
                    local.get 5
                    local.get 6
                    i32.store
                    local.get 6
                    local.get 3
                    i32.store offset=24
                    local.get 6
                    local.get 6
                    i32.store offset=12
                    local.get 6
                    local.get 6
                    i32.store offset=8
                    br 2 (;@6;)
                  end
                  local.get 0
                  i32.const -8
                  local.get 0
                  i32.sub
                  i32.const 15
                  i32.and
                  i32.const 0
                  local.get 0
                  i32.const 8
                  i32.add
                  i32.const 15
                  i32.and
                  select
                  local.tee 1
                  i32.add
                  local.tee 6
                  local.get 3
                  i32.const -56
                  i32.add
                  local.tee 2
                  local.get 1
                  i32.sub
                  local.tee 1
                  i32.const 1
                  i32.or
                  i32.store offset=4
                  local.get 0
                  local.get 2
                  i32.add
                  i32.const 56
                  i32.store offset=4
                  local.get 4
                  local.get 5
                  i32.const 55
                  local.get 5
                  i32.sub
                  i32.const 15
                  i32.and
                  i32.const 0
                  local.get 5
                  i32.const -55
                  i32.add
                  i32.const 15
                  i32.and
                  select
                  i32.add
                  i32.const -63
                  i32.add
                  local.tee 2
                  local.get 2
                  local.get 4
                  i32.const 16
                  i32.add
                  i32.lt_u
                  select
                  local.tee 2
                  i32.const 35
                  i32.store offset=4
                  i32.const 1356
                  i32.const 1816
                  i32.load
                  i32.store
                  i32.const 1340
                  local.get 1
                  i32.store
                  i32.const 1352
                  local.get 6
                  i32.store
                  local.get 2
                  i32.const 16
                  i32.add
                  i32.const 1784
                  i64.load align=4
                  i64.store align=4
                  local.get 2
                  i32.const 1776
                  i64.load align=4
                  i64.store offset=8 align=4
                  i32.const 1784
                  local.get 2
                  i32.const 8
                  i32.add
                  i32.store
                  i32.const 1780
                  local.get 3
                  i32.store
                  i32.const 1776
                  local.get 0
                  i32.store
                  i32.const 1788
                  i32.const 0
                  i32.store
                  local.get 2
                  i32.const 36
                  i32.add
                  local.set 1
                  loop  ;; label = @8
                    local.get 1
                    i32.const 7
                    i32.store
                    local.get 5
                    local.get 1
                    i32.const 4
                    i32.add
                    local.tee 1
                    i32.gt_u
                    br_if 0 (;@8;)
                  end
                  local.get 2
                  local.get 4
                  i32.eq
                  br_if 3 (;@4;)
                  local.get 2
                  local.get 2
                  i32.load offset=4
                  i32.const -2
                  i32.and
                  i32.store offset=4
                  local.get 2
                  local.get 2
                  local.get 4
                  i32.sub
                  local.tee 3
                  i32.store
                  local.get 4
                  local.get 3
                  i32.const 1
                  i32.or
                  i32.store offset=4
                  local.get 3
                  i32.const 255
                  i32.le_u
                  if  ;; label = @8
                    local.get 3
                    i32.const 3
                    i32.shr_u
                    local.tee 1
                    i32.const 3
                    i32.shl
                    i32.const 1368
                    i32.add
                    local.set 0
                    block (result i32)  ;; label = @9
                      i32.const 1328
                      i32.load
                      local.tee 2
                      i32.const 1
                      local.get 1
                      i32.shl
                      local.tee 1
                      i32.and
                      i32.eqz
                      if  ;; label = @10
                        i32.const 1328
                        local.get 1
                        local.get 2
                        i32.or
                        i32.store
                        local.get 0
                        br 1 (;@9;)
                      end
                      local.get 0
                      i32.load offset=8
                    end
                    local.tee 1
                    local.get 4
                    i32.store offset=12
                    local.get 0
                    local.get 4
                    i32.store offset=8
                    local.get 4
                    local.get 0
                    i32.store offset=12
                    local.get 4
                    local.get 1
                    i32.store offset=8
                    br 4 (;@4;)
                  end
                  local.get 4
                  i64.const 0
                  i64.store offset=16 align=4
                  local.get 4
                  i32.const 28
                  i32.add
                  block (result i32)  ;; label = @8
                    i32.const 0
                    local.get 3
                    i32.const 8
                    i32.shr_u
                    local.tee 0
                    i32.eqz
                    br_if 0 (;@8;)
                    drop
                    i32.const 31
                    local.get 3
                    i32.const 16777215
                    i32.gt_u
                    br_if 0 (;@8;)
                    drop
                    local.get 0
                    local.get 0
                    i32.const 1048320
                    i32.add
                    i32.const 16
                    i32.shr_u
                    i32.const 8
                    i32.and
                    local.tee 0
                    i32.shl
                    local.tee 1
                    local.get 1
                    i32.const 520192
                    i32.add
                    i32.const 16
                    i32.shr_u
                    i32.const 4
                    i32.and
                    local.tee 1
                    i32.shl
                    local.tee 2
                    local.get 2
                    i32.const 245760
                    i32.add
                    i32.const 16
                    i32.shr_u
                    i32.const 2
                    i32.and
                    local.tee 2
                    i32.shl
                    i32.const 15
                    i32.shr_u
                    local.get 0
                    local.get 1
                    i32.or
                    local.get 2
                    i32.or
                    i32.sub
                    local.tee 0
                    i32.const 1
                    i32.shl
                    local.get 3
                    local.get 0
                    i32.const 21
                    i32.add
                    i32.shr_u
                    i32.const 1
                    i32.and
                    i32.or
                    i32.const 28
                    i32.add
                  end
                  local.tee 0
                  i32.store
                  local.get 0
                  i32.const 2
                  i32.shl
                  i32.const 1632
                  i32.add
                  local.set 2
                  i32.const 1332
                  i32.load
                  local.tee 1
                  i32.const 1
                  local.get 0
                  i32.shl
                  local.tee 5
                  i32.and
                  i32.eqz
                  if  ;; label = @8
                    local.get 2
                    local.get 4
                    i32.store
                    i32.const 1332
                    local.get 1
                    local.get 5
                    i32.or
                    i32.store
                    local.get 4
                    i32.const 24
                    i32.add
                    local.get 2
                    i32.store
                    local.get 4
                    local.get 4
                    i32.store offset=8
                    local.get 4
                    local.get 4
                    i32.store offset=12
                    br 4 (;@4;)
                  end
                  local.get 3
                  i32.const 0
                  i32.const 25
                  local.get 0
                  i32.const 1
                  i32.shr_u
                  i32.sub
                  local.get 0
                  i32.const 31
                  i32.eq
                  select
                  i32.shl
                  local.set 1
                  local.get 2
                  i32.load
                  local.set 0
                  loop  ;; label = @8
                    local.get 0
                    local.tee 2
                    i32.load offset=4
                    i32.const -8
                    i32.and
                    local.get 3
                    i32.eq
                    br_if 3 (;@5;)
                    local.get 1
                    i32.const 29
                    i32.shr_u
                    local.set 0
                    local.get 1
                    i32.const 1
                    i32.shl
                    local.set 1
                    local.get 2
                    local.get 0
                    i32.const 4
                    i32.and
                    i32.add
                    i32.const 16
                    i32.add
                    local.tee 5
                    i32.load
                    local.tee 0
                    br_if 0 (;@8;)
                  end
                  local.get 5
                  local.get 4
                  i32.store
                  local.get 4
                  i32.const 24
                  i32.add
                  local.get 2
                  i32.store
                  local.get 4
                  local.get 4
                  i32.store offset=12
                  local.get 4
                  local.get 4
                  i32.store offset=8
                  br 3 (;@4;)
                end
                local.get 3
                i32.load offset=8
                local.set 0
                local.get 3
                local.get 6
                i32.store offset=8
                local.get 0
                local.get 6
                i32.store offset=12
                local.get 6
                i32.const 0
                i32.store offset=24
                local.get 6
                local.get 0
                i32.store offset=8
                local.get 6
                local.get 3
                i32.store offset=12
              end
              local.get 8
              i32.const 8
              i32.add
              local.set 1
              br 4 (;@1;)
            end
            local.get 2
            i32.load offset=8
            local.set 0
            local.get 2
            local.get 4
            i32.store offset=8
            local.get 0
            local.get 4
            i32.store offset=12
            local.get 4
            i32.const 24
            i32.add
            i32.const 0
            i32.store
            local.get 4
            local.get 0
            i32.store offset=8
            local.get 4
            local.get 2
            i32.store offset=12
          end
          i32.const 1340
          i32.load
          local.tee 1
          i32.const 1004128
          i32.le_u
          br_if 0 (;@3;)
          i32.const 1352
          i32.load
          local.tee 0
          i32.const 1004128
          i32.add
          local.tee 2
          local.get 1
          i32.const 1004128
          i32.sub
          local.tee 1
          i32.const 1
          i32.or
          i32.store offset=4
          i32.const 1340
          local.get 1
          i32.store
          i32.const 1352
          local.get 2
          i32.store
          local.get 0
          i32.const 1004131
          i32.store offset=4
          local.get 0
          i32.const 8
          i32.add
          local.set 1
          br 2 (;@1;)
        end
        i32.const 0
        local.set 1
        i32.const 1824
        i32.const 48
        i32.store
        br 1 (;@1;)
      end
      block  ;; label = @2
        local.get 7
        i32.eqz
        br_if 0 (;@2;)
        block  ;; label = @3
          local.get 5
          i32.load offset=28
          local.tee 1
          i32.const 2
          i32.shl
          i32.const 1632
          i32.add
          local.tee 2
          i32.load
          local.get 5
          i32.eq
          if  ;; label = @4
            local.get 2
            local.get 0
            i32.store
            local.get 0
            br_if 1 (;@3;)
            i32.const 1332
            local.get 6
            i32.const -2
            local.get 1
            i32.rotl
            i32.and
            local.tee 6
            i32.store
            br 2 (;@2;)
          end
          local.get 7
          i32.const 16
          i32.const 20
          local.get 7
          i32.load offset=16
          local.get 5
          i32.eq
          select
          i32.add
          local.get 0
          i32.store
          local.get 0
          i32.eqz
          br_if 1 (;@2;)
        end
        local.get 0
        local.get 7
        i32.store offset=24
        local.get 5
        i32.load offset=16
        local.tee 1
        if  ;; label = @3
          local.get 0
          local.get 1
          i32.store offset=16
          local.get 1
          local.get 0
          i32.store offset=24
        end
        local.get 5
        i32.const 20
        i32.add
        i32.load
        local.tee 1
        i32.eqz
        br_if 0 (;@2;)
        local.get 0
        i32.const 20
        i32.add
        local.get 1
        i32.store
        local.get 1
        local.get 0
        i32.store offset=24
      end
      block  ;; label = @2
        local.get 3
        i32.const 15
        i32.le_u
        if  ;; label = @3
          local.get 5
          local.get 3
          i32.const 1004128
          i32.add
          local.tee 0
          i32.const 3
          i32.or
          i32.store offset=4
          local.get 0
          local.get 5
          i32.add
          local.tee 0
          local.get 0
          i32.load offset=4
          i32.const 1
          i32.or
          i32.store offset=4
          br 1 (;@2;)
        end
        local.get 5
        i32.const 1004128
        i32.add
        local.tee 2
        local.get 3
        i32.const 1
        i32.or
        i32.store offset=4
        local.get 5
        i32.const 1004131
        i32.store offset=4
        local.get 2
        local.get 3
        i32.add
        local.get 3
        i32.store
        local.get 3
        i32.const 255
        i32.le_u
        if  ;; label = @3
          local.get 3
          i32.const 3
          i32.shr_u
          local.tee 1
          i32.const 3
          i32.shl
          i32.const 1368
          i32.add
          local.set 0
          block (result i32)  ;; label = @4
            i32.const 1328
            i32.load
            local.tee 3
            i32.const 1
            local.get 1
            i32.shl
            local.tee 1
            i32.and
            i32.eqz
            if  ;; label = @5
              i32.const 1328
              local.get 1
              local.get 3
              i32.or
              i32.store
              local.get 0
              br 1 (;@4;)
            end
            local.get 0
            i32.load offset=8
          end
          local.tee 1
          local.get 2
          i32.store offset=12
          local.get 0
          local.get 2
          i32.store offset=8
          local.get 2
          local.get 0
          i32.store offset=12
          local.get 2
          local.get 1
          i32.store offset=8
          br 1 (;@2;)
        end
        local.get 2
        block (result i32)  ;; label = @3
          i32.const 0
          local.get 3
          i32.const 8
          i32.shr_u
          local.tee 0
          i32.eqz
          br_if 0 (;@3;)
          drop
          i32.const 31
          local.get 3
          i32.const 16777215
          i32.gt_u
          br_if 0 (;@3;)
          drop
          local.get 0
          local.get 0
          i32.const 1048320
          i32.add
          i32.const 16
          i32.shr_u
          i32.const 8
          i32.and
          local.tee 0
          i32.shl
          local.tee 1
          local.get 1
          i32.const 520192
          i32.add
          i32.const 16
          i32.shr_u
          i32.const 4
          i32.and
          local.tee 1
          i32.shl
          local.tee 4
          local.get 4
          i32.const 245760
          i32.add
          i32.const 16
          i32.shr_u
          i32.const 2
          i32.and
          local.tee 4
          i32.shl
          i32.const 15
          i32.shr_u
          local.get 0
          local.get 1
          i32.or
          local.get 4
          i32.or
          i32.sub
          local.tee 0
          i32.const 1
          i32.shl
          local.get 3
          local.get 0
          i32.const 21
          i32.add
          i32.shr_u
          i32.const 1
          i32.and
          i32.or
          i32.const 28
          i32.add
        end
        local.tee 0
        i32.store offset=28
        local.get 2
        i64.const 0
        i64.store offset=16 align=4
        local.get 0
        i32.const 2
        i32.shl
        i32.const 1632
        i32.add
        local.set 4
        local.get 6
        i32.const 1
        local.get 0
        i32.shl
        local.tee 1
        i32.and
        i32.eqz
        if  ;; label = @3
          local.get 4
          local.get 2
          i32.store
          i32.const 1332
          local.get 1
          local.get 6
          i32.or
          i32.store
          local.get 2
          local.get 4
          i32.store offset=24
          local.get 2
          local.get 2
          i32.store offset=8
          local.get 2
          local.get 2
          i32.store offset=12
          br 1 (;@2;)
        end
        local.get 3
        i32.const 0
        i32.const 25
        local.get 0
        i32.const 1
        i32.shr_u
        i32.sub
        local.get 0
        i32.const 31
        i32.eq
        select
        i32.shl
        local.set 1
        local.get 4
        i32.load
        local.set 4
        block  ;; label = @3
          loop  ;; label = @4
            local.get 4
            local.tee 0
            i32.load offset=4
            i32.const -8
            i32.and
            local.get 3
            i32.eq
            br_if 1 (;@3;)
            local.get 1
            i32.const 29
            i32.shr_u
            local.set 4
            local.get 1
            i32.const 1
            i32.shl
            local.set 1
            local.get 0
            local.get 4
            i32.const 4
            i32.and
            i32.add
            i32.const 16
            i32.add
            local.tee 6
            i32.load
            local.tee 4
            br_if 0 (;@4;)
          end
          local.get 6
          local.get 2
          i32.store
          local.get 2
          local.get 0
          i32.store offset=24
          local.get 2
          local.get 2
          i32.store offset=12
          local.get 2
          local.get 2
          i32.store offset=8
          br 1 (;@2;)
        end
        local.get 0
        i32.load offset=8
        local.set 1
        local.get 0
        local.get 2
        i32.store offset=8
        local.get 1
        local.get 2
        i32.store offset=12
        local.get 2
        i32.const 0
        i32.store offset=24
        local.get 2
        local.get 1
        i32.store offset=8
        local.get 2
        local.get 0
        i32.store offset=12
      end
      local.get 5
      i32.const 8
      i32.add
      local.set 1
    end
    local.get 9
    i32.const 16
    i32.add
    global.set 0
    local.get 1)
  (func (;9;) (type 4) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32)
    block  ;; label = @1
      local.get 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.const -8
      i32.add
      local.tee 3
      local.get 0
      i32.const -4
      i32.add
      i32.load
      local.tee 1
      i32.const -8
      i32.and
      local.tee 0
      i32.add
      local.set 5
      block  ;; label = @2
        local.get 1
        i32.const 1
        i32.and
        br_if 0 (;@2;)
        local.get 1
        i32.const 3
        i32.and
        i32.eqz
        br_if 1 (;@1;)
        local.get 3
        local.get 3
        i32.load
        local.tee 2
        i32.sub
        local.tee 3
        i32.const 1344
        i32.load
        local.tee 4
        i32.lt_u
        br_if 1 (;@1;)
        local.get 0
        local.get 2
        i32.add
        local.set 0
        local.get 3
        i32.const 1348
        i32.load
        i32.ne
        if  ;; label = @3
          local.get 2
          i32.const 255
          i32.le_u
          if  ;; label = @4
            local.get 3
            i32.load offset=8
            local.tee 4
            local.get 2
            i32.const 3
            i32.shr_u
            local.tee 2
            i32.const 3
            i32.shl
            i32.const 1368
            i32.add
            i32.ne
            drop
            local.get 4
            local.get 3
            i32.load offset=12
            local.tee 1
            i32.eq
            if  ;; label = @5
              i32.const 1328
              i32.const 1328
              i32.load
              i32.const -2
              local.get 2
              i32.rotl
              i32.and
              i32.store
              br 3 (;@2;)
            end
            local.get 1
            local.get 4
            i32.store offset=8
            local.get 4
            local.get 1
            i32.store offset=12
            br 2 (;@2;)
          end
          local.get 3
          i32.load offset=24
          local.set 6
          block  ;; label = @4
            local.get 3
            local.get 3
            i32.load offset=12
            local.tee 1
            i32.ne
            if  ;; label = @5
              local.get 4
              local.get 3
              i32.load offset=8
              local.tee 2
              i32.le_u
              if  ;; label = @6
                local.get 2
                i32.load offset=12
                drop
              end
              local.get 1
              local.get 2
              i32.store offset=8
              local.get 2
              local.get 1
              i32.store offset=12
              br 1 (;@4;)
            end
            block  ;; label = @5
              local.get 3
              i32.const 20
              i32.add
              local.tee 2
              i32.load
              local.tee 4
              br_if 0 (;@5;)
              local.get 3
              i32.const 16
              i32.add
              local.tee 2
              i32.load
              local.tee 4
              br_if 0 (;@5;)
              i32.const 0
              local.set 1
              br 1 (;@4;)
            end
            loop  ;; label = @5
              local.get 2
              local.set 7
              local.get 4
              local.tee 1
              i32.const 20
              i32.add
              local.tee 2
              i32.load
              local.tee 4
              br_if 0 (;@5;)
              local.get 1
              i32.const 16
              i32.add
              local.set 2
              local.get 1
              i32.load offset=16
              local.tee 4
              br_if 0 (;@5;)
            end
            local.get 7
            i32.const 0
            i32.store
          end
          local.get 6
          i32.eqz
          br_if 1 (;@2;)
          block  ;; label = @4
            local.get 3
            local.get 3
            i32.load offset=28
            local.tee 2
            i32.const 2
            i32.shl
            i32.const 1632
            i32.add
            local.tee 4
            i32.load
            i32.eq
            if  ;; label = @5
              local.get 4
              local.get 1
              i32.store
              local.get 1
              br_if 1 (;@4;)
              i32.const 1332
              i32.const 1332
              i32.load
              i32.const -2
              local.get 2
              i32.rotl
              i32.and
              i32.store
              br 3 (;@2;)
            end
            local.get 6
            i32.const 16
            i32.const 20
            local.get 6
            i32.load offset=16
            local.get 3
            i32.eq
            select
            i32.add
            local.get 1
            i32.store
            local.get 1
            i32.eqz
            br_if 2 (;@2;)
          end
          local.get 1
          local.get 6
          i32.store offset=24
          local.get 3
          i32.load offset=16
          local.tee 2
          if  ;; label = @4
            local.get 1
            local.get 2
            i32.store offset=16
            local.get 2
            local.get 1
            i32.store offset=24
          end
          local.get 3
          i32.load offset=20
          local.tee 2
          i32.eqz
          br_if 1 (;@2;)
          local.get 1
          i32.const 20
          i32.add
          local.get 2
          i32.store
          local.get 2
          local.get 1
          i32.store offset=24
          br 1 (;@2;)
        end
        local.get 5
        i32.load offset=4
        local.tee 1
        i32.const 3
        i32.and
        i32.const 3
        i32.ne
        br_if 0 (;@2;)
        local.get 5
        local.get 1
        i32.const -2
        i32.and
        i32.store offset=4
        i32.const 1336
        local.get 0
        i32.store
        local.get 0
        local.get 3
        i32.add
        local.get 0
        i32.store
        local.get 3
        local.get 0
        i32.const 1
        i32.or
        i32.store offset=4
        return
      end
      local.get 5
      local.get 3
      i32.le_u
      br_if 0 (;@1;)
      local.get 5
      i32.load offset=4
      local.tee 1
      i32.const 1
      i32.and
      i32.eqz
      br_if 0 (;@1;)
      block  ;; label = @2
        local.get 1
        i32.const 2
        i32.and
        i32.eqz
        if  ;; label = @3
          local.get 5
          i32.const 1352
          i32.load
          i32.eq
          if  ;; label = @4
            i32.const 1352
            local.get 3
            i32.store
            i32.const 1340
            i32.const 1340
            i32.load
            local.get 0
            i32.add
            local.tee 0
            i32.store
            local.get 3
            local.get 0
            i32.const 1
            i32.or
            i32.store offset=4
            local.get 3
            i32.const 1348
            i32.load
            i32.ne
            br_if 3 (;@1;)
            i32.const 1336
            i32.const 0
            i32.store
            i32.const 1348
            i32.const 0
            i32.store
            return
          end
          local.get 5
          i32.const 1348
          i32.load
          i32.eq
          if  ;; label = @4
            i32.const 1348
            local.get 3
            i32.store
            i32.const 1336
            i32.const 1336
            i32.load
            local.get 0
            i32.add
            local.tee 0
            i32.store
            local.get 3
            local.get 0
            i32.const 1
            i32.or
            i32.store offset=4
            local.get 0
            local.get 3
            i32.add
            local.get 0
            i32.store
            return
          end
          local.get 1
          i32.const -8
          i32.and
          local.get 0
          i32.add
          local.set 0
          block  ;; label = @4
            local.get 1
            i32.const 255
            i32.le_u
            if  ;; label = @5
              local.get 5
              i32.load offset=12
              local.set 2
              local.get 5
              i32.load offset=8
              local.tee 4
              local.get 1
              i32.const 3
              i32.shr_u
              local.tee 1
              i32.const 3
              i32.shl
              i32.const 1368
              i32.add
              local.tee 7
              i32.ne
              if  ;; label = @6
                i32.const 1344
                i32.load
                drop
              end
              local.get 2
              local.get 4
              i32.eq
              if  ;; label = @6
                i32.const 1328
                i32.const 1328
                i32.load
                i32.const -2
                local.get 1
                i32.rotl
                i32.and
                i32.store
                br 2 (;@4;)
              end
              local.get 2
              local.get 7
              i32.ne
              if  ;; label = @6
                i32.const 1344
                i32.load
                drop
              end
              local.get 2
              local.get 4
              i32.store offset=8
              local.get 4
              local.get 2
              i32.store offset=12
              br 1 (;@4;)
            end
            local.get 5
            i32.load offset=24
            local.set 6
            block  ;; label = @5
              local.get 5
              local.get 5
              i32.load offset=12
              local.tee 1
              i32.ne
              if  ;; label = @6
                i32.const 1344
                i32.load
                local.get 5
                i32.load offset=8
                local.tee 2
                i32.le_u
                if  ;; label = @7
                  local.get 2
                  i32.load offset=12
                  drop
                end
                local.get 1
                local.get 2
                i32.store offset=8
                local.get 2
                local.get 1
                i32.store offset=12
                br 1 (;@5;)
              end
              block  ;; label = @6
                local.get 5
                i32.const 20
                i32.add
                local.tee 2
                i32.load
                local.tee 4
                br_if 0 (;@6;)
                local.get 5
                i32.const 16
                i32.add
                local.tee 2
                i32.load
                local.tee 4
                br_if 0 (;@6;)
                i32.const 0
                local.set 1
                br 1 (;@5;)
              end
              loop  ;; label = @6
                local.get 2
                local.set 7
                local.get 4
                local.tee 1
                i32.const 20
                i32.add
                local.tee 2
                i32.load
                local.tee 4
                br_if 0 (;@6;)
                local.get 1
                i32.const 16
                i32.add
                local.set 2
                local.get 1
                i32.load offset=16
                local.tee 4
                br_if 0 (;@6;)
              end
              local.get 7
              i32.const 0
              i32.store
            end
            local.get 6
            i32.eqz
            br_if 0 (;@4;)
            block  ;; label = @5
              local.get 5
              local.get 5
              i32.load offset=28
              local.tee 2
              i32.const 2
              i32.shl
              i32.const 1632
              i32.add
              local.tee 4
              i32.load
              i32.eq
              if  ;; label = @6
                local.get 4
                local.get 1
                i32.store
                local.get 1
                br_if 1 (;@5;)
                i32.const 1332
                i32.const 1332
                i32.load
                i32.const -2
                local.get 2
                i32.rotl
                i32.and
                i32.store
                br 2 (;@4;)
              end
              local.get 6
              i32.const 16
              i32.const 20
              local.get 6
              i32.load offset=16
              local.get 5
              i32.eq
              select
              i32.add
              local.get 1
              i32.store
              local.get 1
              i32.eqz
              br_if 1 (;@4;)
            end
            local.get 1
            local.get 6
            i32.store offset=24
            local.get 5
            i32.load offset=16
            local.tee 2
            if  ;; label = @5
              local.get 1
              local.get 2
              i32.store offset=16
              local.get 2
              local.get 1
              i32.store offset=24
            end
            local.get 5
            i32.load offset=20
            local.tee 2
            i32.eqz
            br_if 0 (;@4;)
            local.get 1
            i32.const 20
            i32.add
            local.get 2
            i32.store
            local.get 2
            local.get 1
            i32.store offset=24
          end
          local.get 0
          local.get 3
          i32.add
          local.get 0
          i32.store
          local.get 3
          local.get 0
          i32.const 1
          i32.or
          i32.store offset=4
          local.get 3
          i32.const 1348
          i32.load
          i32.ne
          br_if 1 (;@2;)
          i32.const 1336
          local.get 0
          i32.store
          return
        end
        local.get 5
        local.get 1
        i32.const -2
        i32.and
        i32.store offset=4
        local.get 0
        local.get 3
        i32.add
        local.get 0
        i32.store
        local.get 3
        local.get 0
        i32.const 1
        i32.or
        i32.store offset=4
      end
      local.get 0
      i32.const 255
      i32.le_u
      if  ;; label = @2
        local.get 0
        i32.const 3
        i32.shr_u
        local.tee 1
        i32.const 3
        i32.shl
        i32.const 1368
        i32.add
        local.set 0
        block (result i32)  ;; label = @3
          i32.const 1328
          i32.load
          local.tee 2
          i32.const 1
          local.get 1
          i32.shl
          local.tee 1
          i32.and
          i32.eqz
          if  ;; label = @4
            i32.const 1328
            local.get 1
            local.get 2
            i32.or
            i32.store
            local.get 0
            br 1 (;@3;)
          end
          local.get 0
          i32.load offset=8
        end
        local.tee 2
        local.get 3
        i32.store offset=12
        local.get 0
        local.get 3
        i32.store offset=8
        local.get 3
        local.get 0
        i32.store offset=12
        local.get 3
        local.get 2
        i32.store offset=8
        return
      end
      local.get 3
      i64.const 0
      i64.store offset=16 align=4
      local.get 3
      i32.const 28
      i32.add
      block (result i32)  ;; label = @2
        i32.const 0
        local.get 0
        i32.const 8
        i32.shr_u
        local.tee 1
        i32.eqz
        br_if 0 (;@2;)
        drop
        i32.const 31
        local.get 0
        i32.const 16777215
        i32.gt_u
        br_if 0 (;@2;)
        drop
        local.get 1
        local.get 1
        i32.const 1048320
        i32.add
        i32.const 16
        i32.shr_u
        i32.const 8
        i32.and
        local.tee 1
        i32.shl
        local.tee 2
        local.get 2
        i32.const 520192
        i32.add
        i32.const 16
        i32.shr_u
        i32.const 4
        i32.and
        local.tee 2
        i32.shl
        local.tee 4
        local.get 4
        i32.const 245760
        i32.add
        i32.const 16
        i32.shr_u
        i32.const 2
        i32.and
        local.tee 4
        i32.shl
        i32.const 15
        i32.shr_u
        local.get 1
        local.get 2
        i32.or
        local.get 4
        i32.or
        i32.sub
        local.tee 1
        i32.const 1
        i32.shl
        local.get 0
        local.get 1
        i32.const 21
        i32.add
        i32.shr_u
        i32.const 1
        i32.and
        i32.or
        i32.const 28
        i32.add
      end
      local.tee 2
      i32.store
      local.get 2
      i32.const 2
      i32.shl
      i32.const 1632
      i32.add
      local.set 1
      block  ;; label = @2
        i32.const 1332
        i32.load
        local.tee 4
        i32.const 1
        local.get 2
        i32.shl
        local.tee 7
        i32.and
        i32.eqz
        if  ;; label = @3
          local.get 1
          local.get 3
          i32.store
          i32.const 1332
          local.get 4
          local.get 7
          i32.or
          i32.store
          local.get 3
          i32.const 24
          i32.add
          local.get 1
          i32.store
          local.get 3
          local.get 3
          i32.store offset=8
          local.get 3
          local.get 3
          i32.store offset=12
          br 1 (;@2;)
        end
        local.get 0
        i32.const 0
        i32.const 25
        local.get 2
        i32.const 1
        i32.shr_u
        i32.sub
        local.get 2
        i32.const 31
        i32.eq
        select
        i32.shl
        local.set 2
        local.get 1
        i32.load
        local.set 1
        block  ;; label = @3
          loop  ;; label = @4
            local.get 1
            local.tee 4
            i32.load offset=4
            i32.const -8
            i32.and
            local.get 0
            i32.eq
            br_if 1 (;@3;)
            local.get 2
            i32.const 29
            i32.shr_u
            local.set 1
            local.get 2
            i32.const 1
            i32.shl
            local.set 2
            local.get 4
            local.get 1
            i32.const 4
            i32.and
            i32.add
            i32.const 16
            i32.add
            local.tee 7
            i32.load
            local.tee 1
            br_if 0 (;@4;)
          end
          local.get 7
          local.get 3
          i32.store
          local.get 3
          i32.const 24
          i32.add
          local.get 4
          i32.store
          local.get 3
          local.get 3
          i32.store offset=12
          local.get 3
          local.get 3
          i32.store offset=8
          br 1 (;@2;)
        end
        local.get 4
        i32.load offset=8
        local.set 0
        local.get 4
        local.get 3
        i32.store offset=8
        local.get 0
        local.get 3
        i32.store offset=12
        local.get 3
        i32.const 24
        i32.add
        i32.const 0
        i32.store
        local.get 3
        local.get 0
        i32.store offset=8
        local.get 3
        local.get 4
        i32.store offset=12
      end
      i32.const 1360
      i32.const 1360
      i32.load
      i32.const -1
      i32.add
      local.tee 0
      i32.store
      local.get 0
      br_if 0 (;@1;)
      i32.const 1784
      local.set 3
      loop  ;; label = @2
        local.get 3
        i32.load
        local.tee 0
        i32.const 8
        i32.add
        local.set 3
        local.get 0
        br_if 0 (;@2;)
      end
      i32.const 1360
      i32.const -1
      i32.store
    end)
  (func (;10;) (type 6) (param i32 i32)
    (local i32 i32 i32 i32 i32 i32)
    local.get 0
    local.get 1
    i32.add
    local.set 5
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.load offset=4
        local.tee 2
        i32.const 1
        i32.and
        br_if 0 (;@2;)
        local.get 2
        i32.const 3
        i32.and
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        i32.load
        local.tee 3
        local.get 1
        i32.add
        local.set 1
        local.get 0
        local.get 3
        i32.sub
        local.tee 0
        i32.const 1348
        i32.load
        i32.ne
        if  ;; label = @3
          i32.const 1344
          i32.load
          local.set 4
          local.get 3
          i32.const 255
          i32.le_u
          if  ;; label = @4
            local.get 0
            i32.load offset=8
            local.tee 4
            local.get 3
            i32.const 3
            i32.shr_u
            local.tee 3
            i32.const 3
            i32.shl
            i32.const 1368
            i32.add
            i32.ne
            drop
            local.get 4
            local.get 0
            i32.load offset=12
            local.tee 2
            i32.eq
            if  ;; label = @5
              i32.const 1328
              i32.const 1328
              i32.load
              i32.const -2
              local.get 3
              i32.rotl
              i32.and
              i32.store
              br 3 (;@2;)
            end
            local.get 2
            local.get 4
            i32.store offset=8
            local.get 4
            local.get 2
            i32.store offset=12
            br 2 (;@2;)
          end
          local.get 0
          i32.load offset=24
          local.set 6
          block  ;; label = @4
            local.get 0
            local.get 0
            i32.load offset=12
            local.tee 2
            i32.ne
            if  ;; label = @5
              local.get 4
              local.get 0
              i32.load offset=8
              local.tee 3
              i32.le_u
              if  ;; label = @6
                local.get 3
                i32.load offset=12
                drop
              end
              local.get 2
              local.get 3
              i32.store offset=8
              local.get 3
              local.get 2
              i32.store offset=12
              br 1 (;@4;)
            end
            block  ;; label = @5
              local.get 0
              i32.const 20
              i32.add
              local.tee 3
              i32.load
              local.tee 4
              br_if 0 (;@5;)
              local.get 0
              i32.const 16
              i32.add
              local.tee 3
              i32.load
              local.tee 4
              br_if 0 (;@5;)
              i32.const 0
              local.set 2
              br 1 (;@4;)
            end
            loop  ;; label = @5
              local.get 3
              local.set 7
              local.get 4
              local.tee 2
              i32.const 20
              i32.add
              local.tee 3
              i32.load
              local.tee 4
              br_if 0 (;@5;)
              local.get 2
              i32.const 16
              i32.add
              local.set 3
              local.get 2
              i32.load offset=16
              local.tee 4
              br_if 0 (;@5;)
            end
            local.get 7
            i32.const 0
            i32.store
          end
          local.get 6
          i32.eqz
          br_if 1 (;@2;)
          block  ;; label = @4
            local.get 0
            local.get 0
            i32.load offset=28
            local.tee 3
            i32.const 2
            i32.shl
            i32.const 1632
            i32.add
            local.tee 4
            i32.load
            i32.eq
            if  ;; label = @5
              local.get 4
              local.get 2
              i32.store
              local.get 2
              br_if 1 (;@4;)
              i32.const 1332
              i32.const 1332
              i32.load
              i32.const -2
              local.get 3
              i32.rotl
              i32.and
              i32.store
              br 3 (;@2;)
            end
            local.get 6
            i32.const 16
            i32.const 20
            local.get 6
            i32.load offset=16
            local.get 0
            i32.eq
            select
            i32.add
            local.get 2
            i32.store
            local.get 2
            i32.eqz
            br_if 2 (;@2;)
          end
          local.get 2
          local.get 6
          i32.store offset=24
          local.get 0
          i32.load offset=16
          local.tee 3
          if  ;; label = @4
            local.get 2
            local.get 3
            i32.store offset=16
            local.get 3
            local.get 2
            i32.store offset=24
          end
          local.get 0
          i32.load offset=20
          local.tee 3
          i32.eqz
          br_if 1 (;@2;)
          local.get 2
          i32.const 20
          i32.add
          local.get 3
          i32.store
          local.get 3
          local.get 2
          i32.store offset=24
          br 1 (;@2;)
        end
        local.get 5
        i32.load offset=4
        local.tee 2
        i32.const 3
        i32.and
        i32.const 3
        i32.ne
        br_if 0 (;@2;)
        local.get 5
        local.get 2
        i32.const -2
        i32.and
        i32.store offset=4
        i32.const 1336
        local.get 1
        i32.store
        local.get 5
        local.get 1
        i32.store
        local.get 0
        local.get 1
        i32.const 1
        i32.or
        i32.store offset=4
        return
      end
      block  ;; label = @2
        local.get 5
        i32.load offset=4
        local.tee 2
        i32.const 2
        i32.and
        i32.eqz
        if  ;; label = @3
          local.get 5
          i32.const 1352
          i32.load
          i32.eq
          if  ;; label = @4
            i32.const 1352
            local.get 0
            i32.store
            i32.const 1340
            i32.const 1340
            i32.load
            local.get 1
            i32.add
            local.tee 1
            i32.store
            local.get 0
            local.get 1
            i32.const 1
            i32.or
            i32.store offset=4
            local.get 0
            i32.const 1348
            i32.load
            i32.ne
            br_if 3 (;@1;)
            i32.const 1336
            i32.const 0
            i32.store
            i32.const 1348
            i32.const 0
            i32.store
            return
          end
          local.get 5
          i32.const 1348
          i32.load
          i32.eq
          if  ;; label = @4
            i32.const 1348
            local.get 0
            i32.store
            i32.const 1336
            i32.const 1336
            i32.load
            local.get 1
            i32.add
            local.tee 1
            i32.store
            local.get 0
            local.get 1
            i32.const 1
            i32.or
            i32.store offset=4
            local.get 0
            local.get 1
            i32.add
            local.get 1
            i32.store
            return
          end
          i32.const 1344
          i32.load
          local.set 3
          local.get 2
          i32.const -8
          i32.and
          local.get 1
          i32.add
          local.set 1
          block  ;; label = @4
            local.get 2
            i32.const 255
            i32.le_u
            if  ;; label = @5
              local.get 5
              i32.load offset=8
              local.tee 4
              local.get 2
              i32.const 3
              i32.shr_u
              local.tee 2
              i32.const 3
              i32.shl
              i32.const 1368
              i32.add
              i32.ne
              drop
              local.get 4
              local.get 5
              i32.load offset=12
              local.tee 3
              i32.eq
              if  ;; label = @6
                i32.const 1328
                i32.const 1328
                i32.load
                i32.const -2
                local.get 2
                i32.rotl
                i32.and
                i32.store
                br 2 (;@4;)
              end
              local.get 3
              local.get 4
              i32.store offset=8
              local.get 4
              local.get 3
              i32.store offset=12
              br 1 (;@4;)
            end
            local.get 5
            i32.load offset=24
            local.set 6
            block  ;; label = @5
              local.get 5
              local.get 5
              i32.load offset=12
              local.tee 2
              i32.ne
              if  ;; label = @6
                local.get 3
                local.get 5
                i32.load offset=8
                local.tee 3
                i32.le_u
                if  ;; label = @7
                  local.get 3
                  i32.load offset=12
                  drop
                end
                local.get 2
                local.get 3
                i32.store offset=8
                local.get 3
                local.get 2
                i32.store offset=12
                br 1 (;@5;)
              end
              block  ;; label = @6
                local.get 5
                i32.const 20
                i32.add
                local.tee 3
                i32.load
                local.tee 4
                br_if 0 (;@6;)
                local.get 5
                i32.const 16
                i32.add
                local.tee 3
                i32.load
                local.tee 4
                br_if 0 (;@6;)
                i32.const 0
                local.set 2
                br 1 (;@5;)
              end
              loop  ;; label = @6
                local.get 3
                local.set 7
                local.get 4
                local.tee 2
                i32.const 20
                i32.add
                local.tee 3
                i32.load
                local.tee 4
                br_if 0 (;@6;)
                local.get 2
                i32.const 16
                i32.add
                local.set 3
                local.get 2
                i32.load offset=16
                local.tee 4
                br_if 0 (;@6;)
              end
              local.get 7
              i32.const 0
              i32.store
            end
            local.get 6
            i32.eqz
            br_if 0 (;@4;)
            block  ;; label = @5
              local.get 5
              local.get 5
              i32.load offset=28
              local.tee 3
              i32.const 2
              i32.shl
              i32.const 1632
              i32.add
              local.tee 4
              i32.load
              i32.eq
              if  ;; label = @6
                local.get 4
                local.get 2
                i32.store
                local.get 2
                br_if 1 (;@5;)
                i32.const 1332
                i32.const 1332
                i32.load
                i32.const -2
                local.get 3
                i32.rotl
                i32.and
                i32.store
                br 2 (;@4;)
              end
              local.get 6
              i32.const 16
              i32.const 20
              local.get 6
              i32.load offset=16
              local.get 5
              i32.eq
              select
              i32.add
              local.get 2
              i32.store
              local.get 2
              i32.eqz
              br_if 1 (;@4;)
            end
            local.get 2
            local.get 6
            i32.store offset=24
            local.get 5
            i32.load offset=16
            local.tee 3
            if  ;; label = @5
              local.get 2
              local.get 3
              i32.store offset=16
              local.get 3
              local.get 2
              i32.store offset=24
            end
            local.get 5
            i32.load offset=20
            local.tee 3
            i32.eqz
            br_if 0 (;@4;)
            local.get 2
            i32.const 20
            i32.add
            local.get 3
            i32.store
            local.get 3
            local.get 2
            i32.store offset=24
          end
          local.get 0
          local.get 1
          i32.add
          local.get 1
          i32.store
          local.get 0
          local.get 1
          i32.const 1
          i32.or
          i32.store offset=4
          local.get 0
          i32.const 1348
          i32.load
          i32.ne
          br_if 1 (;@2;)
          i32.const 1336
          local.get 1
          i32.store
          return
        end
        local.get 5
        local.get 2
        i32.const -2
        i32.and
        i32.store offset=4
        local.get 0
        local.get 1
        i32.add
        local.get 1
        i32.store
        local.get 0
        local.get 1
        i32.const 1
        i32.or
        i32.store offset=4
      end
      local.get 1
      i32.const 255
      i32.le_u
      if  ;; label = @2
        local.get 1
        i32.const 3
        i32.shr_u
        local.tee 2
        i32.const 3
        i32.shl
        i32.const 1368
        i32.add
        local.set 1
        block (result i32)  ;; label = @3
          i32.const 1328
          i32.load
          local.tee 3
          i32.const 1
          local.get 2
          i32.shl
          local.tee 2
          i32.and
          i32.eqz
          if  ;; label = @4
            i32.const 1328
            local.get 2
            local.get 3
            i32.or
            i32.store
            local.get 1
            br 1 (;@3;)
          end
          local.get 1
          i32.load offset=8
        end
        local.tee 3
        local.get 0
        i32.store offset=12
        local.get 1
        local.get 0
        i32.store offset=8
        local.get 0
        local.get 1
        i32.store offset=12
        local.get 0
        local.get 3
        i32.store offset=8
        return
      end
      local.get 0
      i64.const 0
      i64.store offset=16 align=4
      local.get 0
      i32.const 28
      i32.add
      block (result i32)  ;; label = @2
        i32.const 0
        local.get 1
        i32.const 8
        i32.shr_u
        local.tee 2
        i32.eqz
        br_if 0 (;@2;)
        drop
        i32.const 31
        local.get 1
        i32.const 16777215
        i32.gt_u
        br_if 0 (;@2;)
        drop
        local.get 2
        local.get 2
        i32.const 1048320
        i32.add
        i32.const 16
        i32.shr_u
        i32.const 8
        i32.and
        local.tee 2
        i32.shl
        local.tee 3
        local.get 3
        i32.const 520192
        i32.add
        i32.const 16
        i32.shr_u
        i32.const 4
        i32.and
        local.tee 3
        i32.shl
        local.tee 4
        local.get 4
        i32.const 245760
        i32.add
        i32.const 16
        i32.shr_u
        i32.const 2
        i32.and
        local.tee 4
        i32.shl
        i32.const 15
        i32.shr_u
        local.get 2
        local.get 3
        i32.or
        local.get 4
        i32.or
        i32.sub
        local.tee 2
        i32.const 1
        i32.shl
        local.get 1
        local.get 2
        i32.const 21
        i32.add
        i32.shr_u
        i32.const 1
        i32.and
        i32.or
        i32.const 28
        i32.add
      end
      local.tee 3
      i32.store
      local.get 3
      i32.const 2
      i32.shl
      i32.const 1632
      i32.add
      local.set 2
      i32.const 1332
      i32.load
      local.tee 4
      i32.const 1
      local.get 3
      i32.shl
      local.tee 7
      i32.and
      i32.eqz
      if  ;; label = @2
        local.get 2
        local.get 0
        i32.store
        i32.const 1332
        local.get 4
        local.get 7
        i32.or
        i32.store
        local.get 0
        i32.const 24
        i32.add
        local.get 2
        i32.store
        local.get 0
        local.get 0
        i32.store offset=8
        local.get 0
        local.get 0
        i32.store offset=12
        return
      end
      local.get 1
      i32.const 0
      i32.const 25
      local.get 3
      i32.const 1
      i32.shr_u
      i32.sub
      local.get 3
      i32.const 31
      i32.eq
      select
      i32.shl
      local.set 3
      local.get 2
      i32.load
      local.set 2
      block  ;; label = @2
        loop  ;; label = @3
          local.get 2
          local.tee 4
          i32.load offset=4
          i32.const -8
          i32.and
          local.get 1
          i32.eq
          br_if 1 (;@2;)
          local.get 3
          i32.const 29
          i32.shr_u
          local.set 2
          local.get 3
          i32.const 1
          i32.shl
          local.set 3
          local.get 4
          local.get 2
          i32.const 4
          i32.and
          i32.add
          i32.const 16
          i32.add
          local.tee 7
          i32.load
          local.tee 2
          br_if 0 (;@3;)
        end
        local.get 7
        local.get 0
        i32.store
        local.get 0
        i32.const 24
        i32.add
        local.get 4
        i32.store
        local.get 0
        local.get 0
        i32.store offset=12
        local.get 0
        local.get 0
        i32.store offset=8
        return
      end
      local.get 4
      i32.load offset=8
      local.set 1
      local.get 4
      local.get 0
      i32.store offset=8
      local.get 1
      local.get 0
      i32.store offset=12
      local.get 0
      i32.const 24
      i32.add
      i32.const 0
      i32.store
      local.get 0
      local.get 1
      i32.store offset=8
      local.get 0
      local.get 4
      i32.store offset=12
    end)
  (func (;11;) (type 3) (result i32)
    (local i32 i32 i32 i32 i32 i32)
    call 8
    local.tee 0
    i32.eqz
    if  ;; label = @1
      i32.const 0
      return
    end
    local.get 0
    i32.const -8
    i32.add
    local.set 1
    block  ;; label = @1
      local.get 0
      i32.const 4095
      i32.and
      i32.eqz
      if  ;; label = @2
        local.get 1
        local.set 0
        br 1 (;@1;)
      end
      local.get 0
      i32.const -4
      i32.add
      local.tee 3
      i32.load
      local.tee 2
      i32.const -8
      i32.and
      local.get 0
      i32.const 4095
      i32.add
      i32.const -4096
      i32.and
      i32.const -8
      i32.add
      local.tee 0
      local.get 0
      i32.const 4096
      i32.add
      local.get 0
      local.get 1
      i32.sub
      i32.const 15
      i32.gt_u
      select
      local.tee 0
      local.get 1
      i32.sub
      local.tee 4
      i32.sub
      local.set 5
      local.get 2
      i32.const 3
      i32.and
      i32.eqz
      if  ;; label = @2
        local.get 0
        local.get 5
        i32.store offset=4
        local.get 0
        local.get 1
        i32.load
        local.get 4
        i32.add
        i32.store
        br 1 (;@1;)
      end
      local.get 0
      local.get 5
      local.get 0
      i32.load offset=4
      i32.const 1
      i32.and
      i32.or
      i32.const 2
      i32.or
      i32.store offset=4
      local.get 0
      local.get 5
      i32.add
      local.tee 2
      local.get 2
      i32.load offset=4
      i32.const 1
      i32.or
      i32.store offset=4
      local.get 3
      local.get 4
      local.get 3
      i32.load
      i32.const 1
      i32.and
      i32.or
      i32.const 2
      i32.or
      i32.store
      local.get 0
      local.get 0
      i32.load offset=4
      i32.const 1
      i32.or
      i32.store offset=4
      local.get 1
      local.get 4
      call 10
    end
    block  ;; label = @1
      local.get 0
      i32.load offset=4
      local.tee 2
      i32.const 3
      i32.and
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      i32.const -8
      i32.and
      local.tee 1
      i32.const 1000032
      i32.le_u
      br_if 0 (;@1;)
      local.get 0
      local.get 2
      i32.const 1
      i32.and
      i32.const 1000018
      i32.or
      i32.store offset=4
      local.get 0
      i32.const 1000016
      i32.add
      local.tee 3
      local.get 1
      i32.const 1000016
      i32.sub
      local.tee 2
      i32.const 3
      i32.or
      i32.store offset=4
      local.get 0
      local.get 1
      i32.add
      local.tee 1
      local.get 1
      i32.load offset=4
      i32.const 1
      i32.or
      i32.store offset=4
      local.get 3
      local.get 2
      call 10
    end
    local.get 0
    i32.const 8
    i32.add)
  (func (;12;) (type 2) (param i32) (result i32)
    local.get 0
    i32.eqz
    if  ;; label = @1
      memory.size
      i32.const 16
      i32.shl
      return
    end
    local.get 0
    i32.const 65535
    i32.and
    local.get 0
    i32.const -1
    i32.le_s
    i32.or
    i32.eqz
    if  ;; label = @1
      local.get 0
      i32.const 16
      i32.shr_u
      memory.grow
      local.tee 0
      i32.const -1
      i32.eq
      if  ;; label = @2
        i32.const 1824
        i32.const 48
        i32.store
        i32.const -1
        return
      end
      local.get 0
      i32.const 16
      i32.shl
      return
    end
    unreachable)
  (func (;13;) (type 4) (param i32)
    (local i32 i32 i32 i32 i32 i32)
    i32.const 1024
    local.set 3
    block  ;; label = @1
      local.get 0
      i32.load offset=16
      local.tee 1
      if (result i32)  ;; label = @2
        local.get 1
      else
        i32.const 0
        local.set 1
        local.get 0
        call 22
        br_if 1 (;@1;)
        local.get 0
        i32.load offset=16
      end
      local.get 0
      i32.load offset=20
      local.tee 4
      i32.sub
      i32.const 50
      i32.lt_u
      if  ;; label = @2
        local.get 0
        i32.const 1024
        i32.const 50
        local.get 0
        i32.load offset=32
        call_indirect (type 0)
        local.set 1
        br 1 (;@1;)
      end
      block (result i32)  ;; label = @2
        i32.const 50
        local.get 0
        i32.load offset=64
        i32.const 0
        i32.lt_s
        br_if 0 (;@2;)
        drop
        i32.const 0
        local.set 1
        loop  ;; label = @3
          i32.const 50
          local.get 1
          i32.const 50
          i32.add
          i32.eqz
          br_if 1 (;@2;)
          drop
          local.get 1
          i32.const 1074
          i32.add
          local.get 1
          i32.const -1
          i32.add
          local.tee 2
          local.set 1
          i32.const -1
          i32.add
          i32.load8_u
          i32.const 10
          i32.ne
          br_if 0 (;@3;)
        end
        local.get 0
        i32.const 1024
        local.get 2
        i32.const 51
        i32.add
        local.tee 5
        local.get 0
        i32.load offset=32
        call_indirect (type 0)
        local.tee 1
        local.get 5
        i32.lt_u
        br_if 1 (;@1;)
        local.get 2
        i32.const 1075
        i32.add
        local.set 3
        local.get 0
        i32.load offset=20
        local.set 4
        local.get 2
        i32.const -1
        i32.xor
      end
      local.set 1
      local.get 4
      local.get 3
      local.get 1
      call 23
      local.get 0
      local.get 0
      i32.load offset=20
      local.get 1
      i32.add
      i32.store offset=20
      local.get 1
      local.get 5
      i32.add
      local.set 1
    end
    local.get 1
    i32.const 50
    i32.eq
    if  ;; label = @1
      return
    end
    local.get 1
    i32.const 50
    i32.div_u
    drop)
  (func (;14;) (type 2) (param i32) (result i32)
    block (result i32)  ;; label = @1
      i32.const 0
      local.get 0
      i32.load offset=56
      call 1
      local.tee 0
      i32.eqz
      br_if 0 (;@1;)
      drop
      i32.const 1824
      local.get 0
      i32.store
      i32.const -1
    end)
  (func (;15;) (type 1) (param i32 i64 i32) (result i64)
    (local i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 3
    global.set 0
    block (result i64)  ;; label = @1
      local.get 0
      local.get 1
      local.get 2
      i32.const 255
      i32.and
      local.get 3
      i32.const 8
      i32.add
      call 2
      local.tee 0
      if  ;; label = @2
        i32.const 1824
        i32.const 70
        local.get 0
        local.get 0
        i32.const 76
        i32.eq
        select
        i32.store
        i64.const -1
        br 1 (;@1;)
      end
      local.get 3
      i64.load offset=8
    end
    local.get 3
    i32.const 16
    i32.add
    global.set 0)
  (func (;16;) (type 1) (param i32 i64 i32) (result i64)
    local.get 0
    i32.load offset=56
    local.get 1
    local.get 2
    call 15)
  (func (;17;) (type 0) (param i32 i32 i32) (result i32)
    (local i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 3
    global.set 0
    i32.const -1
    local.set 4
    block  ;; label = @1
      local.get 2
      i32.const -1
      i32.le_s
      if  ;; label = @2
        i32.const 1824
        i32.const 28
        i32.store
        br 1 (;@1;)
      end
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      i32.const 12
      i32.add
      call 3
      local.tee 0
      if  ;; label = @2
        i32.const 1824
        local.get 0
        i32.store
        br 1 (;@1;)
      end
      local.get 3
      i32.load offset=12
      local.set 4
    end
    local.get 3
    i32.const 16
    i32.add
    global.set 0
    local.get 4)
  (func (;18;) (type 0) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 3
    global.set 0
    local.get 3
    local.get 2
    i32.store offset=12
    local.get 3
    local.get 1
    i32.store offset=8
    local.get 3
    local.get 0
    i32.load offset=24
    local.tee 1
    i32.store
    local.get 3
    local.get 0
    i32.load offset=20
    local.get 1
    i32.sub
    local.tee 1
    i32.store offset=4
    i32.const 2
    local.set 6
    block (result i32)  ;; label = @1
      local.get 1
      local.get 2
      i32.add
      local.tee 7
      local.get 0
      i32.load offset=56
      local.get 3
      i32.const 2
      call 17
      local.tee 4
      i32.ne
      if  ;; label = @2
        local.get 3
        local.set 1
        loop  ;; label = @3
          local.get 4
          i32.const -1
          i32.le_s
          if  ;; label = @4
            local.get 0
            i32.const 0
            i32.store offset=24
            local.get 0
            i64.const 0
            i64.store offset=16
            local.get 0
            local.get 0
            i32.load
            i32.const 32
            i32.or
            i32.store
            i32.const 0
            local.get 6
            i32.const 2
            i32.eq
            br_if 3 (;@1;)
            drop
            local.get 2
            local.get 1
            i32.load offset=4
            i32.sub
            br 3 (;@1;)
          end
          local.get 1
          i32.const 8
          i32.add
          local.get 1
          local.get 4
          local.get 1
          i32.load offset=4
          local.tee 8
          i32.gt_u
          local.tee 5
          select
          local.tee 1
          local.get 4
          local.get 8
          i32.const 0
          local.get 5
          select
          i32.sub
          local.tee 8
          local.get 1
          i32.load
          i32.add
          i32.store
          local.get 1
          local.get 1
          i32.load offset=4
          local.get 8
          i32.sub
          i32.store offset=4
          local.get 7
          local.get 4
          i32.sub
          local.set 7
          local.get 0
          i32.load offset=56
          local.get 1
          local.get 6
          local.get 5
          i32.sub
          local.tee 6
          call 17
          local.tee 5
          local.set 4
          local.get 5
          local.get 7
          i32.ne
          br_if 0 (;@3;)
        end
      end
      local.get 0
      local.get 0
      i32.load offset=40
      local.tee 1
      i32.store offset=24
      local.get 0
      local.get 1
      i32.store offset=20
      local.get 0
      local.get 1
      local.get 0
      i32.load offset=44
      i32.add
      i32.store offset=16
      local.get 2
    end
    local.get 3
    i32.const 16
    i32.add
    global.set 0)
  (func (;19;) (type 2) (param i32) (result i32)
    (local i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 1
    global.set 0
    block (result i32)  ;; label = @1
      block  ;; label = @2
        local.get 0
        local.get 1
        i32.const 8
        i32.add
        call 4
        local.tee 0
        br_if 0 (;@2;)
        i32.const 59
        local.set 0
        local.get 1
        i32.load8_u offset=8
        i32.const 2
        i32.ne
        br_if 0 (;@2;)
        local.get 1
        i32.load8_u offset=16
        i32.const 36
        i32.and
        br_if 0 (;@2;)
        i32.const 1
        br 1 (;@1;)
      end
      i32.const 1824
      local.get 0
      i32.store
      i32.const 0
    end
    local.get 1
    i32.const 32
    i32.add
    global.set 0)
  (func (;20;) (type 0) (param i32 i32 i32) (result i32)
    local.get 0
    i32.const 2
    i32.store offset=32
    block  ;; label = @1
      local.get 0
      i32.load8_u
      i32.const 64
      i32.and
      br_if 0 (;@1;)
      local.get 0
      i32.load offset=56
      call 19
      br_if 0 (;@1;)
      local.get 0
      i32.const -1
      i32.store offset=64
    end
    local.get 0
    local.get 1
    local.get 2
    call 18)
  (func (;21;) (type 5)
    (local i32 i32 i32)
    i32.const 1828
    i32.load
    local.tee 0
    if  ;; label = @1
      loop  ;; label = @2
        local.get 0
        i32.load offset=20
        local.get 0
        i32.load offset=24
        i32.ne
        if  ;; label = @3
          local.get 0
          i32.const 0
          i32.const 0
          local.get 0
          i32.load offset=32
          call_indirect (type 0)
          drop
        end
        local.get 0
        i32.load offset=4
        local.tee 1
        local.get 0
        i32.load offset=8
        local.tee 2
        i32.ne
        if  ;; label = @3
          local.get 0
          local.get 1
          local.get 2
          i32.sub
          i64.extend_i32_s
          i32.const 1
          local.get 0
          i32.load offset=36
          call_indirect (type 1)
          drop
        end
        local.get 0
        i32.load offset=52
        local.tee 0
        br_if 0 (;@2;)
      end
    end
    block  ;; label = @1
      i32.const 2872
      i32.load
      local.tee 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.load offset=20
      local.get 0
      i32.load offset=24
      i32.ne
      if  ;; label = @2
        local.get 0
        i32.const 0
        i32.const 0
        local.get 0
        i32.load offset=32
        call_indirect (type 0)
        drop
      end
      local.get 0
      i32.load offset=4
      local.tee 1
      local.get 0
      i32.load offset=8
      local.tee 2
      i32.eq
      br_if 0 (;@1;)
      local.get 0
      local.get 1
      local.get 2
      i32.sub
      i64.extend_i32_s
      i32.const 1
      local.get 0
      i32.load offset=36
      call_indirect (type 1)
      drop
    end
    block  ;; label = @1
      i32.const 1312
      i32.load
      local.tee 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.load offset=20
      local.get 0
      i32.load offset=24
      i32.ne
      if  ;; label = @2
        local.get 0
        i32.const 0
        i32.const 0
        local.get 0
        i32.load offset=32
        call_indirect (type 0)
        drop
      end
      local.get 0
      i32.load offset=4
      local.tee 1
      local.get 0
      i32.load offset=8
      local.tee 2
      i32.eq
      br_if 0 (;@1;)
      local.get 0
      local.get 1
      local.get 2
      i32.sub
      i64.extend_i32_s
      i32.const 1
      local.get 0
      i32.load offset=36
      call_indirect (type 1)
      drop
    end
    block  ;; label = @1
      i32.const 1192
      i32.load
      local.tee 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.load offset=20
      local.get 0
      i32.load offset=24
      i32.ne
      if  ;; label = @2
        local.get 0
        i32.const 0
        i32.const 0
        local.get 0
        i32.load offset=32
        call_indirect (type 0)
        drop
      end
      local.get 0
      i32.load offset=4
      local.tee 1
      local.get 0
      i32.load offset=8
      local.tee 2
      i32.eq
      br_if 0 (;@1;)
      local.get 0
      local.get 1
      local.get 2
      i32.sub
      i64.extend_i32_s
      i32.const 1
      local.get 0
      i32.load offset=36
      call_indirect (type 1)
      drop
    end)
  (func (;22;) (type 2) (param i32) (result i32)
    (local i32)
    local.get 0
    local.get 0
    i32.load offset=60
    local.tee 1
    i32.const -1
    i32.add
    local.get 1
    i32.or
    i32.store offset=60
    local.get 0
    i32.load
    local.tee 1
    i32.const 8
    i32.and
    if  ;; label = @1
      local.get 0
      local.get 1
      i32.const 32
      i32.or
      i32.store
      i32.const -1
      return
    end
    local.get 0
    i64.const 0
    i64.store offset=4 align=4
    local.get 0
    local.get 0
    i32.load offset=40
    local.tee 1
    i32.store offset=24
    local.get 0
    local.get 1
    i32.store offset=20
    local.get 0
    local.get 1
    local.get 0
    i32.load offset=44
    i32.add
    i32.store offset=16
    i32.const 0)
  (func (;23;) (type 7) (param i32 i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32)
    block  ;; label = @1
      local.get 2
      i32.eqz
      local.get 1
      i32.const 3
      i32.and
      i32.eqz
      i32.or
      i32.eqz
      if  ;; label = @2
        loop  ;; label = @3
          local.get 0
          local.get 1
          i32.load8_u
          i32.store8
          local.get 2
          i32.const -1
          i32.add
          local.set 3
          local.get 0
          i32.const 1
          i32.add
          local.set 0
          local.get 1
          i32.const 1
          i32.add
          local.set 1
          local.get 2
          i32.const 1
          i32.eq
          br_if 2 (;@1;)
          local.get 3
          local.set 2
          local.get 1
          i32.const 3
          i32.and
          br_if 0 (;@3;)
        end
        br 1 (;@1;)
      end
      local.get 2
      local.set 3
    end
    block  ;; label = @1
      local.get 0
      i32.const 3
      i32.and
      local.tee 2
      i32.eqz
      if  ;; label = @2
        block  ;; label = @3
          local.get 3
          i32.const 16
          i32.lt_u
          if  ;; label = @4
            local.get 3
            local.set 2
            br 1 (;@3;)
          end
          local.get 3
          i32.const -16
          i32.add
          local.set 2
          loop  ;; label = @4
            local.get 0
            local.get 1
            i32.load
            i32.store
            local.get 0
            i32.const 4
            i32.add
            local.get 1
            i32.const 4
            i32.add
            i32.load
            i32.store
            local.get 0
            i32.const 8
            i32.add
            local.get 1
            i32.const 8
            i32.add
            i32.load
            i32.store
            local.get 0
            i32.const 12
            i32.add
            local.get 1
            i32.const 12
            i32.add
            i32.load
            i32.store
            local.get 0
            i32.const 16
            i32.add
            local.set 0
            local.get 1
            i32.const 16
            i32.add
            local.set 1
            local.get 3
            i32.const -16
            i32.add
            local.tee 3
            i32.const 15
            i32.gt_u
            br_if 0 (;@4;)
          end
        end
        local.get 2
        i32.const 8
        i32.and
        if  ;; label = @3
          local.get 0
          local.get 1
          i64.load align=4
          i64.store align=4
          local.get 1
          i32.const 8
          i32.add
          local.set 1
          local.get 0
          i32.const 8
          i32.add
          local.set 0
        end
        local.get 2
        i32.const 4
        i32.and
        if  ;; label = @3
          local.get 0
          local.get 1
          i32.load
          i32.store
          local.get 1
          i32.const 4
          i32.add
          local.set 1
          local.get 0
          i32.const 4
          i32.add
          local.set 0
        end
        local.get 2
        i32.const 2
        i32.and
        if  ;; label = @3
          local.get 0
          local.get 1
          i32.load8_u
          i32.store8
          local.get 0
          local.get 1
          i32.load8_u offset=1
          i32.store8 offset=1
          local.get 1
          i32.const 2
          i32.add
          local.set 1
          local.get 0
          i32.const 2
          i32.add
          local.set 0
        end
        local.get 2
        i32.const 1
        i32.and
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        local.get 1
        i32.load8_u
        i32.store8
        return
      end
      block  ;; label = @2
        local.get 3
        i32.const 32
        i32.lt_u
        br_if 0 (;@2;)
        local.get 2
        i32.const -1
        i32.add
        local.tee 2
        i32.const 2
        i32.gt_u
        br_if 0 (;@2;)
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              local.get 2
              i32.const 1
              i32.sub
              br_table 1 (;@4;) 2 (;@3;) 0 (;@5;)
            end
            local.get 0
            local.get 1
            i32.load8_u offset=1
            i32.store8 offset=1
            local.get 0
            local.get 1
            i32.load
            local.tee 4
            i32.store8
            local.get 0
            local.get 1
            i32.load8_u offset=2
            i32.store8 offset=2
            local.get 3
            i32.const -3
            i32.add
            local.set 7
            local.get 0
            i32.const 3
            i32.add
            local.set 8
            local.get 3
            i32.const -20
            i32.add
            i32.const -16
            i32.and
            local.set 9
            i32.const 0
            local.set 2
            loop  ;; label = @5
              local.get 2
              local.get 8
              i32.add
              local.tee 0
              local.get 1
              local.get 2
              i32.add
              local.tee 5
              i32.const 4
              i32.add
              i32.load
              local.tee 6
              i32.const 8
              i32.shl
              local.get 4
              i32.const 24
              i32.shr_u
              i32.or
              i32.store
              local.get 0
              i32.const 4
              i32.add
              local.get 5
              i32.const 8
              i32.add
              i32.load
              local.tee 4
              i32.const 8
              i32.shl
              local.get 6
              i32.const 24
              i32.shr_u
              i32.or
              i32.store
              local.get 0
              i32.const 8
              i32.add
              local.get 5
              i32.const 12
              i32.add
              i32.load
              local.tee 6
              i32.const 8
              i32.shl
              local.get 4
              i32.const 24
              i32.shr_u
              i32.or
              i32.store
              local.get 0
              i32.const 12
              i32.add
              local.get 5
              i32.const 16
              i32.add
              i32.load
              local.tee 4
              i32.const 8
              i32.shl
              local.get 6
              i32.const 24
              i32.shr_u
              i32.or
              i32.store
              local.get 2
              i32.const 16
              i32.add
              local.set 2
              local.get 7
              i32.const -16
              i32.add
              local.tee 7
              i32.const 16
              i32.gt_u
              br_if 0 (;@5;)
            end
            local.get 2
            local.get 8
            i32.add
            local.set 0
            local.get 1
            local.get 2
            i32.add
            i32.const 3
            i32.add
            local.set 1
            local.get 3
            local.get 9
            i32.sub
            i32.const -19
            i32.add
            local.set 3
            br 2 (;@2;)
          end
          local.get 0
          local.get 1
          i32.load
          local.tee 4
          i32.store8
          local.get 0
          local.get 1
          i32.load8_u offset=1
          i32.store8 offset=1
          local.get 3
          i32.const -2
          i32.add
          local.set 7
          local.get 0
          i32.const 2
          i32.add
          local.set 8
          local.get 3
          i32.const -20
          i32.add
          i32.const -16
          i32.and
          local.set 9
          i32.const 0
          local.set 2
          loop  ;; label = @4
            local.get 2
            local.get 8
            i32.add
            local.tee 0
            local.get 1
            local.get 2
            i32.add
            local.tee 5
            i32.const 4
            i32.add
            i32.load
            local.tee 6
            i32.const 16
            i32.shl
            local.get 4
            i32.const 16
            i32.shr_u
            i32.or
            i32.store
            local.get 0
            i32.const 4
            i32.add
            local.get 5
            i32.const 8
            i32.add
            i32.load
            local.tee 4
            i32.const 16
            i32.shl
            local.get 6
            i32.const 16
            i32.shr_u
            i32.or
            i32.store
            local.get 0
            i32.const 8
            i32.add
            local.get 5
            i32.const 12
            i32.add
            i32.load
            local.tee 6
            i32.const 16
            i32.shl
            local.get 4
            i32.const 16
            i32.shr_u
            i32.or
            i32.store
            local.get 0
            i32.const 12
            i32.add
            local.get 5
            i32.const 16
            i32.add
            i32.load
            local.tee 4
            i32.const 16
            i32.shl
            local.get 6
            i32.const 16
            i32.shr_u
            i32.or
            i32.store
            local.get 2
            i32.const 16
            i32.add
            local.set 2
            local.get 7
            i32.const -16
            i32.add
            local.tee 7
            i32.const 17
            i32.gt_u
            br_if 0 (;@4;)
          end
          local.get 2
          local.get 8
          i32.add
          local.set 0
          local.get 1
          local.get 2
          i32.add
          i32.const 2
          i32.add
          local.set 1
          local.get 3
          local.get 9
          i32.sub
          i32.const -18
          i32.add
          local.set 3
          br 1 (;@2;)
        end
        local.get 0
        local.get 1
        i32.load
        local.tee 4
        i32.store8
        local.get 3
        i32.const -1
        i32.add
        local.set 7
        local.get 0
        i32.const 1
        i32.add
        local.set 8
        local.get 3
        i32.const -20
        i32.add
        i32.const -16
        i32.and
        local.set 9
        i32.const 0
        local.set 2
        loop  ;; label = @3
          local.get 2
          local.get 8
          i32.add
          local.tee 0
          local.get 1
          local.get 2
          i32.add
          local.tee 5
          i32.const 4
          i32.add
          i32.load
          local.tee 6
          i32.const 24
          i32.shl
          local.get 4
          i32.const 8
          i32.shr_u
          i32.or
          i32.store
          local.get 0
          i32.const 4
          i32.add
          local.get 5
          i32.const 8
          i32.add
          i32.load
          local.tee 4
          i32.const 24
          i32.shl
          local.get 6
          i32.const 8
          i32.shr_u
          i32.or
          i32.store
          local.get 0
          i32.const 8
          i32.add
          local.get 5
          i32.const 12
          i32.add
          i32.load
          local.tee 6
          i32.const 24
          i32.shl
          local.get 4
          i32.const 8
          i32.shr_u
          i32.or
          i32.store
          local.get 0
          i32.const 12
          i32.add
          local.get 5
          i32.const 16
          i32.add
          i32.load
          local.tee 4
          i32.const 24
          i32.shl
          local.get 6
          i32.const 8
          i32.shr_u
          i32.or
          i32.store
          local.get 2
          i32.const 16
          i32.add
          local.set 2
          local.get 7
          i32.const -16
          i32.add
          local.tee 7
          i32.const 18
          i32.gt_u
          br_if 0 (;@3;)
        end
        local.get 2
        local.get 8
        i32.add
        local.set 0
        local.get 1
        local.get 2
        i32.add
        i32.const 1
        i32.add
        local.set 1
        local.get 3
        local.get 9
        i32.sub
        i32.const -17
        i32.add
        local.set 3
      end
      local.get 3
      i32.const 16
      i32.and
      if  ;; label = @2
        local.get 0
        local.get 1
        i32.load16_u align=1
        i32.store16 align=1
        local.get 0
        local.get 1
        i32.load8_u offset=2
        i32.store8 offset=2
        local.get 0
        local.get 1
        i32.load8_u offset=3
        i32.store8 offset=3
        local.get 0
        local.get 1
        i32.load8_u offset=4
        i32.store8 offset=4
        local.get 0
        local.get 1
        i32.load8_u offset=5
        i32.store8 offset=5
        local.get 0
        local.get 1
        i32.load8_u offset=6
        i32.store8 offset=6
        local.get 0
        local.get 1
        i32.load8_u offset=7
        i32.store8 offset=7
        local.get 0
        local.get 1
        i32.load8_u offset=8
        i32.store8 offset=8
        local.get 0
        local.get 1
        i32.load8_u offset=9
        i32.store8 offset=9
        local.get 0
        local.get 1
        i32.load8_u offset=10
        i32.store8 offset=10
        local.get 0
        local.get 1
        i32.load8_u offset=11
        i32.store8 offset=11
        local.get 0
        local.get 1
        i32.load8_u offset=12
        i32.store8 offset=12
        local.get 0
        local.get 1
        i32.load8_u offset=13
        i32.store8 offset=13
        local.get 0
        local.get 1
        i32.load8_u offset=14
        i32.store8 offset=14
        local.get 0
        local.get 1
        i32.load8_u offset=15
        i32.store8 offset=15
        local.get 1
        i32.const 16
        i32.add
        local.set 1
        local.get 0
        i32.const 16
        i32.add
        local.set 0
      end
      local.get 3
      i32.const 8
      i32.and
      if  ;; label = @2
        local.get 0
        local.get 1
        i32.load8_u
        i32.store8
        local.get 0
        local.get 1
        i32.load8_u offset=1
        i32.store8 offset=1
        local.get 0
        local.get 1
        i32.load8_u offset=2
        i32.store8 offset=2
        local.get 0
        local.get 1
        i32.load8_u offset=3
        i32.store8 offset=3
        local.get 0
        local.get 1
        i32.load8_u offset=4
        i32.store8 offset=4
        local.get 0
        local.get 1
        i32.load8_u offset=5
        i32.store8 offset=5
        local.get 0
        local.get 1
        i32.load8_u offset=6
        i32.store8 offset=6
        local.get 0
        local.get 1
        i32.load8_u offset=7
        i32.store8 offset=7
        local.get 1
        i32.const 8
        i32.add
        local.set 1
        local.get 0
        i32.const 8
        i32.add
        local.set 0
      end
      local.get 3
      i32.const 4
      i32.and
      if  ;; label = @2
        local.get 0
        local.get 1
        i32.load8_u
        i32.store8
        local.get 0
        local.get 1
        i32.load8_u offset=1
        i32.store8 offset=1
        local.get 0
        local.get 1
        i32.load8_u offset=2
        i32.store8 offset=2
        local.get 0
        local.get 1
        i32.load8_u offset=3
        i32.store8 offset=3
        local.get 1
        i32.const 4
        i32.add
        local.set 1
        local.get 0
        i32.const 4
        i32.add
        local.set 0
      end
      local.get 3
      i32.const 2
      i32.and
      if  ;; label = @2
        local.get 0
        local.get 1
        i32.load8_u
        i32.store8
        local.get 0
        local.get 1
        i32.load8_u offset=1
        i32.store8 offset=1
        local.get 1
        i32.const 2
        i32.add
        local.set 1
        local.get 0
        i32.const 2
        i32.add
        local.set 0
      end
      local.get 3
      i32.const 1
      i32.and
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      local.get 1
      i32.load8_u
      i32.store8
    end)
  (table (;0;) 5 5 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 68416))
  (export "memory" (memory 0))
  (export "_start" (func 5))
  (elem (;0;) (i32.const 1) 14 18 16 20)
  (data (;0;) (i32.const 1024) "[PolyBench] posix_memalign: cannot allocate memory\00\008\04")
  (data (;1;) (i32.const 1080) "\05")
  (data (;2;) (i32.const 1092) "\01")
  (data (;3;) (i32.const 1112) "\02\00\00\00\03\00\00\000\07")
  (data (;4;) (i32.const 1136) "\02\00\00\00\00\00\00\00\ff\ff\ff\ff")
  (data (;5;) (i32.const 1192) "8\04\00\00\00\00\00\00\05")
  (data (;6;) (i32.const 1212) "\01")
  (data (;7;) (i32.const 1232) "\04\00\00\00\03\00\00\008\07\00\00\00\04")
  (data (;8;) (i32.const 1256) "\01\00\00\00\00\00\00\00\0a")
  (data (;9;) (i32.const 1312) "\b0\04"))
