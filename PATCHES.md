# Wassail Modifications for Proteus Evaluation

## Build Status

✅ **Successfully built from source**

## Installation

```bash
opam install ./evaluation/tools/wassail --deps-only -y
opam install core_unix -y
cd evaluation/tools/wassail
dune build @install
```

## Modifications

None - using upstream version as-is.

## Usage for Proteus Evaluation

Wassail provides taint analysis to detect unsafe data flows in WASM binaries.

```bash
dune exec -- wassail taint-from-sources-to-sinks <file.wasm> <sources> <sinks>
```

**Sources:** Function parameter indices (e.g., "0,1,2")
**Sinks:** Dangerous operations (e.g., "div,rem,load,store")

### Example

```bash
dune exec -- wassail taint-from-sources-to-sinks program.wasm "0,1" "div,store"
```

This checks if data from parameters 0 or 1 flows to division or memory store operations.

### Integration with Proteus

- **Verdict mapping:** Taint flows found → VULNERABLE, no flows → SAFE
- **Confidence:** 0.70 for vulnerable, 0.65 for safe
- **Metadata:** Extracts analyzed functions and taint flow information
- **Use case:** Complements symbolic execution with static taint tracking

## Tool Characteristics

- **Type:** Static analysis (taint tracking)
- **Language:** OCaml (same as Proteus!)
- **Analysis:** Interprocedural taint analysis
- **Output:** Text-based analysis results

## Future Contributions

- [ ] Add JSON output format for easier parsing
- [ ] Improve source/sink specification for security analysis
- [ ] Add vulnerability pattern detection
