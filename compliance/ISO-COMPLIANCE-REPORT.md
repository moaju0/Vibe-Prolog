# ISO Prolog Compliance Report

## Summary

This report documents the work done to support ISO Prolog conformity testing and identifies gaps in ISO compliance.

## Implemented Features

### New Built-in Predicates

The following predicates were implemented to support ISO conformity testing:

1. **`read_from_chars/2`** - Parse Prolog terms from character lists or strings
   - Location: `vibeprolog/builtins/io.py`
   - Status: ✅ Fully functional
   - Usage: `read_from_chars("f(a, b)", Term)` parses the string and unifies `Term` with `f(a, b)`

2. **`write_term_to_chars/3`** - Convert Prolog terms to character lists with formatting options
   - Location: `vibeprolog/builtins/io.py`
   - Status: Partial. Uses the operator table for precedence and associativity; some spacing/parentheses edge cases may remain.
   - Supported options:
     - `ignore_ops(true/false)` - Write in canonical form
     - `numbervars(true/false)` - Convert `$VAR(N)` to variable names (A, B, C, ...)
     - `quoted(true/false)` - Quote atoms that need quoting
   - Usage: `write_term_to_chars([1,2,3], [quoted(false)], Chars)` produces `Chars = ['[','1',',','2',',','3',']']`

3. **`setup_call_cleanup/3`** - Execute goals with guaranteed cleanup
   - Location: `vibeprolog/builtins/control.py`
   - Status: ✅ Fully functional
   - Semantics: Calls Setup once, then Goal (possibly backtracking), and always calls Cleanup after Goal completes
   - Usage: `setup_call_cleanup(open(file), process(file), close(file))`

4. **`call_cleanup/2`** - Execute goal with guaranteed cleanup
   - Location: `vibeprolog/builtins/control.py`
   - Status: ✅ Fully functional
   - Semantics: Calls Goal (possibly backtracking) and always calls Cleanup after Goal completes
   - Usage: `call_cleanup(Goal, cleanup_action)`

### Test Files Created

1. **`compliance/iso-conformity-tests-standalone.pl`** - Module-free version of full ISO conformity tests
   - Removed module directives (`module/2`, `use_module/1`)
   - Rewritten test runner to avoid DCG syntax (which requires `phrase/2` from modules)
   - Many tests commented out due to parser limitations (see below)

2. **`compliance/iso-tests-simple.pl`** - Simplified test suite
   - Tests core functionality that works with current parser
   - **Result: All 9 tests PASSED** ✅
   - Tests cover: read_from_chars/2, write_term_to_chars/3, setup_call_cleanup/3, call_cleanup/2

## Current Limitations

### Parser Limitations

The following syntax features are not supported by the parser, preventing many ISO tests from running:

1. **Hex escape sequences in atoms**: `'\\x41\\'` (character code in hex)
2. **Parenthesized operators**: `(-)` (minus operator as a term)
3. **Character code arithmetic**: `0'\\x41\\` (hex character codes)
4. **Dynamic operator parsing**: `op/3` updates the operator table and `current_op/3` reports it, but the parser still uses only the built-in operator set (custom operators require canonical functor syntax)
5. **Complex operator expressions**: Some edge cases with operator precedence

These limitations mean the full ISO conformity test suite (276 tests) cannot be loaded into the interpreter, as the test file itself won't parse.

### Missing ISO Predicates

The following predicates are used extensively by ISO conformity tests but are not yet implemented:

#### Critical (used in 40+ tests each)
- None (operator predicates implemented; parser still limited to built-in operators)

#### Important (used in 10+ tests each)
- `char_code/2` - Character/code conversion
- `sub_atom/5` - Substring matching and extraction

#### Useful (used in a few tests)
- `set_prolog_flag/2` - Set Prolog flags
- `atom_chars/2` - Atom to character list conversion
- `read_term/3` - Read term from stream with options
- `get_char/2` - Read character from stream
- `open/3`, `close/1`, `delete_file/1` - File I/O (used in 2 tests)

### Write Term Formatting Limitations

The current `write_term_to_chars/3` implementation has these limitations:

1. **Spacing and parentheses edge cases**: Rendering now respects operator precedence/associativity, but may still over- or under-parenthesize when operators share precedence or use uncommon specs.
2. **Option coverage**: `variable_names/1` and some ISO formatting options are not supported; quoting/spacing rules are simplified.

## Recommendations

### Short Term (for basic ISO compatibility)

1. **Implement `atom_chars/2`** - Needed by test runner to filter test predicates
2. **Integrate dynamic operators into parsing** - Allow `op/3` updates to influence parser precedence
3. **Implement `char_code/2`** - Character conversion is fundamental
4. **Implement `sub_atom/5`** - String manipulation is widely used

### Medium Term (for better test coverage)

1. **Fix parser to support hex escapes**: `'\\x41\\'` syntax
2. **Fix parser to support parenthesized operators**: `(-)` as a term
3. **Improve `write_term_to_chars/3`** spacing/quoting to align with ISO output
4. **Enhance `write_term_to_chars/3`** with proper operator handling

### Long Term (for full ISO compliance)

1. **Implement file I/O**: `open/3`, `close/1`, `read_term/3`, `get_char/2`
2. **Implement Prolog flags**: `set_prolog_flag/2`, `current_prolog_flag/2`
3. **Fix all parser edge cases** identified by ISO conformity tests
4. **Implement structured error terms**: `error(ErrorType, Context)` format

## Testing Strategy

### Current Approach

Use `compliance/iso-tests-simple.pl` to verify basic functionality:
```bash
uv run vibeprolog.py compliance/iso-tests-simple.pl -q "run_all_tests_manual"
```

### Future Approach

Once the critical predicates and parser fixes are in place:
1. Run full ISO conformity test suite: `compliance/iso-conformity-tests-standalone.pl`
2. Track pass/fail rate to measure ISO compliance progress
3. Use failed tests to prioritize implementation work

## Conclusion

The interpreter now has essential I/O and control predicates for ISO compatibility. The main barriers to running the full ISO conformity test suite are:

1. **Parser limitations** (hex escapes, parenthesized operators, dynamic operator parsing)
2. **Missing string predicates** (atom_chars/2, sub_atom/5, char_code/2)
3. **Output formatting gaps** (`write_term_to_chars/3` spacing/option coverage)

With these addressed, the interpreter would be able to load and run the full 276-test ISO conformity suite.
