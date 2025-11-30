# Dynamic Operator Parsing - Future Implementation Guide

## Overview

The operator table and `op/3` directive are fully implemented and tested. However, **custom operator syntax parsing is not yet implemented**. This document outlines the work needed to complete this feature.

## Current Limitation

Currently, custom operators must be used in canonical (prefix) notation:

```prolog
% What works now:
:- op(500, xfx, loves).
fact(loves(alice, bob)).

% What doesn't work yet:
:- op(500, xfx, loves).
fact(alice loves bob).  % Parser error!
```

## Why This Matters

1. **ISO Compliance**: ISO Prolog expects `op/3` to affect parsing
2. **Meta-programming**: Prolog programs can't define their own syntax
3. **Compatibility**: Many existing Prolog programs use custom operators
4. **DSLs**: Can't create domain-specific languages in Prolog

## Implementation Approach: Two-Pass Parsing

### Strategy Overview

```
Input Source Code
       ↓
[Pass 1: Extract op/3 directives]
       ↓
[Build custom operator table]
       ↓
[Regenerate Lark parser with custom operators]
       ↓
[Pass 2: Parse full source with updated grammar]
       ↓
AST with operator syntax recognized
```

### Pass 1: Extract Operators

Extract `:- op(...)` directives using regex or simple parsing:

```python
def extract_operators(source: str) -> list[tuple[int, str, str]]:
    """Extract (precedence, type, name) tuples from source."""
    pattern = r':-\s*op\s*\(\s*(\d+)\s*,\s*(\w+)\s*,\s*([\w@#$&*+/<=>?^~-]+|\[[^\]]+\])\s*\)'
    # Return list of (prec, type, op) tuples
```

### Pass 2: Dynamic Grammar Generation

Generate a Lark grammar that includes custom operators:

```python
def generate_operator_rules(operators: list) -> str:
    """Generate Lark grammar rules for custom operators."""
    # Group by precedence
    # For each precedence level, generate infix/prefix/postfix rules
    # Handle associativity (yfx, xfy, etc.)
```

### Pass 3: Rebuild Parser

Create a new Lark parser with the updated grammar:

```python
def rebuild_parser_with_operators(base_grammar: str, operators: list) -> Lark:
    """Build new Lark parser with custom operators."""
    # Merge custom operator rules into base grammar
    # Recreate Lark parser instance
    # Return new parser
```

## Technical Challenges

### Challenge 1: Precedence Mapping

**Problem**: ISO uses low numbers for high precedence, Lark is opposite

**Solution**:
- ISO: 1 (highest) to 1200 (lowest)
- Lark: Rules in reverse order
- Must invert ISO precedence values

### Challenge 2: Associativity Handling

**Problem**: ISO has 8 associativity types, Lark has different system

**Mapping**:
- `yfx` (left-assoc) → Lark `%left`
- `xfy` (right-assoc) → Lark `%right`
- `xfx` (non-assoc) → No modifier
- `yfy` (fully-assoc) → Lark `%left` (closest match)
- Prefix: `fx`, `fy` → Handled in unary expression rules
- Postfix: `xf`, `yf` → Need special handling

### Challenge 3: Grammar Conflicts

**Problem**: Some operator combinations cause shift/reduce conflicts

**Solution**:
- Use Lark's priority system
- Test for grammar conflicts
- May need to adjust grammar structure

### Challenge 4: Operator Interaction

**Problem**: Custom operators can interact with built-in operators

**Solution**:
- Ensure custom operators integrate properly with existing precedence
- Test edge cases
- Document interaction rules

## Implementation Steps

### Step 1: Extract Operators Function

Create `vibeprolog/parser.py`:

```python
def extract_operators(source: str) -> list[tuple[int, str, str]]:
    """Extract op/3 directives from source code."""
    operators = []
    # Use regex to find :- op(...).
    # Parse each directive
    # Return list of (precedence, type, operator) tuples
    return operators
```

**Tests needed**:
- Extract single operator
- Extract multiple operators
- Handle list syntax `[op1, op2, ...]`
- Ignore non-op directives

### Step 2: Grammar Generation

Create `vibeprolog/parser.py`:

```python
def generate_operator_grammar(base_grammar: str, operators: list) -> str:
    """Generate Lark grammar with custom operators."""
    # Group operators by precedence
    # For each precedence level:
    #   For each operator:
    #     Generate appropriate rule (infix/prefix/postfix)
    # Integrate into base grammar
    return updated_grammar
```

**Tests needed**:
- Single infix operator
- Multiple operators at same precedence
- Operators at different precedences
- All 8 operator types
- Invalid operators

### Step 3: Parser Class Updates

Update `vibeprolog/parser.py`:

```python
class PrologParser:
    def parse(self, source: str):
        """Parse with two-pass approach."""
        # Pass 1: Extract operators
        ops = extract_operators(source)
        
        # Pass 2: Build grammar with operators
        grammar = generate_operator_grammar(self.base_grammar, ops)
        parser = Lark(grammar, start='program', parser='lalr')
        
        # Pass 3: Parse source
        tree = parser.parse(source)
        return self.transform(tree)
```

**Tests needed**:
- Parse with no custom operators
- Parse with one custom operator
- Parse with multiple custom operators
- Operators in clause heads
- Operators in clause bodies
- Operators in goals

### Step 4: Precedence Handling

Handle operator precedence correctly:

```python
def generate_precedence_rules(operators_by_prec: dict) -> str:
    """Generate rules handling precedence and associativity."""
    # ISO precedence: 1 (high) to 1200 (low)
    # Lark rules: order matters (later = lower precedence)
    
    # Invert and sort
    sorted_prec = sorted(operators_by_prec.keys(), reverse=True)
    
    # Generate rules from highest to lowest precedence
    rules = []
    for prec in sorted_prec:
        # Generate rules for this precedence level
        # Handle associativity
        pass
    
    return "\n".join(rules)
```

### Step 5: Integration with Interpreter

Update `vibeprolog/interpreter.py`:

```python
def consult_string(self, program: str):
    """Consult with two-pass parsing."""
    # Extract operators first
    ops = extract_operators(program)
    
    # Create parser with these operators
    if ops:
        self.parser = PrologParser(custom_operators=ops)
    
    # Parse program
    items = self.parser.parse(program)
    
    # Process items as usual
    self._process_items(items, source_name)
```

## Testing Strategy

### Unit Tests

Test each component in isolation:

```python
class TestExtractOperators:
    def test_extract_single_operator(self)
    def test_extract_multiple_operators(self)
    def test_extract_operator_list(self)
    def test_extract_ignores_non_op_directives(self)

class TestGenerateGrammar:
    def test_generate_single_infix_operator(self)
    def test_generate_multiple_operators(self)
    def test_generate_different_precedences(self)
    def test_generate_all_operator_types(self)

class TestTwoPassParsing:
    def test_parse_with_custom_infix_operator(self)
    def test_parse_with_custom_prefix_operator(self)
    def test_parse_with_custom_postfix_operator(self)
    def test_parse_with_multiple_custom_operators(self)
```

### Integration Tests

Test with complete Prolog programs:

```python
class TestOperatorSyntaxParsing:
    def test_family_relations_with_custom_operator(self)
    def test_dsl_with_custom_operators(self)
    def test_operator_precedence_affects_grouping(self)
    def test_operator_associativity_affects_grouping(self)
    def test_operator_interaction_with_builtins(self)
```

### Edge Case Tests

Test problematic operator combinations:

```python
class TestOperatorEdgeCases:
    def test_operator_shadowing_builtin(self)
    def test_ambiguous_operator_combinations(self)
    def test_operator_with_same_name_different_type(self)
    def test_deep_operator_nesting(self)
```

## Example Implementation (Outline)

```python
# In vibeprolog/parser.py

def extract_operators(source: str) -> list[tuple[int, str, list[str]]]:
    """Extract op/3 directives from Prolog source."""
    import re
    
    operators = []
    pattern = r':-\s*op\s*\(\s*(\d+)\s*,\s*(\w+)\s*,\s*(.+?)\s*\)\s*\.'
    
    for match in re.finditer(pattern, source):
        prec = int(match.group(1))
        op_type = match.group(2)
        names_str = match.group(3)
        
        # Parse names (could be atom or [atom, atom, ...])
        names = parse_op_names(names_str)
        
        for name in names:
            operators.append((prec, op_type, name))
    
    return operators


def generate_operator_grammar(base_grammar: str, operators: list) -> str:
    """Generate Lark grammar with custom operators."""
    
    # Group by precedence
    prec_groups = {}
    for prec, op_type, name in operators:
        if prec not in prec_groups:
            prec_groups[prec] = []
        prec_groups[prec].append((op_type, name))
    
    # Generate rules (simplified - full version is complex)
    op_rules = []
    
    # Sort by precedence (high to low = ISO order)
    for prec in sorted(prec_groups.keys(), reverse=True):
        ops = prec_groups[prec]
        for op_type, name in ops:
            if op_type in ('xfx', 'xfy', 'yfx', 'yfy'):
                # Infix
                op_rules.append(f'    | expr "{name}" expr')
            elif op_type in ('fx', 'fy'):
                # Prefix
                op_rules.append(f'    | "{name}" expr')
            elif op_type in ('xf', 'yf'):
                # Postfix
                op_rules.append(f'    | expr "{name}"')
    
    # Integrate into base grammar (complex - need to modify at right place)
    return base_grammar + "\n" + "\n".join(op_rules)
```

## Estimated Effort

- **Design & Architecture**: 4-8 hours
- **Implementation**: 20-40 hours
  - Extract operators: 2-4 hours
  - Grammar generation: 8-12 hours
  - Parser integration: 4-8 hours
  - Precedence/associativity: 6-16 hours
- **Testing**: 8-16 hours
  - Unit tests: 4-8 hours
  - Integration tests: 4-8 hours
- **Documentation**: 2-4 hours
- **Debugging & refinement**: 8-16 hours

**Total**: 40-80 hours

## Blockers & Risks

1. **Lark Limitations**: LALR parser may have issues with some operator combinations
2. **Performance**: Two-pass parsing slightly slower (acceptable)
3. **Grammar Conflicts**: May need to restructure grammar to avoid conflicts
4. **Associativity Edge Cases**: Complex interactions between operator types

## Success Criteria

1. ✅ All 8 ISO operator types work
2. ✅ Custom operator syntax parses correctly
3. ✅ Precedence and associativity respected
4. ✅ Operators work in all contexts (heads, bodies, goals)
5. ✅ Custom operators coexist with built-in operators
6. ✅ `current_op/3` returns custom operators
7. ✅ No performance regression
8. ✅ All existing tests still pass
9. ✅ Comprehensive new test coverage

## References

- **Lark Parser Docs**: https://lark-parser.readthedocs.io/
- **ISO Prolog Standard**: ISO/IEC 13211-1 Section 6.3.4
- **SWI-Prolog Operators**: https://www.swi-prolog.org/pldoc/man?section=operators
- **Current Implementation**: OPERATOR_IMPLEMENTATION_STATUS.md
- **Test Status**: tests/test_dynamic_operators.py (all tests passing)

## Next Steps

1. Create design document with final approach
2. Implement extract_operators function
3. Implement grammar generation
4. Create unit tests
5. Implement parser integration
6. Create integration tests
7. Refine and optimize
8. Full test suite validation
9. Documentation update
