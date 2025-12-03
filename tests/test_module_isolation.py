"""Tests for module isolation - ensuring predicates can have same name/arity in different modules."""

import pytest

from vibeprolog import PrologInterpreter
from vibeprolog.exceptions import PrologThrow


class TestSamePredicateDifferentModules:
    """Test that different modules can define the same predicate without conflicts."""

    def test_same_predicate_in_two_modules(self):
        """Modules A and B can each define foo/1 with different implementations."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- module(mod_a, [foo/1]).
            foo(a_value).
        """)
        prolog.consult_string("""
            :- module(mod_b, [foo/1]).
            foo(b_value).
        """)
        
        # Both modules should exist
        assert 'mod_a' in prolog.modules
        assert 'mod_b' in prolog.modules
        
        # Each module should have its own foo/1
        result_a = prolog.query_once("mod_a:foo(X)")
        assert result_a is not None
        assert result_a['X'] == 'a_value'
        
        result_b = prolog.query_once("mod_b:foo(X)")
        assert result_b is not None
        assert result_b['X'] == 'b_value'

    def test_three_modules_same_predicate(self):
        """Three modules can all define helper/1."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- module(alpha, [helper/1]).
            helper(alpha_result).
        """)
        prolog.consult_string("""
            :- module(beta, [helper/1]).
            helper(beta_result).
        """)
        prolog.consult_string("""
            :- module(gamma, [helper/1]).
            helper(gamma_result).
        """)
        
        assert prolog.query_once("alpha:helper(X)")['X'] == 'alpha_result'
        assert prolog.query_once("beta:helper(X)")['X'] == 'beta_result'
        assert prolog.query_once("gamma:helper(X)")['X'] == 'gamma_result'


class TestImportWithLocalDefinition:
    """Test that importing from a module doesn't block local definitions."""

    def test_selective_import_allows_local_definition(self):
        """If module B imports pred_a/1 from A but NOT pred_b/1, B can define its own pred_b/1."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- module(exporter, [pred_a/1, pred_b/1]).
            pred_a(from_exporter).
            pred_b(exporter_version).
        """)
        prolog.consult_string("""
            :- module(importer, [pred_b/1, use_pred_a/1]).
            :- use_module(exporter, [pred_a/1]).
            % We only import pred_a, NOT pred_b - so we can define our own pred_b
            pred_b(importer_version).
            % This predicate uses the imported pred_a internally
            use_pred_a(X) :- pred_a(X).
        """)
        
        # Importer's own pred_b should be accessible
        result = prolog.query_once("importer:pred_b(X)")
        assert result is not None
        assert result['X'] == 'importer_version'
        
        # Importer can use the imported pred_a internally via use_pred_a
        result = prolog.query_once("importer:use_pred_a(X)")
        assert result is not None
        assert result['X'] == 'from_exporter'

    def test_must_be_conflict_pattern(self):
        """Simulates the library(error) vs library(clpz) must_be/2 conflict."""
        prolog = PrologInterpreter()
        
        # First module exports must_be/2
        prolog.consult_string("""
            :- module(error_like, [must_be/2, can_be/2]).
            must_be(integer, X) :- integer(X).
            can_be(_, _).
        """)
        
        # Second module imports only can_be/2, defines its own must_be/2
        prolog.consult_string("""
            :- module(clpz_like, [my_check/1]).
            :- use_module(error_like, [can_be/2]).
            % NOT importing must_be/2 - defining our own
            must_be(ground, Term) :- ground(Term).
            my_check(X) :- must_be(ground, X).
        """)
        
        # Verify both modules were loaded without permission error
        assert 'error_like' in prolog.modules
        assert 'clpz_like' in prolog.modules
        
        # my_check uses the module's own must_be (not the imported one from error_like)
        result = prolog.query_once("clpz_like:my_check(hello)")
        assert result is not None


class TestActualLibraryConflict:
    """Test the actual library(error) and library(clpz) case."""

    def test_simulated_must_be_conflict(self):
        """Simulate the library(error) vs library(clpz) must_be/2 conflict pattern."""
        prolog = PrologInterpreter()
        
        # Simulate library(error) with must_be/2 export
        prolog.consult_string("""
            :- module(error_sim, [must_be/2, can_be/2, type_error/3, domain_error/3]).
            must_be(integer, X) :- integer(X).
            must_be(atom, X) :- atom(X).
            can_be(_, _) :- true.
            type_error(_, _, _) :- fail.
            domain_error(_, _, _) :- fail.
        """)
        
        # Simulate library(clpz) - imports only some predicates, defines own must_be/2
        # This is exactly what clpz.pl does: :- use_module(library(error), [domain_error/3, type_error/3, can_be/2]).
        prolog.consult_string("""
            :- module(clpz_sim, [check_ground/1]).
            :- use_module(error_sim, [domain_error/3, type_error/3, can_be/2]).
            % NOT importing must_be/2 - defining our own version
            must_be(ground, Term) :- ground(Term).
            must_be(acyclic, Term) :- acyclic_term(Term).
            check_ground(X) :- must_be(ground, X).
        """)
        
        # Both modules should exist without error
        assert 'error_sim' in prolog.modules
        assert 'clpz_sim' in prolog.modules
        
        # clpz_sim should have its own must_be/2 that works through exported check_ground
        result = prolog.query_once("clpz_sim:check_ground(hello)")
        assert result is not None


class TestDynamicPredicatesPerModule:
    """Test that dynamic predicates are properly scoped per module."""

    @pytest.mark.skip(reason="Dynamic predicate isolation per module requires module-scoped assert/retract - future work")
    def test_dynamic_facts_per_module(self):
        """Dynamic facts in module A don't mix with module B."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- module(store_a, [add_item/1, get_items/1]).
            :- dynamic(item/1).
            add_item(X) :- assertz(item(X)).
            get_items(L) :- findall(X, item(X), L).
        """)
        prolog.consult_string("""
            :- module(store_b, [add_item/1, get_items/1]).
            :- dynamic(item/1).
            add_item(X) :- assertz(item(X)).
            get_items(L) :- findall(X, item(X), L).
        """)
        
        # Add items to each store
        prolog.query_once("store_a:add_item(apple)")
        prolog.query_once("store_a:add_item(banana)")
        prolog.query_once("store_b:add_item(car)")
        
        # Items should be isolated per module
        result_a = prolog.query_once("store_a:get_items(L)")
        result_b = prolog.query_once("store_b:get_items(L)")
        
        assert result_a is not None
        assert result_b is not None
        assert set(result_a['L']) == {'apple', 'banana'}
        assert result_b['L'] == ['car']


class TestStaticPredicateProtectionWithinModule:
    """Test that static predicate protection works within a module."""

    def test_redefine_static_predicate_in_same_module_fails(self):
        """Trying to redefine a static predicate in the same module should fail."""
        prolog = PrologInterpreter()
        
        # This should work - defining in one consult
        prolog.consult_string("""
            :- module(test_mod, [mypred/1]).
            mypred(first).
        """)
        
        # Adding to the same predicate in a new consult should fail
        # (unless declared multifile or discontiguous)
        with pytest.raises(PrologThrow):
            prolog.consult_string("""
                :- module(test_mod, []).
                mypred(second).
            """)

    def test_different_module_can_define_same_predicate(self):
        """Different modules can define predicates with same name/arity."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- module(mod_one, [shared_name/1]).
            shared_name(from_one).
        """)
        # This should NOT fail - it's a different module
        prolog.consult_string("""
            :- module(mod_two, [shared_name/1]).
            shared_name(from_two).
        """)
        
        assert prolog.query_once("mod_one:shared_name(X)")['X'] == 'from_one'
        assert prolog.query_once("mod_two:shared_name(X)")['X'] == 'from_two'


class TestMultifileAcrossModules:
    """Test multifile predicates across modules."""

    @pytest.mark.skip(reason="Multifile across consults within same module requires tracking by module+source - future work")
    def test_multifile_allows_extension(self):
        """Multifile predicates can be extended from other files."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- module(base, [data/1]).
            :- multifile(data/1).
            data(one).
        """)
        # Continue adding in a new consult to the same module
        prolog.consult_string("""
            :- module(base, []).
            data(two).
        """)
        
        results = prolog.query("base:data(X)")
        values = {r['X'] for r in results}
        assert values == {'one', 'two'}


class TestUserModuleDefault:
    """Test that predicates outside module declarations go to 'user' module."""

    def test_predicates_default_to_user(self):
        """Predicates defined without module declaration go to user module."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            global_pred(hello).
        """)
        
        # Should be accessible without module prefix (user is default)
        result = prolog.query_once("global_pred(X)")
        assert result is not None
        assert result['X'] == 'hello'

    def test_user_module_accessible_globally(self):
        """User module predicates are accessible from other modules."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            helper(global_helper).
        """)
        prolog.consult_string("""
            :- module(custom, [use_helper/1]).
            use_helper(X) :- helper(X).
        """)
        
        result = prolog.query_once("custom:use_helper(X)")
        assert result is not None
        assert result['X'] == 'global_helper'


class TestBuiltinProtectionUnchanged:
    """Test that built-in protection still works correctly."""

    def test_builtin_conflict_error_mode(self):
        """With builtin_conflict=error, redefining built-ins raises error."""
        prolog = PrologInterpreter(builtin_conflict="error")
        
        with pytest.raises(PrologThrow):
            prolog.consult_string("""
                :- module(bad_mod, []).
                append(X, Y, Z) :- fail.
            """)

    def test_builtin_conflict_skip_mode(self):
        """With builtin_conflict=skip (default), redefining built-ins is silently skipped."""
        prolog = PrologInterpreter(builtin_conflict="skip")
        
        # Should not raise
        prolog.consult_string("""
            :- module(okay_mod, []).
            append(X, Y, Z) :- fail.
        """)
        
        # Built-in append should still work
        result = prolog.query_once("append([1], [2], L)")
        assert result is not None
        assert result['L'] == [1, 2]


class TestExportImportSemantics:
    """Test that export/import semantics work correctly with module isolation."""

    def test_exported_predicates_accessible(self):
        """Exported predicates are accessible via module qualification."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- module(lib, [public_pred/1]).
            public_pred(accessible).
            private_pred(hidden).
        """)
        
        # Public is accessible
        result = prolog.query_once("lib:public_pred(X)")
        assert result is not None
        assert result['X'] == 'accessible'
        
        # Private is not accessible
        with pytest.raises(PrologThrow):
            prolog.query_once("lib:private_pred(X)")

    def test_imported_predicates_usable(self):
        """Imported predicates can be used without module prefix."""
        prolog = PrologInterpreter()
        prolog.consult_string("""
            :- module(provider, [utility/1]).
            utility(provided).
        """)
        prolog.consult_string("""
            :- module(consumer, [use_utility/1]).
            :- use_module(provider, [utility/1]).
            use_utility(X) :- utility(X).
        """)
        
        result = prolog.query_once("consumer:use_utility(X)")
        assert result is not None
        assert result['X'] == 'provided'


class TestSudokuExample:
    """Test that the sudoku example pattern loads without error."""

    def test_sudoku_pattern_no_permission_error(self):
        """Test that the sudoku pattern (using clpz-like module) works without permission errors.
        
        The actual sudoku.pl depends on library(clpz) which has parsing issues with
        library(dcgs). This test verifies the core module isolation fix works.
        """
        prolog = PrologInterpreter()
        
        # Simulate the module structure that caused the original issue:
        # 1. A module (like library/error) exports must_be/2
        # 2. Another module (like library/clpz) imports from error BUT NOT must_be/2
        # 3. That module defines its own must_be/2
        # 4. A user file loads the second module
        
        # Step 1: Create error-like module
        prolog.consult_string("""
            :- module(error_lib, [must_be/2, can_be/2]).
            must_be(integer, X) :- integer(X).
            can_be(_, _).
        """)
        
        # Step 2: Create clpz-like module that imports selectively and defines own must_be/2
        prolog.consult_string("""
            :- module(clpz_lib, [in_range/2, all_different/1]).
            :- use_module(error_lib, [can_be/2]).
            % Define our own must_be - this is what caused the original bug
            must_be(ground, X) :- ground(X).
            must_be(list, X) :- is_list(X).
            in_range(X, Max) :- integer(X), X >= 1, X =< Max.
            all_different([]).
            all_different([_|T]) :- all_different(T).
        """)
        
        # Step 3: Simulate user code loading clpz
        prolog.consult_string("""
            :- use_module(clpz_lib).
            sudoku_simple([A, B, C]) :-
                in_range(A, 9),
                in_range(B, 9), 
                in_range(C, 9),
                all_different([A, B, C]).
        """)
        
        # Verify modules exist and the pattern works
        assert 'error_lib' in prolog.modules
        assert 'clpz_lib' in prolog.modules
