"""Prolog exception classes and error helpers."""

from __future__ import annotations

from typing import Any

from vibeprolog.terms import Atom, Compound


class PrologThrow(Exception):
    """Exception raised when throw/1 is executed to unwind the call stack."""

    def __init__(self, term: Any):
        self.term = term


class PrologError:
    """Helper to construct ISO Prolog error terms."""

    @staticmethod
    def _create_error_with_context(error_term, context: str | None = None) -> "Compound":
        if context:
            context_term = Compound('context', (Atom(context),))
            return Compound('error', (error_term, context_term))
        return Compound('error', (error_term, Atom('unknown')))

    @staticmethod
    def instantiation_error(context: str | None = None) -> "Compound":
        """Create an instantiation_error term.

        Args:
            context: Name of the predicate that caused the error

        Returns:
            error(instantiation_error, context(Predicate))
        """
        error_term = Atom('instantiation_error')
        return PrologError._create_error_with_context(error_term, context)

    @staticmethod
    def type_error(expected_type: str, culprit: Any, context: str | None = None) -> "Compound":
        """Create a type_error term.

        Args:
            expected_type: The expected type (e.g., 'integer', 'atom', 'list')
            culprit: The actual value that was wrong
            context: Name of the predicate that caused the error

        Returns:
            error(type_error(ExpectedType, Culprit), context(Predicate))
        """
        error_term = Compound('type_error', (Atom(expected_type), culprit))
        return PrologError._create_error_with_context(error_term, context)

    @staticmethod
    def domain_error(valid_domain: str, culprit: Any, context: str | None = None) -> "Compound":
        """Create a domain_error term.

        Args:
            valid_domain: Description of valid domain (e.g., 'not_less_than_zero')
            culprit: The value that's outside the valid domain
            context: Name of the predicate that caused the error

        Returns:
            error(domain_error(ValidDomain, Culprit), context(Predicate))
        """
        error_term = Compound('domain_error', (Atom(valid_domain), culprit))
        return PrologError._create_error_with_context(error_term, context)

    @staticmethod
    def permission_error(
        operation: str, permission_type: str, culprit: Any, context: str | None = None
    ) -> "Compound":
        """Create a permission_error term.

        Args:
            operation: The attempted operation (e.g., 'modify')
            permission_type: The type of permission denied (e.g., 'static_procedure')
            culprit: The offending value
            context: Name of the predicate that caused the error

        Returns:
            error(permission_error(Operation, PermissionType, Culprit), context(Predicate))
        """
        error_term = Compound(
            'permission_error', (Atom(operation), Atom(permission_type), culprit)
        )
        return PrologError._create_error_with_context(error_term, context)

    @staticmethod
    def syntax_error(description: str, context: str | None = None) -> "Compound":
        """Create a syntax_error term.

        Args:
            description: Description of the syntax error
            context: Name of the predicate that caused the error

        Returns:
            error(syntax_error(Description), context(Predicate))
        """
        error_term = Compound('syntax_error', (Atom(description),))
        return PrologError._create_error_with_context(error_term, context)

    @staticmethod
    def existence_error(object_type: str, culprit: Any, context: str | None = None) -> "Compound":
        """Create an existence_error term.

        Args:
            object_type: Type of object (e.g., 'procedure', 'file')
            culprit: The reference that doesn't exist
            context: Name of the predicate that caused the error

        Returns:
            error(existence_error(ObjectType, Culprit), context(Predicate))
        """
        error_term = Compound('existence_error', (Atom(object_type), culprit))
        return PrologError._create_error_with_context(error_term, context)

    @staticmethod
    def evaluation_error(error_type: str, context: str | None = None) -> "Compound":
        """Create an evaluation_error term.

        Args:
            error_type: The type of evaluation error (e.g., 'zero_divisor', 'undefined', 'float_overflow', 'underflow')
            context: Name of the predicate that caused the error

        Returns:
            error(evaluation_error(ErrorType), context(Predicate))
        """
        error_term = Compound('evaluation_error', (Atom(error_type),))
        return PrologError._create_error_with_context(error_term, context)

    @staticmethod
    def resource_error(resource: str, context: str | None = None) -> "Compound":
        """Create a resource_error term.

        Args:
            resource: Type of resource (e.g., 'recursion_depth_exceeded', 'stack')
            context: Optional context (predicate/arity)

        Returns:
            error(resource_error(Resource), context(Predicate))
        """
        error_term = Compound('resource_error', (Atom(resource),))
        return PrologError._create_error_with_context(error_term, context)

    @staticmethod
    def permission_error(operation: str, permission_type: str, culprit: Any, context: str | None = None) -> "Compound":
        """Create a permission_error term.

        Args:
            operation: The operation that was denied (e.g., 'open', 'access')
            permission_type: Type of permission (e.g., 'source_sink', 'input', 'output')
            culprit: The object for which permission was denied
            context: Name of the predicate that caused the error

        Returns:
            error(permission_error(Operation, PermissionType, Culprit), context(Predicate))
        """
        error_term = Compound('permission_error', (Atom(operation), Atom(permission_type), culprit))
        return PrologError._create_error_with_context(error_term, context)


__all__ = ["PrologThrow", "PrologError"]