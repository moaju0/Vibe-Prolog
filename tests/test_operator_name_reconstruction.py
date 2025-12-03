import unittest

from vibeprolog.utils import reconstruct_operator_name_from_term

class DummyAtom:
    def __init__(self, name):
        self.name = name

class DummyCompound:
    def __init__(self, functor, args):
        self.functor = functor
        self.args = args

class TestReconstructOperatorName(unittest.TestCase):
    def test_atom_reconstruction(self):
        a = DummyAtom("#=")
        self.assertEqual(reconstruct_operator_name_from_term(a), "#=")

    def test_single_arg_compound(self):
        inner = DummyAtom("#=")
        comp = DummyCompound("!", [inner])
        self.assertEqual(reconstruct_operator_name_from_term(comp), "!#=")

    def test_nested_compound(self):
        inner = DummyCompound("op", [DummyAtom("A")])
        outer = DummyCompound("prefix", [inner])
        self.assertEqual(reconstruct_operator_name_from_term(outer), "prefixopA")

    def test_non_reconstructible(self):
        class Empty: pass
        e = Empty()
        self.assertIsNone(reconstruct_operator_name_from_term(e))

if __name__ == '__main__':
    unittest.main()