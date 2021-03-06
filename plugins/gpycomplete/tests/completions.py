import unittest
import gpycomplete
from base import *
import sys, os

code = """
import os\n
import sys\n

def function(a, b=1):\n\t
    bar = a\n\t
    foo = 2\n

class Class(object):\n\t

    def __init__(self, arg1, arg2=2):\n\t
        self.arg1 = arg1\n
        self.arg2 = arg2\n

    def test(self, append):\n\t
        print self.arg1 + append

class_obj = Class(1, 2)
"""

class CompletionsTestCase(BaseTestCase):

    def testSameWordCompletion(self):
        self.assertEquals(gpycomplete.get_completions('sys', code), ['sys'])
        self.assertEquals(gpycomplete.get_completions('os', code), ['os'])

    def testNoCompletion(self):
        self.assertEquals(gpycomplete.get_completions('asdf', code), [])
        self.assertEquals(gpycomplete.get_completions('asdf.', code), [])

    def testImportCompletion(self):
        self.assertEquals(gpycomplete.get_completions('sys.', code), gpycomplete._get_dir(sys))
        self.assertEquals(gpycomplete.get_completions('os.', code), gpycomplete._get_dir(os))
        self.assertEquals(gpycomplete.get_completions('sys.path', code), ['path', 'path_hooks', 'path_importer_cache'])
        self.assertEquals(gpycomplete.get_completions('os.de', code), ['defpath', 'devnull'])

    def testGlobalNameCompletion(self):
        self.assertEquals(gpycomplete.get_completions('functi', code), ['function'])
        self.assertEquals(gpycomplete.get_completions('Cla', code), ['Class'])

    def testLocalNameCompletion(self):
        self.assertEquals(gpycomplete.get_completions('fo', code, [["", 'def', 'function']], '\t'), ['foo'])
        self.assertEquals(gpycomplete.get_completions('__ini', code, [["", 'class', 'Class']], '\t'), ['__init__'])


if __name__=='__main__':
    unittest.main()
