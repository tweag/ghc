test('haddockHtmlTest',
     [ignore_stdout, ignore_stderr, unless(in_tree_compiler(), skip), req_haddock, expect_broken(314)],
     makefile_test,
     ['htmlTest'])

test('haddockLatexTest',
     [ignore_stdout, ignore_stderr, unless(in_tree_compiler(), skip), req_haddock],
     makefile_test,
     ['latexTest'])

test('haddockHoogleTest',
     [ignore_stdout, ignore_stderr, unless(in_tree_compiler(), skip), req_haddock],
     makefile_test,
     ['hoogleTest'])

test('haddockHypsrcTest',
     [ignore_stdout, ignore_stderr, unless(in_tree_compiler(), skip), req_haddock, expect_broken(314)],
     makefile_test,
     ['hypsrcTest'])
