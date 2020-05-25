setTestOpts(only_ways(['normal']));

qextra_files = extra_files(['Vector.hs', 'Monad'])

test('qdorun001', [qextra_files], multimod_compile_and_run, ['qdorun001', ''])
test('qdorun002', [qextra_files], multimod_compile_and_run, ['qdorun002', ''])
