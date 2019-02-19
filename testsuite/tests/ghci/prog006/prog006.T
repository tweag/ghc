test('prog006', [extra_files(['A.hs', 'Boot.hs-boot', 'Boot1.hs', 'Boot2.hs']), expect_broken(243)], ghci_script, ['prog006.script'])
