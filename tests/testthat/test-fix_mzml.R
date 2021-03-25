test_that("input check", {
  expect_warning(fix_mzml(mzml_file = c(test_file_nofix, test_file_nofix),
                          new_mzml_file = test_file_fix),
                 "Length of 'mzml_file' is > 1, only first one is used!")

  expect_warning(fix_mzml(mzml_file = test_file_nofix,
                          new_mzml_file = c(test_file_fix, test_file_fix)),
                 "Length of 'new_mzml_file' is > 1, only first one is used!")

  expect_error(fix_mzml(mzml_file = test_file_nofix,
                        new_mzml_file = test_file_nofix),
               "Input file is the same as output file. Can not overwrite file!")

  expect_error(fix_mzml(mzml_file = my_test_file,
                        new_mzml_file = test_file_fix),
               "mzML file is probably already fixed!")
})
