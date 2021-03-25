test_that("output check", {
  my_dim <- dim(read_files(files = my_test_file))

  expect_equal(my_dim, c(32, 1))

  expect_s4_class(read_files(files = my_test_file),
                  "MChromatograms")
})

test_that("input check", {
  expect_error(read_files(files = 123),
               "'files' should be a character vector!")
})
