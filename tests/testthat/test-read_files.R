test_that("correct S4 class output", {
  expect_s4_class(read_files(files = my_test_file),
                  "MChromatograms")
})

test_that("output correct dimensions", {
  my_dim <- dim(read_files(files = my_test_file))

  expect_equal(my_dim, c(32, 1))
})
