test_that("output is matrix", {
  expect_type(get_all_sim(data = raw_data),
              "double")
})

test_that("input data is not MChromatograms", {
  expect_error(get_all_sim(data = sim_names),
               "'data' is not a MChromatograms object!")
})

test_that("correct column names", {
  sim_col_name <- colnames(get_all_sim(data = raw_data))

  expect_identical(sim_col_name, "sim")
})

test_that("correct dimension output", {
  my_dim <- dim(get_all_sim(data = raw_data))

  expect_equal(my_dim, c(32, 1))
})
