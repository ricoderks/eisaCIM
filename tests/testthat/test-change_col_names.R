test_that("Correct column names are exported", {
  expect_equal(change_col_names(), c("rt", "trans_1", "trans_2", "trans_3"))
})
