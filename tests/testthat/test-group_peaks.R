wrong_peak_list <- peak_list
colnames(wrong_peak_list)[1] <- "wrong"

test_that("input check", {
  expect_error(group_peaks(peak_list = raw_data,
                           rt_diff = 5),
               "'peak_list' does not appear to be a data frame!")

  expect_error(group_peaks(peak_list = wrong_peak_list,
                           rt_diff = 5),
               "'peak_list' doesn't contain all the correct column names!")

  expect_error(group_peaks(peak_list = peak_list,
                           rt_diff = -5),
               "'rt_diff' should be a positive number and of length 1!")

  expect_error(group_peaks(peak_list = peak_list,
                           rt_diff = "5"),
               "'rt_diff' should be a positive number and of length 1!")

  expect_error(group_peaks(peak_list = peak_list,
                           rt_diff = c(5, 10)),
               "'rt_diff' should be a positive number and of length 1!")
})

test_that("output check", {
  grouped <- group_peaks(peak_list = peak_list,
                         rt_diff = 5)

  grouped_cols <- colnames(grouped)
  my_cols <- c("rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "sim",
               "peak_group", "num_peaks", "min_rt", "max_rt")

  expect_true(all(my_cols %in% grouped_cols))

  expect_true(any(class(grouped) == "data.frame"))
})
