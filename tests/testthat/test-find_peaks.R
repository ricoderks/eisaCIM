test_that("input checking", {
  expect_error(find_peaks(data = sim_data,
                          sim_names = sim_names,
                          sim_ids = sim_ids,
                          noise = noise),
               "'data' is not a MChromatograms object!")

  expect_error(find_peaks(data = raw_data,
                          sim_names = sim_names,
                          sim_ids = c(sim_ids, 10),
                          noise = noise),
               "'sim_ids', 'sim_names' and 'noise' all need to have the same length!!")

  expect_error(find_peaks(data = raw_data,
                          sim_names = sim_names,
                          sim_ids = as.character(sim_ids),
                          noise = noise),
               "'sim_ids' needs to be integer numbers!")

  expect_error(find_peaks(data = raw_data,
                          sim_names = sim_names,
                          sim_ids = sim_ids,
                          noise = as.character(noise)),
               "'noise' needs to be numerical!")

  expect_error(find_peaks(data = raw_data,
                          sim_names = sim_names,
                          sim_ids = sim_ids,
                          noise = noise,
                          snthresh = -10),
               "'snthresh' needs to be positive numerical and of length 1!")

  expect_error(find_peaks(data = raw_data,
                          sim_names = sim_names,
                          sim_ids = sim_ids,
                          noise = noise,
                          snthresh = as.character(10)),
               "'snthresh' needs to be positive numerical and of length 1!")

  expect_error(find_peaks(data = raw_data,
                          sim_names = sim_names,
                          sim_ids = sim_ids,
                          noise = noise,
                          peakwidth = as.character(c(5, 30))),
               "'peakwidth' needs to be numerical and of length 2!")

  expect_error(find_peaks(data = raw_data,
                          sim_names = sim_names,
                          sim_ids = sim_ids,
                          noise = noise,
                          peakwidth = 20),
               "'peakwidth' needs to be numerical and of length 2!")

  expect_error(find_peaks(data = raw_data,
                          sim_names = sim_names,
                          sim_ids = sim_ids,
                          noise = noise,
                          snthresh = c(10, 2)),
               "'snthresh' needs to be positive numerical and of length 1!")
})

test_that("output checking", {
  my_peaks <- find_peaks(data = raw_data,
                         sim_names = sim_names,
                         sim_ids = sim_ids,
                         noise = noise)

  my_cols <- colnames(my_peaks)
  expect_cols <- c("rt", "rtmin", "rtmax", "into", "intb", "maxo", "sn", "sim")

  expect_true(any(class(my_peaks) == "data.frame"))

  expect_true(all(my_cols %in% expect_cols))
  })
