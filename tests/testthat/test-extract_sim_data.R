test_that("output is data frame", {
  expect_type(extract_sim_data(data = raw_data,
                               sim_names = sim_names,
                               sim_ids = sim_ids),
              "list")
})

test_that("input data is not MChromatograms", {
  expect_error(extract_sim_data(data = sim_names,
                               sim_names = sim_names,
                               sim_ids = sim_ids),
              "'data' is not a MChromatograms object!")
})

test_that("input data is not MChromatograms", {
  expect_error(extract_sim_data(data = raw_data,
                                sim_names = sim_names,
                                sim_ids = sim_ids[1]),
               "'sim_names' and 'sim_ids' need to have the same length!!")
})
