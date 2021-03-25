# create plot for testing
p <- plot_cim(sim_data = sim_data,
              peak_list = peak_list,
              rt_line = 6.08,
              select_sim = "241",
              title = "SIM's of cystine")

wrong_sim_data <- sim_data
colnames(wrong_sim_data)[1] <- "rta"

wrong_peak_list <- peak_list
colnames(wrong_peak_list)[1] <- "wrong"

test_that("output checking", {
  expect_doppelganger("CIM graph", p)

  expect_equal(class(p), c("gg", "ggplot"))
})

test_that("input checking", {
  expect_error(plot_cim(sim_data = raw_data,
                        peak_list = peak_list,
                        rt_line = 6.08,
                        select_sim = "241",
                        title = "SIM's of cystine"),
               "'sim_data' is not a data frame!")

  expect_error(plot_cim(sim_data = wrong_sim_data,
                        peak_list = peak_list,
                        rt_line = 6.08,
                        select_sim = "241",
                        title = "SIM's of cystine"),
               "'sim_data' doesn't contain all the correct columns!")

  expect_error(plot_cim(sim_data = sim_data,
                        peak_list = raw_data,
                        rt_line = 6.08,
                        select_sim = "241",
                        title = "SIM's of cystine"),
               "'peak_list' does not appear to be a data frame!")

  expect_error(plot_cim(sim_data = sim_data,
                        peak_list = wrong_peak_list,
                        rt_line = 6.08,
                        select_sim = "241",
                        title = "SIM's of cystine"),
               "'peak_list' doesn't contain all the correct column names!")

  expect_error(plot_cim(sim_data = sim_data,
                        peak_list = peak_list,
                        rt_line = "6.08",
                        select_sim = "241",
                        title = "SIM's of cystine"),
               "'rt_line' should be a positive number!")

  expect_error(plot_cim(sim_data = sim_data,
                        peak_list = peak_list,
                        rt_line = -6.08,
                        select_sim = "241",
                        title = "SIM's of cystine"),
               "'rt_line' should be a positive number!")

  expect_error(plot_cim(sim_data = sim_data,
                        peak_list = peak_list,
                        rt_line = 6.08,
                        title = "SIM's of cystine"),
               "Define a sim trace to use!")

  expect_error(plot_cim(sim_data = sim_data,
                        peak_list = peak_list,
                        rt_line = 6.08,
                        select_sim = 241,
                        title = "SIM's of cystine"),
               "'select_sim' should be a character.")

  expect_warning(plot_cim(sim_data = sim_data,
                          peak_list = peak_list,
                          rt_line = 6.08,
                          select_sim = c("241", "200"),
                          title = "SIM's of cystine"),
                 "Only the first value of 'select_sim' will be used!")
})
