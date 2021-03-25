# create plot for testing
p1 <- plot_sims(sim_data,
               rt_line = 6.08,
               title = "SIM's of cystine")

p2 <- plot_sims(sim_data,
                rt_line = 6.08,
                title = "SIM's of cystine",
                peak_data = peak_data)

wrong_sim_data <- sim_data
colnames(wrong_sim_data) <- c("rta", "intensity", "sim")

test_that("does the plot look ok", {
  expect_doppelganger("SIM graph", p1)
  expect_doppelganger("SIM graph with boxes", p2)
})

test_that("output correct class",{
  expect_equal(class(p1), c("gg", "ggplot"))
})

test_that("check inputs", {
  expect_error(plot_sims(raw_data,
                         rt_line = 6.08,
                         title = "SIM's of cystine"),
               "'sim_data' is not a data frame!")

  expect_error(plot_sims(wrong_sim_data,
                         rt_line = 6.08,
                         title = "SIM's of cystine"),
               "'sim_data' doesn't contain all the correct columns!")

  expect_error(plot_sims(sim_data,
                         rt_line = 6.08,
                         title = "SIM's of cystine",
                         peak_data = raw_data),
               "'peak_data' is not a data frame!")

  expect_error(plot_sims(sim_data,
                         rt_line = 6.08,
                         title = "SIM's of cystine",
                         peak_data = wrong_peak_data),
               "'peak_data' doesn't contain all the correct columns!")

  expect_error(plot_sims(sim_data,
                         rt_line = "6.08",
                         title = "SIM's of cystine"),
               "'rt_line' should be a positive number!")

  expect_error(plot_sims(sim_data,
                         rt_line = -6.08,
                         title = "SIM's of cystine"),
               "'rt_line' should be a positive number!")

})
