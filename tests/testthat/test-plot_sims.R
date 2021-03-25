# create plot for testing
p <- plot_sims(sim_data,
               rt_line = 5.3,
               title = "SIM's of cystine")

wrong_sim_data <- sim_data
colnames(wrong_sim_data) <- c("rta", "intensity", "sim")

test_that("does the plot look ok", {
  expect_doppelganger("SIM graph", p)
})

test_that("output correct class",{
  expect_equal(class(p), c("gg", "ggplot"))
})

test_that("check inputs", {
  expect_error(plot_sims(raw_data,
                         rt_line = 5.3,
                         title = "SIM's of cystine"),
               "'sim_data' is not a data frame!")

  expect_error(plot_sims(wrong_sim_data,
                         rt_line = 5.3,
                         title = "SIM's of cystine"),
               "'sim_data' doesn't contain all the correct columns!")

})
