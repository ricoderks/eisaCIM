# create plot for testing
p <- plot_reconstruct_spectrum(peak_list = peak_list_clean,
                               peak_group = 2,
                               title = "Reconstructed EISA fragmentation spectrum")

wrong_peaklist_clean <- peak_list_clean
colnames(wrong_peaklist_clean)[1] <- "wrong"

test_that("output checking", {
  expect_doppelganger("reconstructed spectrum graph", p)

  expect_equal(class(p), c("gg", "ggplot"))
})

test_that("input checking", {
  expect_error(plot_reconstruct_spectrum(peak_list = raw_data,
                                         peak_group = 2,
                                         title = "Reconstructed EISA fragmentation spectrum"),
               "'peak_list' does not appear to be a data frame!")

  expect_error(plot_reconstruct_spectrum(peak_list = wrong_peaklist_clean,
                                         peak_group = 2,
                                         title = "Reconstructed EISA fragmentation spectrum"),
               "'peak_list' doesn't contain all the correct column names!")

  expect_error(plot_reconstruct_spectrum(peak_list = peak_list_clean,
                                         title = "Reconstructed EISA fragmentation spectrum"),
               "No peak group selected!")

  expect_error(plot_reconstruct_spectrum(peak_list = peak_list_clean,
                                         peak_group = "2",
                                         title = "Reconstructed EISA fragmentation spectrum"),
               "'peak_group' should be a positive number of length 1!")

  expect_error(plot_reconstruct_spectrum(peak_list = peak_list_clean,
                                         peak_group = -2,
                                         title = "Reconstructed EISA fragmentation spectrum"),
               "'peak_group' should be a positive number of length 1!")

  expect_error(plot_reconstruct_spectrum(peak_list = peak_list_clean,
                                         peak_group = c(1, 2),
                                         title = "Reconstructed EISA fragmentation spectrum"),
               "'peak_group' should be a positive number of length 1!")

  expect_error(plot_reconstruct_spectrum(peak_list = peak_list_clean,
                                         peak_group = 2,
                                         xmin = -20,
                                         title = "Reconstructed EISA fragmentation spectrum"),
               "'xmin', 'xmax' and 'xstep' should be a positive number of length 1!")

  expect_error(plot_reconstruct_spectrum(peak_list = peak_list_clean,
                                         peak_group = 2,
                                         xmin = "20",
                                         title = "Reconstructed EISA fragmentation spectrum"),
               "'xmin', 'xmax' and 'xstep' should be a positive number of length 1!")

  expect_error(plot_reconstruct_spectrum(peak_list = peak_list_clean,
                                         peak_group = 2,
                                         xmin = c(20, 20),
                                         title = "Reconstructed EISA fragmentation spectrum"),
               "'xmin', 'xmax' and 'xstep' should be a positive number of length 1!")
  expect_error(plot_reconstruct_spectrum(peak_list = peak_list_clean,
                                         peak_group = 2,
                                         xmax = -260,
                                         title = "Reconstructed EISA fragmentation spectrum"),
               "'xmin', 'xmax' and 'xstep' should be a positive number of length 1!")

  expect_error(plot_reconstruct_spectrum(peak_list = peak_list_clean,
                                         peak_group = 2,
                                         xmax = "260",
                                         title = "Reconstructed EISA fragmentation spectrum"),
               "'xmin', 'xmax' and 'xstep' should be a positive number of length 1!")

  expect_error(plot_reconstruct_spectrum(peak_list = peak_list_clean,
                                         peak_group = 2,
                                         xmax = c(260, 200),
                                         title = "Reconstructed EISA fragmentation spectrum"),
               "'xmin', 'xmax' and 'xstep' should be a positive number of length 1!")

  expect_error(plot_reconstruct_spectrum(peak_list = peak_list_clean,
                                         peak_group = 2,
                                         xstep = -20,
                                         title = "Reconstructed EISA fragmentation spectrum"),
               "'xmin', 'xmax' and 'xstep' should be a positive number of length 1!")

  expect_error(plot_reconstruct_spectrum(peak_list = peak_list_clean,
                                         peak_group = 2,
                                         xstep = "20",
                                         title = "Reconstructed EISA fragmentation spectrum"),
               "'xmin', 'xmax' and 'xstep' should be a positive number of length 1!")

  expect_error(plot_reconstruct_spectrum(peak_list = peak_list_clean,
                                         peak_group = 2,
                                         xstep = c(20, 20),
                                         title = "Reconstructed EISA fragmentation spectrum"),
               "'xmin', 'xmax' and 'xstep' should be a positive number of length 1!")

  expect_error(plot_reconstruct_spectrum(peak_list = peak_list_clean,
                                         peak_group = 2,
                                         xmin = 260,
                                         xmax = 20,
                                         title = "Reconstructed EISA fragmentation spectrum"),
               "'xmin' should be smaller then 'xmax'!")
})
