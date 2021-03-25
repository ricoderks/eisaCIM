library(testthat)
library(vdiffr)
library(withr)
library(eisaCIM)

set.seed(123)

my_test_file <- test_path("data", "mammalian_cell_ref_original_Hilic_pos_F2_eisa_sim.mzML")
test_file_nofix <- test_path("data", "mammalian_cell_ref_original_Hilic_neg_F1_eisa_sim.mzML")
test_file_fix <- test_path("data", "fixed.mzML")

sim_names <- c("241", "152", "120", "74")
sim_ids <- c(14, 12, 6, 25)
noise <- c(1e6, 5e6, 5e6, 5e6)

raw_data <- read_files(files = my_test_file)

sim_data <- data.frame(rt = rep(seq(0, 12, 0.01), 4),
                       intensity = c(c(rnorm(300) * 1000, 1e4, 1e5, 1e6, 1e5, 1e4, rnorm(300) * 1000,
                                       1e4, 1e5, 1e6, 1e7, 1e6, 1e5, 1e4, rnorm(300) * 1000, rnorm(289) * 1000),
                                     c(rnorm(300) * 1000, 1e4, 1e5, 1e6, 1e7, 1e6, 1e5, 1e4, rnorm(300) * 1000,
                                       1e4, 1e5, 5e6, 1e5, 1e4, rnorm(300) * 1000, rnorm(289) * 1000),
                                     c(rnorm(300) * 1000, 1e4, 1e5, 5e6, 1e5, 1e4, rnorm(300) * 1000,
                                       1e4, 1e5, 1e6, 2e6, 1e6, 1e5, 1e4, rnorm(300) * 1000, rnorm(289) * 1000),
                                     c(rnorm(300) * 1000, 1e4, 1e5, 1e6, 1e7, 1e6, 1e5, 1e4, rnorm(300) * 1000,
                                       1e4, 1e5, 8e6, 1e5, 1e4, rnorm(300) * 1000, rnorm(289) * 1000)),
                       sim = factor(c(rep("241", 1201), rep("152", 1201), rep("120", 1201), rep("74", 1201)),
                                    labels = c("241", "152", "120", "74"),
                                    levels = c("241", "152", "120", "74")))

rt <- c(rnorm(4, sd = 0.01) + 3, rnorm(4, sd = 0.01) + 6.08)
peak_data <- data.frame(rt = rt,
                        rtmin = rt - 0.1,
                        rtmax = rt + 0.1,
                        into = c(1e6, 1e7, 5e6, 1e7, 1e7, 5e6, 2e6, 8e6),
                        intb = c(1e6, 1e7, 5e6, 1e7, 1e7, 5e6, 2e6, 8e6),
                        maxo = c(1e6, 1e7, 5e6, 1e7, 1e7, 5e6, 2e6, 8e6),
                        sn = rep(1000, 8),
                        sim = factor(c("241", "152", "120", "74", "241", "152", "120", "74"),
                                     labels = c("241", "152", "120", "74"),
                                     levels = c("241", "152", "120", "74")))

peak_list <- data.frame(rt = rt,
                        rtmin = rt - 0.1,
                        rtmax = rt + 0.1,
                        into = c(1e6, 1e7, 5e6, 1e7, 1e7, 5e6, 2e6, 8e6),
                        intb = c(1e6, 1e7, 5e6, 1e7, 1e7, 5e6, 2e6, 8e6),
                        maxo = c(1e6, 1e7, 5e6, 1e7, 1e7, 5e6, 2e6, 8e6),
                        sn = rep(1000, 8),
                        sim = factor(c("241", "152", "120", "74", "241", "152", "120", "74"),
                                     labels = c("241", "152", "120", "74"),
                                     levels = c("241", "152", "120", "74")),
                        peak_group = c(rep(1, 4), rep(2, 4)),
                        num_peaks = rep(2, 8))
peak_list$min_rt[peak_list$peak_group == 1] <- mean(peak_list$rtmin[peak_list$peak_group == 1])
peak_list$min_rt[peak_list$peak_group == 2] <- mean(peak_list$rtmin[peak_list$peak_group == 2])
peak_list$max_rt[peak_list$peak_group == 1] <- mean(peak_list$rtmax[peak_list$peak_group == 1])
peak_list$max_rt[peak_list$peak_group == 2] <- mean(peak_list$rtmax[peak_list$peak_group == 2])

wrong_peak_data <- data.frame(rt = c(2.78255009651184, 2.78255009651184, 2.83051657676697, 2.86890006065369, 5.31559991836548, 5.31559991836548, 5.31559991836548, 5.31559991836548),
                              rtmin = c(2.60983324050903, 2.68659996986389,  2.67700004577637, 2.70578336715698, 5.1716833114624, 5.1716833114624,5.1620831489563, 5.1716833114624),
                              rtmin = c(2.60983324050903, 2.68659996986389, 2.67700004577637, 2.70578336715698, 5.1716833114624, 5.1716833114624, 5.1620831489563, 5.1716833114624),
                              rtmax = c(3.02241659164429, 3.0128333568573, 3.0128333568573, 3.15674996376038, 5.45953321456909, 5.45953321456909, 5.45953321456909, 5.45953321456909),
                              into = c(19213835.0852645, 5285628.44256639, 687582.655131074, 4051855.7306413, 7040737.57903854, 1495514.54780904, 963515.374510488, 4247141.30654748),
                              intb = c(18771634.7464581, 2907281.22968473, 340892.042094804, 4051855.27007967, 6712647.07121426,1067669.17084884, 601885.764096645, 2952910.09276223),
                              maxo = c(83016000, 50180728, 3815220, 23230656, 96641024, 15794776, 9582360, 36750584),
                              sn = c(853, 93, 11, 23230655, 443, 48, 16, 111),
                              si = c("152", "74", "214", "120", "241", "152", "120", "74"))

#clean up
defer(unlink(test_file_fix), teardown_env())
