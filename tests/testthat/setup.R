library(testthat)
library(eisaCIM)

my_test_file <- test_path("ref", "mammalian_cell_ref_original_Hilic_pos_F2_eisa_sim.mzML")

sim_names <- c("241", "152", "120", "74")
sim_ids <- c(14, 12, 6, 25)

raw_data <- read_files(files = my_test_file)
