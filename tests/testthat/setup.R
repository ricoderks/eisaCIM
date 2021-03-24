library(testthat)
library(eisaCIM)

sim_names <- c("241", "152", "120", "74")
sim_ids <- c(14, 12, 6, 25)

raw_data <- read_files(files = test_path("ref", "mammalian_cell_ref_original_Hilic_pos_F2_eisa_sim.mzML"))
