library(testthat)
library(vdiffr)
library(eisaCIM)

my_test_file <- test_path("data", "mammalian_cell_ref_original_Hilic_pos_F2_eisa_sim.mzML")

sim_names <- c("241", "152", "120", "74")
sim_ids <- c(14, 12, 6, 25)

raw_data <- read_files(files = my_test_file)

sim_data <- extract_sim_data(data = raw_data,
                             sim_names = sim_names,
                             sim_ids = sim_ids)
