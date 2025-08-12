library(tidyverse)
library(dataQCtools)
library(here)

here::i_am("cleaning/quality_control.R")

# note: this should be run after 'cropping.R'

# load cropped data -------------------------

crop_path <- here("data_for_proj3", "cropped_data")
csv_files <- list.files(path = crop_path, pattern = '*csv')
csv_file_path <- paste0(crop_path, "/", csv_files)
crop_data <- lapply(csv_file_path, read_csv)
names(crop_data) <- str_extract(csv_files, pattern = "[^_]+")

# creating qc plots  ------------------------

qc_loc <- here("plots", "qc") %>% paste0("/")
make_qc_plots_of_cropped_data(paste0(crop_path, "/"), qc_loc)