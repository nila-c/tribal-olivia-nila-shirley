library(tidyverse)
library(dataQCtools)
library(here)
library(data.table)
library(readxl)

here::i_am("cleaning/cropping.R")

# loading data -------------------

# location data
path <- here("data_for_proj3", "jst_data_ldr_times", "extdata", "data", "2015_summer")

# uncomment below for loading in data
# data_paths_orig <- list.files(path = path, pattern = "\\.csv$") %>% as.vector()
# data_paths <- paste(path, "/", data_paths_orig, sep = "")
# loc_data <- sapply(data_paths, fread, skip = 1, blank.lines.skip = TRUE)
# names(loc_data) <- data_paths_orig

# ldr times data
ldr_name <- list.files(path = path, pattern="\\.xlsx$") %>% as.vector()

# uncomment below for loading in data
# ldr_path <- paste(path, "/", ldr_name, sep = "")
# ldr_times <- read_excel(ldr_path)

# cropping ----------------------
data_paths_nospace <- sapply(data_paths, str_replace_all, pattern = " ", replacement = "")
file.rename(data_paths, data_paths_nospace)
paste0(path, "/") %>%
  crop_raw_data(ldrtimes_fn = ldr_name, cropped_loc = here("data_for_proj3"))