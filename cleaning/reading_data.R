# loading libraries -----------------
library(tidyverse)
library(dataQCtools)
library(here)
here::i_am("cleaning/reading_data.R")

# loading data ----------------------
sklallam_raw <- read_csv(here("data_for_proj3", "dungeness1.csv"), na = "", skip = 1)
hoh_wqts <- readRDS(here("data_for_proj3", "hoh_wqts.rds"))

# creating clean dataset ------------

## extracting names -----------------

orig_name <- "Copy of HOBO-temp-DO-Model-U26-001- Site-Dungeness 1"

sensor <- str_extract(string = colnames(sklallam_raw)[3], pattern = "\\d+") %>%
  as.numeric()
site <- str_extract(orig_name, pattern = "(?<=Site-).*$")
