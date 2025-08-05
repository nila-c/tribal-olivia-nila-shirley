library(tidyverse)
library(dataQCtools)
library(here)

here::i_am("statistics/7DADM-calc.R")

# loading data -------------------
ho_tqwd <- read_csv(here("Hoh_Data", "Hoh_TWQD_DBO.csv"), na = "NULL")
ho_wqts <- readRDS(here("Hoh_Data", "ho_wqts.rds"))

# I'm not super sure which one is the actual SiteName column, so I'll use
# MONLOC_AB instead since that seems like the best bet

# Statistics are calculated in 
ho_wqts_7dadm <- ho_wqts %>%
  filter(CharacteristicName == "Temperature, water") %>%
  group_by(Date, MONLOC_AB) %>%
  summarise(
    DailyMax = max(ResultMeasureValue, na.rm = T),
    DailyMin = min(ResultMeasureValue, na.rm = T)) %>%
  ungroup() %>%
  group_by(MONLOC_AB) %>%
  mutate(sevenDADM = runner::mean_run(x = DailyMax,
                                             k = 7, lag = -3,
                                             idx = Date,
                                             na_pad = TRUE, na_rm = FALSE)) %>%
  ungroup()


