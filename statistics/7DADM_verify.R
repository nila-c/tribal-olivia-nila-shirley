library(tidyverse)
library(dataQCtools)
library(here)

#RUN 7DADM-calc.R before the below
#code to verify our computed statistics by comparing to statistics from TQWD

#read in statistics from tqwd
ho_tqwd <- read_csv(here("Hoh_Data", "hoh_tqwd.csv"), na = "NULL")

#read in statistics from wqts and get the 7DADM values
ho_wqts_stats <- read_csv("Hoh_Data/hoh_wqts_stats.csv")
ho_wqts_stats_subset <- ho_wqts_stats %>% 
  filter(StatisticalBaseCode == "7DADM",
         CharacteristicName == "Temperature, water") %>%
  select(Stat_DATE, MONLOC_AB, StatMEASURE) %>%
  mutate(Date = parse_date_time(Stat_DATE,
                                orders = c("m/d/Y"))) %>%
  select(-Stat_DATE) %>%
  cbind(data.frame(source = "WQTS_Computed")) %>%
  rename("7DADM" = "StatMEASURE") %>%
  distinct()

#select relevant columns from R computed statistics and rename columns to compare with WQTS computed ones
ho_wqts_7dadm_combine <- ho_wqts_7dadm %>%
  select(MONLOC_AB, Date, sevenDADM) %>%
  rename("7DADM" = "sevenDADM") %>%
  cbind(source = "R_Computed")

#combine the WQTS and R computed statistics to compare5
verification_df <- rbind(ho_wqts_stats_subset, ho_wqts_7dadm_combine) %>%
  pivot_wider(id_cols = c("MONLOC_AB", "Date"),
              names_from = "source", 
              values_from = "7DADM") 

repeat_sites <- rbind(ho_wqts_stats_subset, ho_wqts_7dadm_combine) %>%
  summarise(n = n(), .by = c(MONLOC_AB, Date, source)) %>%
  filter(n > 1) %>%
  mutate(Stat_DATE = substr(format(Date, "%m/%d/%Y"), start = 2, stop = 10))

#fix data for JACKTH1 to match the wqts stats table format
repeat_sites[6,5] <- "7/1/2022"

#get verification df w/out repeat keys
verification_df_no_repeats <- verification_df[-which(unlist(lapply(verification_df$WQTS_Computed, function(x) {length(x) > 1}))),]

#compute difference in R and WQTS Stats computed 7DADM values
verification_df_no_repeats_test <- verification_df_no_repeats %>%
  mutate(rel_diff = if_else(is.null(WQTS_Computed) | is.null(R_Computed), NA, WQTS_Computed - R_Computed))


