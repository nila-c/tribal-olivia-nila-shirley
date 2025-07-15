# notes for creating a water quality standard (WQS) lookup table
# Kim Bray and Jess Kunke, Fri Apr 26 2024

# Goal: given quality-controlled data for a given water year,
# identify which sites are impaired and the dates of their exceedances

# a creek/site is impaired if its temperatures exceed water quality standards
# (WQS) at least twice during the water year and there is a period of at least
# 7 days between two of those exceedances

# The output of this script is a table like the following:

# Site      Impaired   Dates of impairment
# Site 1    No
# Site 2    Yes         3/17-3/19, 5/21
# Site 3    Yes         10/20, 4/28-4/29
# Site 4    No
# Site 5    No
# ...       ...         ...

# Summary of this code:
# - Read in quality-controlled data for all sites for a given water year
# - compute 7DADM
# - use a site-group lookup table to determine which WQS group each site
#   is in
# - use a group-WQS lookup table to determine what the WQS is for that
#   group over the course of the water year
# - make the site impairments table (example shown above)

# site-group lookup table:
# - two sites are in the same group if they have the same WQS
#   schedule as each other throughout the water year
# - there is one row for each site
#
# example:
#
# Site              Group
# -----------------------------
# AndersonLower     1
# AndersonUpper     1
# CedarLower        1
# CedarUpper        2

# group-WQS lookup table:
# - provides details for the WQS schedule for each group
# - there is one row for each group-WQS combination from 01/01 to 12/31
#   for example, if a group has three different WQS throughout
#   the water year, there are three rows for that group
# - this can be generated using the write_group_wqs() function,
#   or it can be written in a spreadsheet and exported to a csv file
#   called group_wqs.csv
#
# example:
#
# Group   FromDate  ToDate  WQS
# -----------------------------
# 1       01/01     12/31   15
# -----------------------------
# 2       01/01     02/14   12
# 2       02/15     07/01   16
# 2       07/02     12/31   12
# -----------------------------

source("code/dataQCfunctions.R")

# all packages needed for dataQCfunctions.R are loaded in that script,
# but this script also uses the magrittr pipe (%!>%), so we load that here
library(magrittr)

# to handle data from a new water year, just change these file paths
fall_data_loc <- "data/2021_fall/4_final_data/"
sum_data_loc <- "data/2022_summer/4_final_data/"

# read in site-group lookup table
# - For each site, which WQS group is it in?
site_group = read_csv("lookup_tables/site_group.csv") %>%
  # removes any completely empty rows
  filter_all(any_vars(!is.na(.)))

# # uncomment next line of code to write group_wqs to file;
# # first edit the write_group_wqs() function in watertemp_functions.R
# # if you want to update the group_wqs table
# write_group_wqs(base_loc)

# read in group-WQS lookup table
# - For each group, how does the WQS change over the water year?
group_wqs = read_csv("lookup_tables/group_wqs.csv") %>%
  # removes any completely empty rows (in case this data frame is written
  # in Excel in the future and Excel adds extra empty lines)
  filter_all(any_vars(!is.na(.)))

# read in quality-controlled data...
wq_data = read_in_qcd_data(fall_data_loc, sum_data_loc) %!>%
  # compute 7DADM for this data
  compute_7DADM() %!>%
  # use site-group lookup table to determine which WQS group each site is in
  match_sites_to_WQS_groups(site_group) %!>%
  # use group-WQS lookup table to determine what the WQS is for each group
  # over the course of the water year, and determine when exceedances happen
  match_group_to_WQS(group_wqs)

# generate the desired site impairment table
site_impairments = site_impairments_table(wq_data)


