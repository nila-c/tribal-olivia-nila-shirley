# this script crops raw data from HOBO files
#
# input: raw data files (csv format), ldrtimes file (Excel format)
# output: cropped data files (csv format), plots of cropped data
#
# Important:
#   Before running this script, rename raw HOBO files to have filenames in 
#     this format: sitename_medium_deployseason_deployyear.csv
#     e.g. NolanLower_air_sum_23.csv
# 
#   Make sure ldrtimes has columns for deployment season and year:
#   ldrtimes$deploy_season = "sum"
#   ldrtimes$deploy_year = "23"
#
# we can label the red lines on the plots if you like
#
# before running this script, update the file paths below
# - to toggle a line between commented (#...) and uncommented (...), use 
#   Shift-Control-C (Windows) or Shift-Command-C (Mac)
# - make sure to add a slash "/" at the end of each file path!
#

# load all the packages and functions you'll need
source("code/dataQCfunctions.R")
# if you'd like to have your computer make sound when the job is done,
# install and load this package
# install.packages("beepr") # uncomment and run this step only once, then comment it again
# library(beepr) # leave this uncommented

# base path: this is the shared part of the filepath to the directories
# containing your raw data files, cropped data files, and cropped data plots
# - if these folders are in the dataQCtools R project folder, you can use a relative path
# - otherwise you can make this an absolute path
# - you can also delete base_loc and paste0(base_loc, ...) and just explicitly 
#   specify rawdata_loc, cropped_loc, and croppedplots_loc (see make_qc_plots.R 
#   for an example of this)
# - make sure filepaths for folders always end in "/"!
base_loc <- "data/2022_summer/"

# directory containing raw data
rawdata_loc <- paste0(base_loc, "1_raw_csv/")
# directory where cropped data will be (or currently is) stored
# csv files in this directory are data files to be plotted!
cropped_loc <- paste0(base_loc, "2_cropped_csv/")
# directory where cropped vs raw plots will be stored
croppedplots_loc <- paste0(base_loc, "2_cropped_plots/")
# name of your LDRTimes file that has the lookup table with deployment/retrieval dates
# the code assumes this file is in rawdata_loc
ldrtimes_fn <- "LDRTimes_summer22.xlsx"

# check that R can find your raw data files
# Get all temperature data filenames
# note: * is called a glob, short for global
# IMPORTANT: filenames must be in the format sitename_medium_deployseason_deployyear.csv
#            e.g. NolanLower_air_sum_23.csv
csv_files = list.files(path = rawdata_loc, pattern = '*csv')
# this is now a list of all filenames; we haven't read in the data yet, but
# make sure this lists all the raw files you want to crop
csv_files

# read in LDR file and take a look at it
# note: this assumes the LDR file is in the folder indicated by rawdata_loc
ldrtimes = read_xlsx(paste0(rawdata_loc, ldrtimes_fn))

# once you're sure that file paths are working and your ldrtimes looks right,
# crop the files!
crop_raw_data(rawdata_loc, ldrtimes_fn, cropped_loc, croppedplots_loc)
# beep(8) # uncomment if you want to use beepr!
# system("say 'Attention all your data has been cropped'") # or on a Mac you can use this


