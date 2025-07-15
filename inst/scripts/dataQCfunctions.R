# functions for 7DADM and impairment

# load necessary libraries
library(zoo)
library(grid)
library(plotly)
library(readxl)
library(scales)
library(runner) # for moving averages
library(ggthemes) # for plotting
library(tidyverse) # need for dplyr and ggplot2
library(lubridate) # for handling dates
library(gridExtra)
library(flextable) # for making tables with conditional formatting, merged cells and more
library(RColorBrewer)

# Read in data for all sites and deployments for a given water year 
read_in_qcd_data = function(fall_data_loc, sum_data_loc){
  print("Reading in QC'd data...")
  # assumes all data files are csv format, not xls or xlsx
  # read in fall deployment files
  # fall filename format: "AndersonLower_2022_fall_FINAL.csv"
  data_files_fall = list.files(path = fall_data_loc, pattern = '*csv')
  # read in summer deployment files
  # summer filename format: "AndersonLower_2023_sum_FINAL.csv"
  data_files_sum = list.files(path = sum_data_loc, pattern = '*csv')
  # combine fall and summer filenames into one list so we just need one loop
  data_files = c(data_files_fall, data_files_sum)
  
  # loop through each file, read it in, process it
  all_data <-NULL # Create new data frame to hold combined data
  
  file_no = 0
  for(this_file in data_files){ # for each file,
    # update file_no counter
    file_no = file_no + 1
    #this_file = data_files[51] # uncomment to troubleshoot within loop
    
    # extract metadata (sitename and deployment season) from filename
    filename_parts = strsplit(this_file, '[_.]')[[1]]
    sitename = filename_parts[1]
    deploy_season = paste(filename_parts[3], filename_parts[2])
    
    # all the fall files are first in the list, so we can use this to tell
    # from file_no whether the current file is a summer or fall deployment
    # and that tells us in which directory we can find this file
    if(file_no > length(data_files_fall)){
      file_dir = sum_data_loc
    }else{
      file_dir = fall_data_loc
    }
    
    # read in the data (this_file) from the right directory (file_dir)
    this_data = read_csv(file = paste0(file_dir, this_file),
                         skip = 1, # skip header line
                         # the only columns we really need are 2, 3, 10, and 13,
                         # (date, time, water temp, and the QC indicator UseForCalc)
                         # but we can keep them all for reference
                         col_select = c(2, 3, 10, 13), # read only the four columns of data we need
                         col_names = FALSE, # don't try to name columns from a row of the file
                         show_col_types = FALSE) %>% # suppresses print message
      data.frame() %>% # get rid of annoying attribute information
      # the next several lines
      # convert the character-format datetime to an R POSIXct object
      # ymd_hms is the format the character string is in initially; it tells R
      # how to read and interpret the character string
      rename(StartDate = X2,
             StartTime = X3, 
             WaterTemp = X10, 
             UseForCalc = X13) %>%
      mutate(StartDate = mdy(StartDate),
             StartTime = as.character(StartTime),
             SiteName = sitename,
             DeploySeason = deploy_season) %>%
      mutate(datetime = as.POSIXct(paste(StartDate, StartTime, " "), 
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz = "America/Los_Angeles")) %>%
      mutate(Date = as.Date(datetime, format= "%Y-%m-%d")) %>%
      # keep only the data with UseForCalc = 1 (data that passed QC)
      filter(UseForCalc == 1)
    
    # add this file's data to the combined data frame
    all_data <- bind_rows(all_data, this_data)
    # print some basic info to the R Console to update us on what was read in
    print(paste0(" - Site ", sitename, " Deployment ", deploy_season,
                 " Dates ", min(this_data$Date), " to ", max(this_data$Date)))
  } # end for-loop
  print("Done reading in QC'd data.")
  return(all_data)
}

# Compute 7DADM: for each period of seven consecutive days,
# compute the daily max water temperature for each day, then
# compute the average of those daily max temperatures
# - Exclude time periods in which the logger was dewatered, buried, or 
#   nonfunctional: filter for records with UseForCalc = 1
# - Account for gaps in time
compute_7DADM = function(all_data){
  print("Computing 7DADM...")
  temp_stats = all_data %>%
    # compute daily max water temp by site and date
    group_by(SiteName, Date)  %>%
    # summarize(DailyMax = max(WaterTemp, na.rm = TRUE),
    #           DailyMin = min(WaterTemp, na.rm = TRUE),
    #           DailyMean = mean(WaterTemp, na.rm = TRUE)) %>%
    summarize(DailyMax = max(WaterTemp, na.rm = TRUE)) %>%
    # summarize(DailyMax = max(WaterTemp, na.rm = TRUE), .groups = "drop_last") %>%
    ungroup() %>%
    # add in any missing SiteName-Date combinations so we can tell which data is
    # for consecutive dates
    right_join(data.frame(SiteName = rep(unique(all_data$SiteName),
                                         each = as.integer(max(all_data$Date)-min(all_data$Date))+1),
                          Date = rep(seq(min(all_data$Date), max(all_data$Date), 1),
                                     n_distinct(all_data$SiteName)))) %>%
    # sort by SiteName first, then by Date within SiteName
    arrange(SiteName, Date) %>%
    # compute 7DADM
    group_by(SiteName) %>%
    mutate(sevenDADM = mean_run(x = DailyMax, k = 7, lag = -3, idx = Date,
                                na_pad = TRUE, na_rm = FALSE)) %>%
    ungroup()
  
  print("Done computing 7DADM.")
  return(temp_stats)
}

# use a site-group lookup table to determine which WQS group each site is in
match_sites_to_WQS_groups = function(wq_data, site_group){
  return(left_join(wq_data, site_group, by = join_by(SiteName == site)))
}

# use group-WQS lookup table to determine what the WQS is for each group 
# over the course of the water year
match_group_to_WQS = function(wq_data, group_wqs){
  wq_data = wq_data %>%
    mutate(Day = format(as.Date(Date), "%m/%d")) %>%
    left_join(group_wqs, by = "grp_type", relationship = "many-to-many") %>%
    mutate(dates_match = (Day > start_date & Day < end_date)) %>%
    filter(dates_match) %>%
    select(SiteName, Date, Day, sevenDADM, WQS) %>%
    mutate(Exceedance = (sevenDADM > WQS))
  return(wq_data)
}

write_group_wqs = function(outdir){
  # define group_wqs
  group_wqs = data.frame(
    grp_type = c("core_sal",
                 rep("core_sal_supp1", 3),
                 rep("core_sal_supp2", 3),
                 "char"),
    start_date = c("01/01",
                   "01/01", "07/02", "09/01",
                   "01/01", "02/15", "07/02",
                   "01/01"),
    end_date =   c("12/31",
                   "07/01", "08/31", "12/31",
                   "02/14", "07/01", "12/31",
                   "12/31"),
    WQS = c(16,
            13, 16, 13,
            16, 13, 16,
            12)
  )
  
  # write to csv file
  write_csv(group_wqs, paste0(outdir, file = "group_wqs.csv"))
}

site_impairments_table = function(wq_data){
  site_impairments = data.frame(
    Site = character(),
    Impaired = character(),
    Dates = character())
  
  print(unique(wq_data$SiteName))
  
  # format Dates to have date ranges for consecutive dates, 
  # e.g. 7/18-7/20 instead of 7-18, 7-19, 7-20
  for(site in unique(wq_data$SiteName)){
    exceedance_dates = filter(wq_data, SiteName == site, Exceedance)$Date
    diffs = diff(exceedance_dates)
    print(site)
    if(length(diffs) >=1 & sum(diffs>7) > 0){
      print(site)
      consec_dates = split(format(exceedance_dates, "%m/%d"), 
                           cumsum(c(1,diff(exceedance_dates)!=1)))
      date_list = sapply(consec_dates, function(x) paste(x[1], tail(x, n=1), sep = "-"))
      site_impairments = add_row(site_impairments, 
                                 Site = site,
                                 Impaired = "Yes",
                                 Dates = paste(date_list, collapse = ", "))
    } else {
      print(site)
      site_impairments = add_row(site_impairments, 
                                 Site = site,
                                 Impaired = "No",
                                 Dates = NA)
    }
  }
  
  return(site_impairments)
}

# IMPORTANT: filenames must have format sitename_medium_deployseason_deployyear.csv
#            e.g. NolanLower_air_sum_23.csv
# Note: 
# if you get warning messages like the following, it's okay; it is just telling 
# you how many missing values you have
#1: Removed 189 rows containing missing values or values outside the scale range (`geom_line()`).
#2: Removed 189 rows containing missing values or values outside the scale range (`geom_point()`)
crop_raw_data = function(rawdata_loc, ldrtimes_fn, cropped_loc, croppedplots_loc){
  # get a list of all the temperature data filenames
  csv_files = list.files(path = rawdata_loc, pattern = '*csv')
  # read in LDR file
  # - this assumes the LDR file is an excel file in the folder indicated by rawdata_loc
  ldrtimes = read_xlsx(paste0(rawdata_loc, ldrtimes_fn))
  
  i = 0
  for(this.file in csv_files){
    i = i + 1
    #this.file = csv_files[1] # uncomment to troubleshoot within loop
    cat(paste0("Reading file ", i, " of ", length(csv_files), ": ", this.file), fill = TRUE)
    
    # extract metadata from the filename
    filename.parts = strsplit(this.file, '[_.]')[[1]]
    csv.site = filename.parts[1]
    csv.media = filename.parts[2]
    csv.season = filename.parts[3]
    csv.year = filename.parts[4]
    
    # convert the character-format datetime to an R POSIXct object
    # ymd_hm is the format the character string is in initially; it tells R
    # how to read and interpret the character string
    # sometimes R reads in the datetime format as mdy_hms and sometimes mdy_hm. 
    # This tryCatch handles either hh:mm:ss or hh:mm format in csv files
    this.data =  tryCatch(
      { 
        read_csv(paste0(rawdata_loc, this.file),
                 skip = 2, # skip the first two lines of the file
                 col_select = 1:3, # read only the first three columns of data
                 col_names = FALSE, # don't try to name columns from a row of the file
                 show_col_types = FALSE) %>% # suppresses print message
          rename("row.num" = X1,
                 "datetime" = X2,
                 "temperature" = X3) %>%
          mutate(datetime = mdy_hms(datetime)) #for datetime in hh:mm:ss
      }, 
      warning = function(cond) { #if datetime isn't in hh:mm:ss, will now try hh:mm format
        read_csv(paste0(rawdata_loc, this.file),
                 skip = 2, # skip the first two lines of the file
                 col_select = 1:3, # read only the first three columns of data
                 col_names = FALSE, # don't try to name columns from a row of the file
                 show_col_types = FALSE) %>% # suppresses print message
          rename("row.num" = X1,
                 "datetime" = X2,
                 "temperature" = X3) %>%
          mutate(datetime = mdy_hm(datetime)) #for datetime in hh:mm
      }
    )
    
    # crop the data
    deploy.retrieval = ldrtimes %>%
      # select the row(s) of ldrtimes that match this datafile
      # should be exactly one row, but if there are no rows or multiple rows that
      # match, this step will pull that many rows
      filter(site == csv.site, deploy_season == csv.season,
             deploy_year == csv.year, media == csv.media) %>%
      # keep just the deploy_time and retrieval_time variables/columns
      select(deploy_time, retrieval_time)
    
    if(nrow(deploy.retrieval) == 0){
      stop("no rows of ldrtimes matched this csv file.")
    }
    if(nrow(deploy.retrieval) > 1){
      stop("multiple rows of ldrtimes matched this csv file.")
    }
    
    deploy = deploy.retrieval$deploy_time
    retrieval = deploy.retrieval$retrieval_time
    
    if(retrieval > deploy) {
      cropped.data = filter(this.data,
                            datetime > deploy,
                            datetime < retrieval)
      
    } # if(retrieval > deploy)
    
    # write cropped csv files to cropped folder
    write_csv(cropped.data,
              file=paste0(cropped_loc, str_split_i(this.file,"[.]",1),"_cropped.csv"))
    
    #Create a dataframe of the raw and cropped data
    cropvraw <- left_join(this.data, cropped.data, by=c("row.num", "datetime")) %>%
      rename(raw.temp=temperature.x, cropped.temp =temperature.y) %>%#rename temperature from each file 
      #create new column of data type (raw or cropped for plotting in ggplot)
      pivot_longer(cols=raw.temp:cropped.temp, names_to="type", values_to="temp")
    
    cropvraw.plot <- ggplot(cropvraw, aes(x=datetime, y=temp, color=type)) +
      geom_line() +
      geom_point() +
      labs(title = paste0(" Raw versus Cropped data"),
           x = "Date", y = "Temperature (C)")+
      theme(axis.text = element_text(colour = "black", size = (12)))
    
    ggsave(paste0(croppedplots_loc, csv.site, "_rawvscroppeddata.png"), cropvraw.plot,
           width = 11, height = 8.5, units = "in")
    
  }
  
  cat("Done.", fill = TRUE)
}

make_qc_plots_of_cropped_data = function(cropped_loc, qc_plots_loc){
  # IMPORTANT: files must be csv format, and filenames must start with
  #            <sitename>_<medium>_<deployseason>_<deployyear>
  #            e.g. the beginning of the filename must be NolanLower_air_sum_23 
  #            for air data from site NolanLower with deployment date Summer 2023
  
  # Make list of file names to plot
  filenames <- list.files(path = cropped_loc, pattern=".csv")
  
  # Loop to read all the data files and combine them into one datafile for easier plotting
  Combined <-NULL # Create new data frame to hold combined data
  
  print("Reading in cropped data files...")
  i = 0
  for (selection in filenames) {
    i = i+1
    #selection = filenames[1] # for troubleshooting the for-loop
    cat(paste0("Reading file ", i, " of ", length(filenames), ": ", selection), fill = TRUE)
    # split the filename string everywhere there is an underscore or period
    # so that we can get the following metadata from it:
    #  site name, deployment season and year, and media (air/water)
    info_from_filename = unlist(strsplit(selection, split="[_.]"))[1:4]
    sitename = info_from_filename[1]
    media = info_from_filename[2] # air or water; assumes this info is the third chunk after splitting the filename
    deploy_season = info_from_filename[3]
    if(deploy_season != "sum" & deploy_season != "fall"){
      stop("filename does not say sum or fall.")
    }
    deploy_year = 2000 + as.integer(info_from_filename[4]) # or we can format as character
    
    oneread <- read.csv(
      file=paste0(cropped_loc, selection), as.is=T, skip=1, fill=T, header=F
      ) %>% ## Reads the selected datafile.
      select(1:3) %>% #select the first 3 columns (remove air temp from Hoh River sites)
      # add the metadata as variables/columns
      mutate(sitename=sitename,
             media=media,
             deploy_season=deploy_season,
             deploy_year=deploy_year)
    Combined <- bind_rows(Combined, oneread)  ## Adds the datafile's data to the existing combined datafile.
    
  } # filenames loop  
  cat("Done reading in cropped data.", fill = TRUE)
  
  cat("Computing temperature differences...", fill = TRUE)
  # only compute AWMaxDiff and AirRange if there is air data for at least one site
  if("air" %in% unique(Combined$media)){
    Combined <- Combined %>%
      rename(row=V1, datetime=V2, temp=V3) %>%  # Rename the first three variables that came from the csv file
      mutate(datetime = ymd_hms(datetime)) %>%  # change format of datetime column
      mutate(date = date(datetime)) %>%
      group_by(sitename, date, media)  %>%  # compute daily max/min by site, date, and media (air/water)
      summarize(dailymax = max(temp, na.rm = TRUE),
                dailymin = min(temp, na.rm = TRUE)) %>%
      # pivot dataframe to add air or water to column name; this is necessary to calculate stats
      # this puts air and water on the same rows so we can just subtract columns in the next line
      # this requires that at least one site has air temperature!
      pivot_wider(names_from = media, values_from = c(dailymin, dailymax)) %>% 
      mutate(AWMaxDiff = dailymax_air - dailymax_water,
             AirRange = dailymax_air - dailymin_air,
             WaterRange = dailymax_water - dailymin_water) %>%
      #pivot longer for plotting purposes later; to plot on same graph, need "calc" column ("grouping variable") 
      pivot_longer(cols = dailymin_air:WaterRange, names_to = "calc", values_to = "value")
  }else{
    Combined <- Combined %>%
      rename(row=V1, datetime=V2, temp=V3) %>%  # Rename the first three variables that came from the csv file
      mutate(datetime = ymd_hms(datetime)) %>%  # change format of datetime column
      mutate(date = date(datetime)) %>% # extract just the date
      group_by(sitename, date, media)  %>%  # compute daily max/min by site, date, and media (air/water)
      summarize(dailymax = max(temp, na.rm = TRUE),
                dailymin = min(temp, na.rm = TRUE)) %>%
      # pivot dataframe to add air or water to column name; this is necessary to calculate stats
      # this puts air and water on the same rows so we can just subtract columns in the next line
      # this requires that at least one site has air temperature!
      pivot_wider(names_from = media, values_from = c(dailymin, dailymax)) %>% 
      mutate(WaterRange = dailymax_water - dailymin_water) %>%
      #pivot longer for plotting purposes later; to plot on same graph, need "calc" column ("grouping variable") 
      pivot_longer(cols = dailymin_water:WaterRange, names_to = "calc", values_to = "value")
  }
  
  
  
  # set some plotting parameters
  sites <- unique(Combined$sitename)
  range.colors <- c(AirRange = "blue", WaterRange = "black")
  maxdiff.colors <- c(dailymax_air = "blue", dailymax_water = "black", AWMaxDiff = "purple")
  
  # loop through sites to plot all graphs for all sites
  i = 0
  for(s in sites){
    i = i+1
    cat(paste0("Making QC plots for site ", i, " of ", length(sites), ": ", s), fill = TRUE)
    # s = sites[1] # uncomment if you want to troubleshoot this loop
    # filter the data to just this site
    this.site.combined = filter(Combined, sitename == s)
    rangeplot <- ggplot(this.site.combined %>% filter(calc %in% c("AirRange","WaterRange")),
                        aes(x = date, y = value, color = calc)) +
      geom_line() +
      geom_point() +
      labs(title = paste0(s," Air and Water Temperature ranges"),
           x = "Date", y = "Temperature (C)",
           color = "Media") +
      scale_color_manual(values = range.colors) +   
      geom_hline(yintercept = 3, linewidth = 0.3, color = "red") 
    
    ggsave(paste0(qc_plots_loc, s, "_AirWaterTempRange.png"), rangeplot,
           width = 11, height = 8.5, units = "in")
    
    maxdiffplot <- ggplot(this.site.combined %>%
                            filter(calc %in% c("dailymax_air", "dailymax_water", "AWMaxDiff")),
                          aes(x = date, y = value, color = calc)) +
      geom_line() +
      labs(title = paste0(s," Max Air and Water Temperature and Difference"),
           x = "Date", y = "Temperature (C)",
           color = "Media") +
      scale_color_manual(values = maxdiff.colors) +   
      geom_hline(yintercept = 20, linewidth = 0.3, color = "red") 
    
    ggsave(paste0(qc_plots_loc, s, "_MaxDiffAirWaterTemp.png"),
           maxdiffplot, width = 11, height = 8.5, units = "in")
    
    maxdifplotly<-ggplotly(maxdiffplot)#to create plotly of maxdifplot to trace plot
    rangeplotly<-ggplotly(rangeplot)#to create plotly of rangeplot to trace plot
    
    htmlwidgets::saveWidget(maxdifplotly, paste0(qc_plots_loc, s, "_MaxDiffAirWaterTemp.html"))
    htmlwidgets::saveWidget(rangeplotly, paste0(qc_plots_loc, s, "_AirWaterTempRange.html"))
  }
  cat("Done.", fill = TRUE)
}





