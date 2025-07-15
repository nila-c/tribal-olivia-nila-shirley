#' Read in quality-controlled data for all sites and deployments for a given water year
#'
#' This function is called by compute_7DADM_and_exceedance() to read in and
#' combine all the quality-controlled data into a single R data frame object
#' before computing 7DADM.
#'
#' @param fall_data_loc File path to directory containing fall deployment data
#' files; for water year 2021-2022, the fall deployment season is Fall 2021.
#' Files must be csv format.
#' @param sum_data_loc File path to directory containing summer deployment data
#' files; for water year 2021-2022, the summer deployment season is Summer 2022.
#' Files must be csv format.
#'
#' @return A single data frame object containing all the data from fall_data_loc
#' and sum_data_loc
#' @export
#'
#' @examples
read_in_qcd_data = function(fall_data_loc, sum_data_loc){
  cat("Reading in QC'd data...", fill = TRUE)
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
    this_data = readr::read_csv(file = paste0(file_dir, this_file),
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
      dplyr::rename(
        StartDate = .data$X2,
        StartTime = .data$X3,
        WaterTemp = .data$X10,
        UseForCalc = .data$X13) %>%
      dplyr::mutate(
        StartDate = lubridate::mdy(.data$StartDate),
        StartTime = as.character(.data$StartTime),
        SiteName = sitename,
        DeploySeason = deploy_season) %>%
      dplyr::mutate(datetime = as.POSIXct(
        paste(.data$StartDate, .data$StartTime, " "),
        format = "%Y-%m-%d %H:%M:%S",
        tz = "America/Los_Angeles")) %>%
      dplyr::mutate(Date = base::as.Date(.data$datetime, format = "%Y-%m-%d")) %>%
      # keep only the data with UseForCalc = 1 (data that passed QC)
      dplyr::filter(.data$UseForCalc == 1)

    # add this file's data to the combined data frame
    all_data <- dplyr::bind_rows(all_data, this_data)
    # print some basic info to the R Console to update us on what was read in
    cat(paste0(" - Site ", sitename, " Deployment ", deploy_season,
               " Dates ", min(this_data$Date), " to ", max(this_data$Date)),
        fill = TRUE)
  } # end for-loop
  cat("Done reading in QC'd data.", fill = TRUE)
  return(all_data)
}
