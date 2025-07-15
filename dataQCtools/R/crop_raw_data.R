#' Crop raw water/air temperature data from HOBO sensors
#'
#' @param rawdata_loc File path to the directory containing the raw data to be
#' cropped. The names of the raw data files must be in the format
#' sitename_medium_deployseason_deployyear.csv, e.g. NolanLower_air_sum_23.csv.
#' @param ldrtimes_fn Filename for the LDRTimes file, the lookup table of
#' deployment and retrieval times. This file is assumed to be in the same folder
#' as the raw data.
#' @param cropped_loc File path to the directory where cropped data files will
#' be written and stored.
#' @param nfiles Number of files to process. Default value is `NULL`, in which
#' case all files in the raw data directory `rawdata_loc` are processed. If
#' `nfiles` is set to a positive integer such as 3, then only the first three
#' files in `rawdata_loc` will be cropped.
#'
#' @return
#' @export
#'
#' @examples
#'
#' # the following two lines of code are only necessary for the example data;
#' # for your own data, simply set base_loc to the base file path for your
#' # computer, e.g. base_loc = "data/2022_summer/"
#' example_loc = fs::path_package("extdata", package = "dataQCtools")
#' base_loc = paste0(example_loc, "/data/2022_summer/")
#' rawdata_loc = paste0(base_loc, "1_raw_csv/")
#' cropped_loc = base_loc
#' #cropped_loc = paste0(base_loc, "2_cropped_csv/")
#' #croppedplots_loc = paste0(base_loc, "2_cropped_plots/")
#' ldrtimes_fn = "LDRTimes_summer22.xlsx"

#' # check that R can find your raw data files
#' # Get a list of the filenames of all the raw data files you will be clipping
#' csv_files = list.files(path = rawdata_loc, pattern = '*csv')
#' # this is now a list of all filenames; we haven't read in the data yet, but
#' # make sure this lists all the raw files you want to crop:
#' csv_files
#'
#' # read in LDRTimes lookup table and take a look at it
#' ldrtimes = readxl::read_xlsx(paste0(rawdata_loc, ldrtimes_fn))
#' ldrtimes # or View(ldrtimes)
#'
#' # then crop the data!
#' crop_raw_data(rawdata_loc, ldrtimes_fn, cropped_loc)
#'
crop_raw_data = function(rawdata_loc, ldrtimes_fn, cropped_loc, nfiles = NULL){ #, croppedplots_loc){
  cropped_loc = paste0(cropped_loc, "2_cropped_csv/")
  croppedplots_loc = paste0(cropped_loc, "2_cropped_plots/")
  dir.create(cropped_loc, showWarnings = FALSE)
  dir.create(croppedplots_loc, showWarnings = FALSE)

  # get a list of all the temperature data filenames
  csv_files = list.files(path = rawdata_loc, pattern = '*csv')
  # read in LDR file
  # - this assumes the LDR file is an excel file in the folder indicated by rawdata_loc
  ldrtimes = readxl::read_xlsx(paste0(rawdata_loc, ldrtimes_fn))

  if(!is.null(nfiles)){csv_files = csv_files[1:nfiles]}

  i = 0
  for(this.file in csv_files){
    i = i + 1
    #this.file = csv_files[1] # uncomment to troubleshoot within loop
    cat(paste0("Reading file ", i, " of ", length(csv_files), ": ", this.file), fill = TRUE)

    # extract metadata from the filename
    filename.parts = stringr::str_split_1(this.file, '[_.]')
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
        readr::read_csv(paste0(rawdata_loc, this.file),
                 skip = 2, # skip the first two lines of the file
                 col_select = 1:3, # read only the first three columns of data
                 col_names = FALSE, # don't try to name columns from a row of the file
                 show_col_types = FALSE) %>% # suppresses print message
          dplyr::rename("row.num" = .data$X1,
                 "datetime" = .data$X2,
                 "temperature" = .data$X3) %>%
          dplyr::mutate(datetime = lubridate::mdy_hms(.data$datetime)) #for datetime in hh:mm:ss
      },
      warning = function(cond) { #if datetime isn't in hh:mm:ss, will now try hh:mm format
        readr::read_csv(paste0(rawdata_loc, this.file),
                 skip = 2, # skip the first two lines of the file
                 col_select = 1:3, # read only the first three columns of data
                 col_names = FALSE, # don't try to name columns from a row of the file
                 show_col_types = FALSE) %>% # suppresses print message
          dplyr::rename("row.num" = .data$X1,
                 "datetime" = .data$X2,
                 "temperature" = .data$X3) %>%
          dplyr::mutate(datetime = lubridate::mdy_hm(.data$datetime)) #for datetime in hh:mm
      }
    )

    # crop the data
    deploy.retrieval = ldrtimes %>%
      # select the row(s) of ldrtimes that match this datafile
      # should be exactly one row, but if there are no rows or multiple rows that
      # match, this step will pull that many rows
      dplyr::filter(.data$site == csv.site, .data$deploy_season == csv.season,
                    .data$deploy_year == csv.year, .data$media == csv.media) %>%
      # keep just the deploy_time and retrieval_time variables/columns
      dplyr::select(.data$deploy_time, .data$retrieval_time)

    if(nrow(deploy.retrieval) == 0){
      stop("no rows of ldrtimes matched this csv file.")
    }
    if(nrow(deploy.retrieval) > 1){
      stop("multiple rows of ldrtimes matched this csv file.")
    }

    deploy = deploy.retrieval$deploy_time
    retrieval = deploy.retrieval$retrieval_time

    if(retrieval > deploy) {
      cropped.data = dplyr::filter(this.data,
                                   .data$datetime > deploy,
                                   .data$datetime < retrieval)

    } # if(retrieval > deploy)

    # write cropped csv files to cropped folder
    readr::write_csv(cropped.data,
              file=paste0(cropped_loc,
                          stringr::str_split_i(this.file, "[.]", 1), "_cropped.csv"))

    #Create a dataframe of the raw and cropped data
    cropvraw <- dplyr::left_join(this.data, cropped.data, by=c("row.num", "datetime")) %>%
      dplyr::rename(raw.temp = .data$temperature.x,
                    cropped.temp = .data$temperature.y) %>%#rename temperature from each file
      #create new column of data type (raw or cropped for plotting in ggplot)
      tidyr::pivot_longer(cols = .data$raw.temp:.data$cropped.temp,
                          names_to="type", values_to="temp")

    cropvraw.plot <- ggplot2::ggplot(cropvraw,
                                     ggplot2::aes(x = .data$datetime,
                                                  y = .data$temp,
                                                  color = .data$type)) +
      ggplot2::geom_line(na.rm=TRUE) +
      ggplot2::geom_point(na.rm=TRUE) +
      ggplot2::labs(title = paste0(" Raw versus Cropped data"),
           x = "Date", y = "Temperature (C)")+
      ggplot2::theme(axis.text = ggplot2::element_text(colour = "black", size = (12)))

    ggplot2::ggsave(paste0(croppedplots_loc, csv.site, "_rawvscroppeddata.png"),
                    cropvraw.plot,
                    width = 11, height = 8.5, units = "in")
  }

  cat("Done.", fill = TRUE)
}
