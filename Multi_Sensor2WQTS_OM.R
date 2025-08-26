##
#     Multi-sensor2WQTS
#This R project proposes to create data management pipeline from multiple 
# make and models of data logging data sondes for collecting water quality data
# and save the processed data to a SQL Server database called WQTS (water quality time series).
# The initial project includes code and sample data from a HOBO water temperature and
# dissolved oxygen sensor/logger. 
# The first sensor/logger is the Hobo® DO/ Temperature data loggers (Model U26-001)
# The plan is to add additional sensors.
# The WQTS database was developed and provide by the NW Indian Fisheries Commission
# University of Washington Department of Statistics students and facility contributed 
# to this project.


library(tidyverse)
library(lubridate)
library(janitor)
library(rstudioapi)
library(stringr)
library(rlang)

# Define sensor-specific column mappings
sensor_mappings <- list(
  "HOBO_U26-001_DO_Temp" = c('lnum', 'date_time', 'DO', 'temp_c', 'Attached', 'Stopped', 'End', 'filename'),
  "HOBO_U26-001_Temp_Crop" = c('lnum', 'date_time', 'temp_c')
)

# Embedded SQL Server schema (WQTS_Data)
sql_columns <- c(
  "TS_ID", "MONLOC_AB", "StartDate", "StartTime", "DataLoggerLineName",
  "Deployment", "ProjectIdentifier", "ActivityIdentifier", "ActivityMediaName",
  "CharacteristicName", "ResultMeasureValue", "ResultMeasureUnitCode",
  "DataType", "UseForCalc", "COMMENTS", "CreatedDateTime", "LastChangeDate"
)

# Prompt user to select sensor type
select_sensor_type <- function() {
  cat("Available sensor types:\n")
  sensor_names <- names(sensor_mappings)
  for (i in seq_along(sensor_names)) {
    cat(i, ": ", sensor_names[i], "\n")
  }
  choice <- as.integer(readline(prompt = "Select a sensor type by number: "))
  if (choice >= 1 && choice <= length(sensor_names)) {
    return(sensor_names[choice])
  } else {
    stop("Invalid selection.")
  }
}

# Cross-platform folder selection
select_data_folder <- function() {
  if (interactive() && rstudioapi::isAvailable()) {
    folder <- rstudioapi::selectDirectory()
    if (is.null(folder)) stop("No folder selected.")
    return(folder)
  } else {
    stop("Interactive folder selection requires RStudio.")
  }
}

# Extract serial number from column headers
extract_serial_number <- function(file_path) {
  header_lines <- readLines(file_path, n = 3)
  pattern <- "S/N:\\s*(\\d+)"
  matches <- str_match(header_lines[2], pattern)
  if (!is.na(matches[1, 2])) {
    return(matches[1, 2])
  } else {
    matches <- str_match(header_lines[3], pattern)
    if (!is.na(matches[1, 2])) {
      return(matches[1, 2])
    } else {
      warning("Serial number not found in header.")
      return(NA)
    }
  }
}

# Read and process files -- EDITED BY OM TO HANDLE CROPPED FILES
read_plus <- function(flnm, sensor_type) {
  snum <- extract_serial_number(flnm)  # will be NA for cropped files; that's OK
  
  # Cropped CSVs typically have NO header; raw HOBO exports do.
  if (sensor_type == "HOBO_U26-001_Temp_Crop") {
    df <- readr::read_csv(flnm, col_names = FALSE, skip=1, show_col_types = FALSE)
  } else {
    df <- readr::read_csv(flnm, col_names = FALSE, skip = 2, show_col_types = FALSE)
  }
  
  df %>%
    mutate(filename = basename(flnm), snum = snum)
}


# Main ingestion function -- EDITED BY OM TO HANDLE CROPPED FILES
ingest_sensor_data <- function() {
  sensor_type <- select_sensor_type()
  col_names <- sensor_mappings[[sensor_type]]
  folder <- select_data_folder()
  
  files <- list.files(path = folder, pattern = "*.csv", full.names = TRUE)
  tbl_with_sources <- purrr::map_dfr(files, read_plus, sensor_type = sensor_type)
  names(tbl_with_sources)[1:length(col_names)] <- col_names
  
  # Extract site, media, deployment year and season from filename
  # Step 1: Extract components from filename (OM does this with extract instead of separate)
  tbl_with_sources <- tbl_with_sources %>%
    tidyr::extract(
      col   = filename,
      into  = c("site","media","deployment_full"),
      regex = "^([^_]+)_([^_]+)_(.+)\\.[Cc][Ss][Vv]$",
      remove = FALSE
    ) %>%
    # Step 2: Parse date_time and create date and time columns (OM has it check the different orders to accomodate different encodings)
    mutate(
      date_time = if (!inherits(date_time, "POSIXt")) {
        parse_date_time(
          date_time,
          orders = c(
            "Y-m-d H:M:S","Y-m-d H:M",
            "m/d/Y H:M","m/d/y H:M", 
            "m/d/Y H:M:S","m/d/y H:M:S",
            "m/d/Y I:M p","m/d/y I:M p",
            "Y-m-d","m/d/Y","m/d/y"
          ),
          tz = "UTC"
        )
      } else {
        date_time
      },
      date = as.Date(date_time),
      time = format(date_time, "%H:%M:%S")
    ) %>%
    # Step 3: create new columns for season and year as encoded in filename
    mutate(
      deployment = deployment_full %>%
        str_remove("(?i)\\.csv$") %>%
        str_remove("_cropped$"),
      deployment_season = str_extract(deployment, "^[^_]+"),
      deployment_year   = coalesce(
        str_extract(deployment, "(?<=_)\\d{4}(?=(_|$))"),  # 2019
        str_extract(deployment, "(?<=_)\\d{2}(?=(_|$))")   # 15
      ),
      sn_file = str_remove(deployment_full, ".csv$")
    ) %>%
    # Step 4: add Celsius column
    mutate(temp_f = (temp_c * 9/5) + 32, .after = temp_c) %>%
    clean_names()
  
  # Construct SQL-ready dataframe
  has_do  <- "do" %in% names(tbl_with_sources)
  value   <- if (has_do) sym("do") else sym("temp_c")
  char    <- if (has_do) "Dissolved oxygen (DO)" else "Temperature, water"
  unit    <- if (has_do) "mg/l"                  else "deg C"
  aligned_data <- tbl_with_sources %>%
    transmute(
      TS_ID = NA_integer_,
      MONLOC_AB = site,
      StartDate = date,
      StartTime = time,
      DataLoggerLineName = snum,
      Deployment = deployment,
      ProjectIdentifier = "DungLowFlow",
      ActivityIdentifier = paste0(site, "_", format(date, "%Y%m%d"), "_", deployment, "_TS"),
      ActivityMediaName = str_to_title(media),
      CharacteristicName = char,
      ResultMeasureValue = !!value,
      ResultMeasureUnitCode = unit,
      DataType = "Raw",
      UseForCalc = 1L,
      COMMENTS = NA_character_,
      CreatedDateTime = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      LastChangeDate  = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  
  return(aligned_data)
}


# Example usage
# 1: selecting HOBO_U26-001_DO_Temp sensor type and a folder with Dungeness1_water_sum_2019.csv
WQTS_data_do <- ingest_sensor_data()
print(head(WQTS_data_do))
# selecting HOBO_U26-001_Temp_Crop sensor type and the cropped_data folder
WQTS_data_temp_cropped <- ingest_sensor_data()
print(head(WQTS_data_temp_cropped))

