#' Compute 7DADM
#'
#' For each period of seven consecutive days, this function computes the daily
#' max water temperature for each day, then computes the average of those daily
#' max temperatures. This implementation assumes the data has already been
#' quality-controlled to remove or impute data during time periods in which the
#' logger was dewatered, buried, or nonfunctional.
#'
#' @param all_data An R data frame object with the data to be used in computing
#' 7DADM. In compute_7DADM_and_exceedance(), this data frame is the output of
#' read_in_qcd_data().
#'
#' @return A data frame with the following four columns:
#'
#'  - SiteName
#'  - Date
#'  - DailyMax, daily maximum temperature
#'  - sevenDADM, the newly calculated 7DADM values
#' @export
#'
#' @examples
compute_7DADM = function(all_data){
  cat("Computing 7DADM...", fill = TRUE)
  temp_stats = all_data %>%
    # compute daily max water temp by site and date
    dplyr::group_by(.data$SiteName, .data$Date)  %>%
    dplyr::summarize(DailyMax = max(.data$WaterTemp, na.rm = TRUE), .groups = 'drop') %>%
    # add in any missing SiteName-Date combinations so we can tell which data is
    # for consecutive dates
    dplyr::right_join(data.frame(SiteName = rep(unique(all_data$SiteName),
                                         each = as.integer(max(all_data$Date)-min(all_data$Date))+1),
                          Date = rep(seq(min(all_data$Date), max(all_data$Date), 1),
                                     dplyr::n_distinct(all_data$SiteName))),
                      by = dplyr::join_by(.data$SiteName, .data$Date)) %>%
    # sort by SiteName first, then by Date within SiteName
    dplyr::arrange(.data$SiteName, .data$Date) %>%
    # compute 7DADM
    dplyr::group_by(.data$SiteName) %>%
    dplyr::mutate(sevenDADM = runner::mean_run(x = .data$DailyMax,
                                       k = 7, lag = -3,
                                       idx = .data$Date,
                                       na_pad = TRUE, na_rm = FALSE)) %>%
    dplyr::ungroup()

  cat("Done computing 7DADM.", fill = TRUE)
  return(temp_stats)
}
