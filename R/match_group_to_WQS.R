#' Match groups of sites to their corresponding water quality standards
#'
#' Use group-WQS lookup table to determine what the WQS is for each group over
#' the course of the water year.
#'
#' @param wq_data Data frame
#' @param group_wqs Group-WQS lookup table as an R data.frame object
#'
#' @return Data frame with water quality standards and exceedances
#' @export
#'
#' @examples
match_group_to_WQS = function(wq_data, group_wqs){
  wq_data = wq_data %>%
    dplyr::mutate(Day = format(base::as.Date(.data$Date), "%m/%d")) %>%
    dplyr::left_join(group_wqs, by = "grp_type", relationship = "many-to-many") %>%
    dplyr::mutate(dates_match = (.data$Day > .data$start_date & .data$Day < .data$end_date)) %>%
    dplyr::filter(.data$dates_match) %>%
    dplyr::select(.data$SiteName, .data$Date, .data$Day, .data$sevenDADM, .data$WQS) %>%
    dplyr::mutate(Exceedance = (.data$sevenDADM > .data$WQS))
  return(wq_data)
}
