#' Match individual sites to water quality standards
#'
#' Use a site-group lookup table to determine which WQS group each site is in.
#'
#' @param wq_data Data frame
#' @param site_group Site-group lookup table as an R data.frame object
#'
#' @return Data frame with site_group lookup table information
#' @export
#'
#' @examples
match_sites_to_WQS_groups = function(wq_data, site_group){
  return(dplyr::left_join(wq_data, site_group, by = dplyr::join_by(.data$SiteName == .data$site)))
}
