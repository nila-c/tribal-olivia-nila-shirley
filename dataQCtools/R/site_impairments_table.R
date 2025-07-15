#' Create a table of site impairments
#'
#' Given quality-controlled data for a given water year, this function
#' identifies which sites are impaired and the dates of their exceedances.
#' A creek/site is impaired if its temperatures exceed water quality standards
#' (WQS) at least twice during the water year and there is a period of at least
#' seven days between two of those exceedances.
#'
#'
#'
#' @param wq_data Data set with exceedances, such as the output of compute_7DADM_and_exceedance().
#'
#' @return A table such as the following:
#'
#' | Site    | Impaired | Dates of impairment |
#' | ------- | -------- | ------------------- |
#' | Site 1  |  No      |                     |
#' | Site 2  |  Yes     | 3/17-3/19, 5/21     |
#' | Site 3  |  Yes     | 10/20, 4/28-4/29    |
#' | Site 4  |  No      |                     |
#' | Site 5  |  No      |                     |
#'
#' @export
#'
#' @examples
site_impairments_table = function(wq_data){
  site_impairments = data.frame(
    Site = character(),
    Impaired = character(),
    Dates = character())

  cat(unique(wq_data$SiteName), fill = TRUE)

  # format Dates to have date ranges for consecutive dates,
  # e.g. 7/18-7/20 instead of 7-18, 7-19, 7-20
  for(site in unique(wq_data$SiteName)){
    # cat(site, fill = TRUE)
    exceedance_dates = dplyr::filter(wq_data, .data$SiteName == site, .data$Exceedance)$Date
    diffs = diff(exceedance_dates)
    if(length(diffs) >=1 & sum(diffs>7) > 0){
      # cat(site, fill = TRUE)
      consec_dates = split(format(exceedance_dates, "%m/%d"),
                           cumsum(c(1, diff(exceedance_dates) != 1)))
      date_list = sapply(consec_dates, function(x) paste(x[1], tail(x, n=1), sep = "-"))
      site_impairments = dplyr::add_row(site_impairments,
                                        Site = site,
                                        Impaired = "Yes",
                                        Dates = paste(date_list, collapse = ", "))
    } else {
      # cat(site, fill = TRUE)
      site_impairments = dplyr::add_row(site_impairments,
                                        Site = site,
                                        Impaired = "No",
                                        Dates = NA)
    }
  }

  return(site_impairments)
}
