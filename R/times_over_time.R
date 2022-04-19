utils::globalVariables(c("olympic_marathon", "result", "year", "gender"))

## ggplot visualization of how the finishing times of any ranking (places 1-10) have evolved over time for a given gender

#' Shows finishing time data over time.
#'
#' Creates a ggplot visualization of all complete finishing times in \code{olympic_marathon}
#'
#' @return ggplot object
#'
#' @examples times_over_time()
#'
#' @export

times_over_time <- function() {
  olympic_marathon %>%
    dplyr::filter(!(result %in% c("DNF", "DNS", "DQ"))) %>%
    dplyr::mutate(result = hms::as_hms(result)) %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = result, color = gender)) +
    ggplot2::geom_jitter() +
    ggplot2::ggtitle("All Finishing Times By Olympic Year") +
    ggplot2::scale_y_time("Result (hr:min:sec)", limits = c(hms::as_hms("02:00:00"), hms::as_hms("04:25:00"))) +
    ggplot2::scale_x_discrete("Olympic Year") +
    ggplot2::theme_minimal()
}
