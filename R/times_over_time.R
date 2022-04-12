## ggplot visualization of how the finishing times of any ranking (places 1-10) have evolved over time for a given gender

#' @export

times_over_time <- function(rank_choice, ...) {
  olympic_marathon %>%
    dplyr::filter(rank == rank_choice) %>%
    dplyr::mutate(result = hms::as_hms(result)) %>%
    ggplot2::ggplot(aes(x = year, y = result, color = gender)) +
    ggplot2::geom_point() +
    ggplot2::scale_y_time(limits = c(hms::as_hms("00:00:00"), hms::as_hms("02:30:00"))) +
    ggplot2::theme_minimal()
}
