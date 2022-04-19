utils::globalVariables(c("olympic_marathon", "result", "nationality", "gender", "rank", "year", "country_gender"))

#' Visualization of finishing times from two countries
#'
#' Creates a ggplot of the finishing times of all competitors from two input countries (excluding DNF)
#'
#' @param nationality1 3 character indicator for first nationality
#' @param nationality2 3 character indicator for second nationality
#'
#' @return ggplot object
#'
#' @examples
#' two_country_viz("ITA", "NOR")
#'
#' @export

two_country_viz <- function(nationality1, nationality2) {
  # Stop and print error messages if problem with inputs
  if (class(nationality1) != "character")
    stop("nationality1 input value must be of class character. ", nationality1, " has class ", class(nationality1), ".")
  if (class(nationality2) != "character")
    stop("nationality2 input value must be of class character. ", nationality2, " has class ", class(nationality2), ".")
  if(nationality1 %in% unlist(olympic_marathon %>% dplyr::select(nationality)) == FALSE)
    stop("nationality ", nationality1, " not found in data.")
  if(nationality2 %in% unlist(olympic_marathon %>% dplyr::select(nationality)) == FALSE)
    stop("nationality ", nationality2, " not found in data.")

  data_cpy <- olympic_marathon
  data_cpy <- data_cpy %>%
    dplyr::filter(nationality == nationality1 | nationality == nationality2) %>%
    dplyr::filter(rank != "NA") %>%
    dplyr::mutate(result = hms::as_hms(result)) %>%
    dplyr::mutate(country_gender = ifelse(nationality == nationality1 & gender == "F",
                                   paste0(nationality1, " Women"), ifelse(nationality ==
                                                                            nationality2 & gender == "F", paste0(nationality2, " Women"),
                                                                          ifelse(nationality == nationality1 &
                                                                                   gender == "M", paste0(nationality1, " Men"), paste0(nationality2, " Men"))))) %>%
    dplyr::mutate(country_gender = factor(country_gender,
                                   levels = c(paste0(nationality1, " Men"), paste0(nationality2, " Men"), paste0(nationality1, " Women"), paste0(nationality2, " Women")))) %>%
    dplyr::group_by(year)

  ggplot2::ggplot(data = data_cpy, ggplot2::aes(x = year, y = result, col = country_gender)) +
    ggplot2::geom_point() +
    ggplot2::labs(title = paste0("Result vs. Year by Gender for ", nationality1, " and ", nationality2), x = "Year", y = "Result", col = "Nationality/Gender") +
    ggplot2::scale_color_manual(values=c("#7AFF6E", "#0A7D00", "#D26EFF", "#5A0083"))
}
