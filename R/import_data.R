#' Imports data for women's marathon from 2020!
#' @export
#' @import pdftools
#' @import tidyverse

import_womens_data_2020 <- function() {

  ## ## pdf_text from https://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/
  original_text <- pdftools::pdf_text("womens_olympic_marathon.pdf")
  ## idea from: https://www.charlesbordet.com/en/extract-pdf/#use-pdftoolspdf_text
  split_text <- original_text %>%
    strsplit("\n")

  ## combine lists from https://statisticsglobe.com/combine-lists-in-r
  full_data <- c(split_text[[1]][21:66], split_text[[2]][17:58])

  full_data <- as.character(full_data)

  ## gsub() from https://stackoverflow.com/questions/25707647/merge-multiple-spaces-to-single-space-remove-trailing-leading-spaces
  ## specific syntax from https://stackoverflow.com/questions/65033661/remove-multiple-spaces-but-leave-single-space-in-r
  full_data_no_extra_spaces <- gsub('\\s{2,}','  ',full_data)


  full_data_as_tibble <- tibble::as_tibble(full_data_no_extra_spaces)

  full_data_as_tibble_separated <- full_data_as_tibble %>%
    tidyr::separate(value, into = c("space", "rank", "bib_last_first", "nationality", "date", "result", "5"), sep = "  ") %>%
    dplyr::select(-space, -`5`) %>%
    dplyr::mutate(result = ifelse(is.na(result) == TRUE, "DNF", result)) %>%
    dplyr::mutate(date = ifelse(result == "DNF", nationality, date)) %>%
    dplyr::mutate(nationality = ifelse(is.na(as.numeric(rank)) == TRUE, bib_last_first, nationality)) %>%
    dplyr::mutate(bib_last_first = ifelse(is.na(as.numeric(rank)) == TRUE, rank, bib_last_first)) %>%
    dplyr::mutate(rank = ifelse(is.na(as.numeric(rank)) == TRUE, NA, rank)) %>%
    dplyr::mutate(gender = "W") %>%
    dplyr::mutate(event = "Marathon Women") %>%
    dplyr::mutate(location = "Tokyo") %>%
    dplyr::mutate(year = "2020") %>%
    dplyr::mutate(medal = ifelse(rank == "1", "G",
                          ifelse(rank == "2", "S",
                                 ifelse(rank == "3", "B", NA)))) %>%
    dplyr::select(-date)

  ## removing bib number from https://stackoverflow.com/questions/13590139/remove-numbers-from-alphanumeric-characters

  full_data_as_tibble_separated$bib_last_first <- gsub('[[:digit:]]+', '', full_data_as_tibble_separated$bib_last_first)

  ## trimws from https://stat.ethz.ch/R-manual/R-patched/library/base/html/trimws.html

  full_data_as_tibble_separated$bib_last_first <- full_data_as_tibble_separated$bib_last_first %>%
    trimws("left")

  full_data_as_tibble_separated_n32 <- full_data_as_tibble_separated %>%
    dplyr::rename(name = bib_last_first)

  return(full_data_as_tibble_separated_n32)
}
