#' Imports data for women's marathon from the 2020 Tokyo Olympics!
#' @return a tibble with the ranked list of women's Olympic marathon runners from the 2020 Tokyo Olympics and associated times
#' @examples
#' import_womens_data_2020()
#' @export
#' @import pdftools
#' @import dplyr
#' @import tibble
#' @import magrittr
#' @import tidyr

import_womens_data_2020 <- function() {

  ## Pulls in PDF
  ## pdf_text from https://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/
  original_text <- pdftools::pdf_text("womens_olympic_marathon.pdf")

  ## Splits the PDF by new line
  ## idea from: https://www.charlesbordet.com/en/extract-pdf/#use-pdftoolspdf_text
  split_text <- original_text %>%
    strsplit("\n")

  ## Combines the two pieces of the data, which were on different lines
  ## combine lists from https://statisticsglobe.com/combine-lists-in-r
  full_data <- c(split_text[[1]][21:66], split_text[[2]][17:58])

  ## Turns the list data into a character vector
  full_data <- as.character(full_data)

  ## Makes all of the large series of spaces into just double spaces (so that they can be removed later, but that they are distinguishable from single spaces)
  ## gsub() from https://stackoverflow.com/questions/25707647/merge-multiple-spaces-to-single-space-remove-trailing-leading-spaces
  ## specific syntax from https://stackoverflow.com/questions/65033661/remove-multiple-spaces-but-leave-single-space-in-r
  full_data_no_extra_spaces <- gsub('\\s{2,}','  ',full_data)

  ## Converts the data into a Tibble
  full_data_as_tibble <- tibble::as_tibble(full_data_no_extra_spaces)

  full_data_as_tibble_separated <- full_data_as_tibble %>%

    ## Separates the mass of text into columns
    tidyr::separate(value, into = c("space", "rank", "bib_last_first", "nationality", "date", "result", "5"), sep = "  ") %>%

    ## Removes the unnecessary column at the beginning
    dplyr::select(-space, -`5`) %>%

    ## A series of steps to align the results of all of those who DNF-ed, and thus
    ## do not have a "place", with the results of the rest of them
    dplyr::mutate(result = ifelse(is.na(result) == TRUE, "DNF", result)) %>%
    dplyr::mutate(date = ifelse(result == "DNF", nationality, date)) %>%
    dplyr::mutate(nationality = ifelse(is.na(as.numeric(rank)) == TRUE, bib_last_first, nationality)) %>%
    dplyr::mutate(bib_last_first = ifelse(is.na(as.numeric(rank)) == TRUE, rank, bib_last_first)) %>%
    dplyr::mutate(rank = ifelse(is.na(as.numeric(rank)) == TRUE, NA, rank)) %>%

    ## Adds general overview information
    dplyr::mutate(gender = "W") %>%
    dplyr::mutate(event = "Marathon Women") %>%
    dplyr::mutate(location = "Tokyo") %>%
    dplyr::mutate(year = "2020") %>%

    ## Assigns medals based on rank
    dplyr::mutate(medal = ifelse(rank == "1", "G",
                          ifelse(rank == "2", "S",
                                 ifelse(rank == "3", "B", NA)))) %>%

    ## Removes athlete birthdate
    dplyr::select(-date)

  ## Removes bib number, from https://stackoverflow.com/questions/13590139/remove-numbers-from-alphanumeric-characters
  full_data_as_tibble_separated$bib_last_first <- gsub('[[:digit:]]+', '', full_data_as_tibble_separated$bib_last_first)

  ## Trims white space
  ## trimws from https://stat.ethz.ch/R-manual/R-patched/library/base/html/trimws.html
  full_data_as_tibble_separated$bib_last_first <- full_data_as_tibble_separated$bib_last_first %>%
    trimws("left")

  ## Renames the name column
  full_data_as_tibble_separated_n32 <- full_data_as_tibble_separated %>%
    dplyr::rename(name = bib_last_first)

  return(full_data_as_tibble_separated_n32)
}
