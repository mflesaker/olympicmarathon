## code to prepare `womens_2020` dataset goes here

library(tidyverse)
library(pdftools)

## Pulls in PDF
## pdf_text from https://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/
original_text <- pdf_text("data-raw/womens_olympic_marathon.pdf")

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
full_data_as_tibble <- as_tibble(full_data_no_extra_spaces)

full_data_as_tibble_separated <- full_data_as_tibble %>%

  ## Separates the mass of text into columns
  separate(value, into = c("space", "rank", "bib_last_first", "nationality", "date", "result", "5"), sep = "  ") %>%

  ## Removes the unnecessary column at the beginning
  select(-space, -`5`) %>%

  ## A series of steps to align the results of all of those who DNF-ed, and thus
  ## do not have a "place", with the results of the rest of them
  mutate(result = ifelse(is.na(result) == TRUE, "DNF", result)) %>%
  mutate(date = ifelse(result == "DNF", nationality, date)) %>%
  mutate(nationality = ifelse(is.na(as.numeric(rank)) == TRUE, bib_last_first, nationality)) %>%
  mutate(bib_last_first = ifelse(is.na(as.numeric(rank)) == TRUE, rank, bib_last_first)) %>%
  mutate(rank = ifelse(is.na(as.numeric(rank)) == TRUE, NA, rank)) %>%

  ## Adds general overview information
  mutate(gender = "W") %>%
  mutate(event = "Marathon Women") %>%
  mutate(location = "Tokyo") %>%
  mutate(year = "2020") %>%

  ## Assigns medals based on rank
  mutate(medal = ifelse(rank == "1", "G",
                               ifelse(rank == "2", "S",
                                      ifelse(rank == "3", "B", NA)))) %>%

  ## Removes athlete birthdate
  select(-date)

## Removes bib number, from https://stackoverflow.com/questions/13590139/remove-numbers-from-alphanumeric-characters
full_data_as_tibble_separated$bib_last_first <- gsub('[[:digit:]]+', '', full_data_as_tibble_separated$bib_last_first)

## Trims white space
## trimws from https://stat.ethz.ch/R-manual/R-patched/library/base/html/trimws.html
full_data_as_tibble_separated$bib_last_first <- full_data_as_tibble_separated$bib_last_first %>%
  trimws("left")

## Renames the name column
womens_2020_data <- full_data_as_tibble_separated %>%
  rename(name = bib_last_first) %>%
  separate(name, into = c("last", "first", "extra_name", "extra_name2", "extra name3"), sep = " ") %>%
  mutate(last_name = ifelse(str_detect(first,"[[:lower:]]") == FALSE, paste0(last, " ", first), last)) %>%
  mutate(first_name = ifelse(str_detect(first,"[[:lower:]]") == FALSE, extra_name, first)) %>%
  mutate(first_name = ifelse(is.na(extra_name2) == FALSE, paste0(first_name, " ", extra_name2), first_name)) %>%
  mutate(last_name = ifelse(first_name == "la CAPANI", "de la CRUZ CAPANI", last_name)) %>%
  mutate(first_name = ifelse(first_name == "la CAPANI", "Jovana", first_name)) %>%
  mutate(last_name = str_to_title(last_name)) %>%
  mutate(name = paste0(first_name, " ", last_name)) %>%
  select(rank, name, nationality, result, gender, event, location, year, medal)

usethis::use_data(womens_2020_data, overwrite = TRUE)
