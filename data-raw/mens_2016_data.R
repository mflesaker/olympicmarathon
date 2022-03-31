library(tidyverse)
library(lubridate)

## code from https://stackoverflow.com/questions/3195522/is-there-a-simple-way-in-r-to-extract-only-the-text-elements-of-an-html-page
library(htm2txt)
url <- 'https://www.worldathletics.org/results/olympic-games/2016/the-xxxi-olympic-games-7093747/men/marathon/final/result'
olympic_2016_raw_men <- gettxt(url)

mens_2016_data <- olympic_2016_raw_men %>%
  strsplit("\n")

mens_2016_data <- as.character(mens_2016_data[[1]][231:542])

## code from https://www.edureka.co/community/3900/how-to-extract-every-nth-element-of-a-vector-using-r
mens_2016_data <- mens_2016_data[seq(1, length(mens_2016_data), 2)]

mens_2016_data <- mens_2016_data %>%
  as_tibble() %>%
  filter(value != "POS BIB ATHLETE COUNTRY MARK") %>%
  # separate names and times https://www.tutorialspoint.com/how-to-separate-string-and-a-numeric-value-in-r
  separate(value, into = c("rank_number_name_country", "result"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])") %>%
  separate(rank_number_name_country, into = c("rank_number", "name_country"), sep = "(?<=[0-9])\\s*(?=[a-zA-Z])") %>%
  ## stringr code from https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
  mutate(rank = ifelse(str_sub(name_country, -2, -1) == "DQ", NA,
                       ifelse(str_sub(name_country, -3, -1) == "DNF", NA, str_sub(rank_number, 0, -5)))) %>%
  mutate(country = ifelse(str_sub(name_country, -2, -1) == "DQ", str_sub(name_country, -6, -3),
                          ifelse(str_sub(name_country, -3, -1) == "DNF",
                                 str_sub(name_country, -7, -4),
                                 str_sub(name_country, -3, -1)))) %>%
  mutate(name = ifelse(str_sub(name_country, -2, -1) == "DQ",
                       str_sub(name_country, 0, -7),
                       ifelse(str_sub(name_country, -3, -1) == "DNF",
                              str_sub(name_country, 0, -8),
                              str_sub(name_country, 0, -4)))) %>%
  mutate(result = ifelse(str_sub(name_country, -2, -1) == "DQ", "DQ",
                       ifelse(str_sub(name_country, -3, -1) == "DNF", "DNF",
                              result))) %>%
  rename(nationality = country) %>%
  select(rank, name, nationality, result) %>%
  mutate(result = ifelse(str_sub(result, -2, -1) == "PB", str_sub(result, 0, -3), ifelse(str_sub(result, -2, -1) == "SB", str_sub(result, 0, -3), result))) %>%
  mutate(gender = "M") %>%
  mutate(event = "Marathon Men") %>%
  mutate(location = "Rio de Janeiro") %>%
  mutate(year = 2016) %>%
  mutate(rank = str_trim(rank, side = "right")) %>%
  mutate(name = str_trim(name, side = "right")) %>%
  mutate(medal = ifelse(rank == "1", "G", ifelse(rank == "2", "S", ifelse(rank == "3", "B", NA))))

usethis::use_data(mens_2016_data, overwrite = TRUE)
