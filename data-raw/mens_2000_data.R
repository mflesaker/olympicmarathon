library(tidyverse)
library(lubridate)

## code from https://stackoverflow.com/questions/3195522/is-there-a-simple-way-in-r-to-extract-only-the-text-elements-of-an-html-page
library(htm2txt)
url <- 'https://www.worldathletics.org/results/olympic-games/2004/the-xxvi-olympic-games-6951910/men/marathon/final/result'
olympic_2000_raw_men <- gettxt(url)

mens_2000_data <- olympic_2000_raw_men %>%
  strsplit("\n")

mens_2000_data <- as.character(mens_2000_data[[1]][233:431])

## code from https://www.edureka.co/community/3900/how-to-extract-every-nth-element-of-a-vector-using-r
mens_2000_data <- mens_2000_data[seq(1, length(mens_2000_data), 2)]

mens_2000_data <- mens_2000_data %>%
  as_tibble() %>%
  # separate names and times https://www.tutorialspoint.com/how-to-separate-string-and-a-numeric-value-in-r
  separate(value, into = c("rank_name_country", "result"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])") %>%
  separate(rank_name_country, into = c("rank", "name_country"), sep = "(?<=[0-9])\\s*(?=[a-zA-Z])") %>%
  mutate(name_country = ifelse(str_sub(rank, -3, -1) %in% c("DNF", "DNS"), rank, name_country)) %>%
  mutate(rank = ifelse(str_sub(rank, -3, -1) %in% c("DNF", "DNS"), NA, rank)) %>%
  ## stringr code from https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
  mutate(country = ifelse(str_sub(name_country, -3, -1) == "DNS", str_sub(name_country, -7, -5),
                          ifelse(str_sub(name_country, -3, -1) == "DNF",
                                 str_sub(name_country, -7, -5),
                                 str_sub(name_country, -3, -1)))) %>%
  mutate(name = ifelse(str_sub(name_country, -3, -1) == "DNS",
                       str_sub(name_country, 0, -9),
                       ifelse(str_sub(name_country, -3, -1) == "DNF",
                              str_sub(name_country, 0, -9),
                              str_sub(name_country, 0, -4)))) %>%
  mutate(result = ifelse(str_sub(name_country, -3, -1) == "DNS", "DNS",
                         ifelse(str_sub(name_country, -3, -1) == "DNF", "DNF",
                                result))) %>%
  rename(nationality = country) %>%
  select(rank, name, nationality, result) %>%
  mutate(result = ifelse(str_sub(result, -2, -1) == "PB", str_sub(result, 0, -3), ifelse(str_sub(result, -2, -1) == "SB", str_sub(result, 0, -3), result))) %>%
  mutate(result = ifelse(str_sub(result, -2, -1) == "NR", str_sub(result, 0, -3), result)) %>%
  mutate(gender = "M") %>%
  mutate(event = "Marathon Men") %>%
  mutate(location = "Sydney") %>%
  mutate(year = 2000) %>%
  mutate(medal = ifelse(rank == "1", "G", ifelse(rank == "2", "S", ifelse(rank == "3", "B", NA)))) %>%
  mutate(name = str_to_title(name)) %>%
  mutate(rank = as.numeric(rank))

usethis::use_data(mens_2000_data, overwrite = TRUE)



