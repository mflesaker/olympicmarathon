library(tidyverse)
library(lubridate)

## code from https://stackoverflow.com/questions/3195522/is-there-a-simple-way-in-r-to-extract-only-the-text-elements-of-an-html-page
library(htm2txt)
url <- 'https://www.worldathletics.org/results/olympic-games/2012/the-xxx-olympic-games-6999193/women/marathon/final/result'
olympic_2012_raw_women <- gettxt(url)

womens_2012_data <- olympic_2012_raw_women %>%
  strsplit("\n")

womens_2012_data <- as.character(womens_2012_data[[1]][231:542])

## code from https://www.edureka.co/community/3900/how-to-extract-every-nth-element-of-a-vector-using-r
womens_2012_data <- womens_2012_data[seq(0, length(womens_2012_data), 2)]

womens_2012_data <- womens_2012_data %>%
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
  mutate(result = ifelse(str_sub(result, -2, -1) == "NR", str_sub(result, 0, -3), result)) %>%
  mutate(result = ifelse(str_sub(result, -2, -1) == "OR", str_sub(result, 0, -3), result)) %>%
  mutate(gender = "F") %>%
  mutate(event = "Marathon Women") %>%
  mutate(location = "London") %>%
  mutate(year = 2012) %>%
  mutate(rank = str_trim(rank, side = "right")) %>%
  mutate(name = str_trim(name, side = "right")) %>%
  mutate(medal = ifelse(rank == "1", "G", ifelse(rank == "2", "S", ifelse(rank == "3", "B", NA))))

womens_2012_data <- womens_2012_data %>%
  filter(name != "NA") %>%
  mutate(name = str_to_title(name)) %>%
  mutate(rank = as.numeric(rank))

usethis::use_data(womens_2012_data, overwrite = TRUE)
