library(tidyverse)
library(lubridate)

## code from https://stackoverflow.com/questions/3195522/is-there-a-simple-way-in-r-to-extract-only-the-text-elements-of-an-html-page
library(htm2txt)
url <- 'https://www.worldathletics.org/competition/calendar-results/results/6961749?eventId=10229534&gender=W'
olympic_1996_raw_women <- gettxt(url)

womens_1996_data <- olympic_1996_raw_women %>%
  strsplit("\n")

womens_1996_data <- as.character(womens_1996_data[[1]][100:187])

womens_1996_data <- womens_1996_data %>%
  as_tibble()

# separate names and times https://www.tutorialspoint.com/how-to-separate-string-and-a-numeric-value-in-r
womens_1996_data <- womens_1996_data %>%
  mutate(rank = ifelse(str_sub(value, -2, -1) == "DQ" | str_sub(value, -3, -1) == "DNF" | str_sub(value, 1, 9) == "Valentina", NA, str_sub(value, 1, 2))) %>%
  mutate(rank = ifelse(str_sub(rank, -1, -1) == ".", str_sub(rank, 1, 1), rank)) %>%
  mutate(name = ifelse(str_sub(value, -2, -1) == "DQ", str_sub(value, 1, -20), str_sub(value, 4, -25))) %>%
  mutate(name = ifelse(str_sub(value, 1, 9) == "Valentina", str_sub(value, 1, 16), name)) %>%
  mutate(name = ifelse(str_sub(value, -3, -1) == "DNF", str_sub(value, 1, -21), name)) %>%
  mutate(name = ifelse(str_sub(name, 1, 1) == " ", str_sub(name, 2, ), name)) %>%
  mutate(nationality = ifelse(str_sub(value, -2, -1) == "DQ", str_sub(value, -6, -4), str_sub(value, -11, -9))) %>%
  mutate(nationality = ifelse(str_sub(value, -3, -1) == "DNF", str_sub(value, -7, -5), nationality)) %>%
  mutate(result = ifelse(str_sub(value, -3, -1) == "DNF", "DNF", str_sub(value, -7, -1))) %>%
  mutate(result = ifelse(str_sub(value, -2, -1) == "DQ", "DQ", result)) %>%
  mutate(gender = "F") %>%
  mutate(event = "Marathon Women") %>%
  mutate(location = "Atlanta") %>%
  mutate(year = 1996) %>%
  mutate(medal = ifelse(rank == "1", "G", ifelse(rank == "2", "S", ifelse(rank == "3", "B", NA)))) %>%
  mutate(name = str_to_title(name)) %>%
  mutate(rank = as.numeric(rank)) %>%
  select(rank, name, nationality, result, gender, event, location, year, medal)

usethis::use_data(womens_1996_data, overwrite = TRUE)
