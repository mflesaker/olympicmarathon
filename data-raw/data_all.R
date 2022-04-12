
library(tidyverse)
library(lubridate)
library(htm2txt)

## 1996

## 1996 Men -------------------------------------------------------------------------------------------------------

url <- 'https://www.worldathletics.org/competition/calendar-results/results/6961749?eventId=10229634&gender=M'
olympic_1996_raw_men <- gettxt(url)

mens_1996_data <- olympic_1996_raw_men %>%
  strsplit("\n")

mens_1996_data <- as.character(mens_1996_data[[1]][100:223])

mens_1996_data <- mens_1996_data %>%
  as_tibble()

# vector of ranks of competitors who do not have full birthdays listed
rank_vec = c("110", "101", "98", "85", "44")

# separate names and times https://www.tutorialspoint.com/how-to-separate-string-and-a-numeric-value-in-r
mens_1996_data <- mens_1996_data %>%
  mutate(rank = ifelse(str_sub(value, -3, -1) == "DNF", NA, str_sub(value, 1, 2))) %>%
  mutate(rank = ifelse(str_sub(rank, -1, -1) == ".", str_sub(rank, 1, 1), rank)) %>%
  mutate(rank = ifelse(str_sub(value, 4, 4) == ".", str_sub(value, 1, 3), rank)) %>%
  mutate(name = ifelse(str_sub(value, -3, -1) == "DNF", str_sub(value, 1, -21), str_sub(value, 4, -25))) %>%
  mutate(name = ifelse(rank %in% rank_vec, str_sub(value, 4, -17), name)) %>%
  mutate(name = ifelse(str_sub(name, 1, 1) == ".", str_sub(name, 3, ), name)) %>%
  mutate(name = ifelse(str_sub(name, 1, 1) == " ", str_sub(name, 2, ), name)) %>%
  mutate(nationality = ifelse(str_sub(value, -3, -1) == "DNF", str_sub(value, -7, -5), str_sub(value, -11, -9))) %>%
  mutate(result = ifelse(str_sub(value, -3, -1) == "DNF", "DNF", str_sub(value, -7, -1))) %>%
  mutate(gender = "M") %>%
  mutate(event = "Marathon Men") %>%
  mutate(location = "Atlanta") %>%
  mutate(year = 1996) %>%
  mutate(medal = ifelse(rank == "1", "G", ifelse(rank == "2", "S", ifelse(rank == "3", "B", NA)))) %>%
  mutate(name = str_to_title(name)) %>%
  mutate(rank = as.numeric(rank)) %>%
  select(rank, name, nationality, result, gender, event, location, year, medal)

## 1996 Women -----------------------------------------------------------------------------------------------------

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


## 2000

## 2000 Men -------------------------------------------------------------------------------------------------------


## code from https://stackoverflow.com/questions/3195522/is-there-a-simple-way-in-r-to-extract-only-the-text-elements-of-an-html-page
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

## 2000 Women ----------------------------------------------------------------------------------------------------

url <- 'https://www.worldathletics.org/results/olympic-games/2004/the-xxvi-olympic-games-6951910/women/marathon/final/result'
olympic_2000_raw_women <- gettxt(url)

womens_2000_data <- olympic_2000_raw_women %>%
  strsplit("\n")

womens_2000_data <- as.character(womens_2000_data[[1]][233:337])

## code from https://www.edureka.co/community/3900/how-to-extract-every-nth-element-of-a-vector-using-r
womens_2000_data <- womens_2000_data[seq(1, length(womens_2000_data), 2)]

womens_2000_data <- womens_2000_data %>%
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
  mutate(result = ifelse(str_sub(result, -2, -1) == "CR", str_sub(result, 0, -3), ifelse(str_sub(result, -2, -1) == "NR", str_sub(result, 0, -3), result))) %>%
  mutate(gender = "F") %>%
  mutate(event = "Marathon Women") %>%
  mutate(location = "Sydney") %>%
  mutate(year = 2000) %>%
  mutate(medal = ifelse(rank == "1", "G", ifelse(rank == "2", "S", ifelse(rank == "3", "B", NA)))) %>%
  mutate(name = str_to_title(name)) %>%
  mutate(rank = as.numeric(rank))

## 2004

## 2004 Men -------------------------------------------------------------------------------------------------------

url <- 'https://www.worldathletics.org/results/olympic-games/2004/the-xxvi-olympic-games-6951910/men/marathon/final/result'
olympic_2004_raw_men <- gettxt(url)

mens_2004_data <- olympic_2004_raw_men %>%
  strsplit("\n")

mens_2004_data <- as.character(mens_2004_data[[1]][233:431])

## code from https://www.edureka.co/community/3900/how-to-extract-every-nth-element-of-a-vector-using-r
mens_2004_data <- mens_2004_data[seq(1, length(mens_2004_data), 2)]

mens_2004_data <- mens_2004_data %>%
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
  mutate(location = "Athens") %>%
  mutate(year = 2004) %>%
  mutate(medal = ifelse(rank == "1", "G", ifelse(rank == "2", "S", ifelse(rank == "3", "B", NA)))) %>%
  mutate(name = str_to_title(name)) %>%
  mutate(rank = as.numeric(rank))


## 2004 Women -------------------------------------------------------------------------------------------------------

url <- 'https://www.worldathletics.org/results/olympic-games/2994/the-xxvii-olympic-games-6913163/women/marathon/final/result'
olympic_2004_raw_women <- gettxt(url)

womens_2004_data <- olympic_2004_raw_women %>%
  strsplit("\n")

womens_2004_data <- as.character(womens_2004_data[[1]][233:395])

womens_2004_data

## code from https://www.edureka.co/community/3900/how-to-extract-every-nth-element-of-a-vector-using-r
womens_2004_data <- womens_2004_data[seq(1, length(womens_2004_data), 2)]

womens_2004_data <- womens_2004_data %>%
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
  mutate(gender = "F") %>%
  mutate(event = "Marathon Women") %>%
  mutate(location = "Athens") %>%
  mutate(year = 2004) %>%
  mutate(medal = ifelse(rank == "1", "G", ifelse(rank == "2", "S", ifelse(rank == "3", "B", NA)))) %>%
  mutate(name = str_to_title(name)) %>%
  mutate(rank = as.numeric(rank))



## 2008

## 2008 Men -------------------------------------------------------------------------------------------------------

url <- 'https://www.worldathletics.org/results/olympic-games/2008/the-xxix-olympic-games-6977748/men/marathon/final/result'
olympic_2008_raw_men <- gettxt(url)

mens_2008_data <- olympic_2008_raw_men %>%
  strsplit("\n")

mens_2008_data <- as.character(mens_2008_data[[1]][231:542])

## code from https://www.edureka.co/community/3900/how-to-extract-every-nth-element-of-a-vector-using-r
mens_2008_data <- mens_2008_data[seq(1, length(mens_2008_data), 2)]

mens_2008_data <- mens_2008_data %>%
  as_tibble() %>%
  filter(value != "POS ATHLETE COUNTRY MARK")

# separate names and times https://www.tutorialspoint.com/how-to-separate-string-and-a-numeric-value-in-r
mens_2008_data <- mens_2008_data %>%
  separate(value, into = c("rank_number_name_country", "result"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])") %>%
  separate(rank_number_name_country, into = c("rank_number", "name_country"), sep = "(?<=[0-9])\\s*(?=[a-zA-Z])") %>%
  ## stringr code from https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
  mutate(rank = ifelse(str_sub(rank_number, -2, -1) == "DQ", NA,
                       ifelse(str_sub(rank_number, -3, -1) == "DNF" | str_sub(rank_number, -3, -1) == "DNS", NA, rank_number))) %>%
  mutate(country = ifelse(str_sub(rank_number, -2, -1) == "DQ", str_sub(name_country, -6, -3),
                          ifelse(str_sub(rank_number, -3, -1) == "DNF" | str_sub(rank_number, -3, -1) == "DNS",
                                 str_sub(rank_number, -7, -4),
                                 str_sub(name_country, -3, -1)))) %>%
  mutate(name = ifelse(str_sub(rank_number, -2, -1) == "DQ",
                       str_sub(name_country, 0, -7),
                       ifelse(str_sub(rank_number, -3, -1) == "DNF" | str_sub(rank_number, -3, -1) == "DNS",
                              str_sub(rank_number, 0, -8),
                              str_sub(name_country, 0, -4)))) %>%
  mutate(result = ifelse(str_sub(rank_number, -2, -1) == "DQ", "DQ",
                         ifelse(str_sub(rank_number, -3, -1) == "DNF" | str_sub(rank_number, -3, -1) == "DNS",
                                str_sub(rank_number, -3, -1),
                                result))) %>%
  rename(nationality = country) %>%
  select(rank, name, nationality, result) %>%
  mutate(result = ifelse(str_sub(result, -2, -1) == "PB", str_sub(result, 0, -3), ifelse(str_sub(result, -2, -1) == "SB", str_sub(result, 0, -3), result))) %>%
  mutate(result = ifelse(str_sub(result, -2, -1) == "OR", str_sub(result, 0, -3), result)) %>%
  mutate(gender = "M") %>%
  mutate(event = "Marathon Men") %>%
  mutate(location = "Beijing") %>%
  mutate(year = 2008) %>%
  mutate(rank = str_trim(rank, side = "right")) %>%
  mutate(name = str_trim(name, side = "right")) %>%
  mutate(medal = ifelse(rank == "1", "G", ifelse(rank == "2", "S", ifelse(rank == "3", "B", NA)))) %>%
  filter(name != "NA") %>%
  mutate(name = str_to_title(name)) %>%
  mutate(rank = as.numeric(rank))


## 2008 Women -------------------------------------------------------------------------------------------------------

url <- 'https://www.worldathletics.org/results/olympic-games/2008/the-xxix-olympic-games-6977748/women/marathon/final/result'
olympic_2008_raw_women <- gettxt(url)

womens_2008_data <- olympic_2008_raw_women %>%
  strsplit("\n")

womens_2008_data <- as.character(womens_2008_data[[1]][231:542])

## code from https://www.edureka.co/community/3900/how-to-extract-every-nth-element-of-a-vector-using-r
womens_2008_data <- womens_2008_data[seq(0, length(womens_2008_data), 2)]

womens_2008_data <- womens_2008_data %>%
  as_tibble() %>%
  filter(value != "POS ATHLETE COUNTRY MARK")

# separate names and times https://www.tutorialspoint.com/how-to-separate-string-and-a-numeric-value-in-r
womens_2008_data <- womens_2008_data %>%
  separate(value, into = c("rank_number_name_country", "result"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])") %>%
  separate(rank_number_name_country, into = c("rank_number", "name_country"), sep = "(?<=[0-9])\\s*(?=[a-zA-Z])") %>%
  ## stringr code from https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
  mutate(rank = ifelse(str_sub(rank_number, -2, -1) == "DQ", NA,
                       ifelse(str_sub(rank_number, -3, -1) == "DNF" | str_sub(rank_number, -3, -1) == "DNS", NA, rank_number))) %>%
  mutate(country = ifelse(str_sub(rank_number, -2, -1) == "DQ", str_sub(name_country, -6, -3),
                          ifelse(str_sub(rank_number, -3, -1) == "DNF" | str_sub(rank_number, -3, -1) == "DNS",
                                 str_sub(rank_number, -7, -4),
                                 str_sub(name_country, -3, -1)))) %>%
  mutate(name = ifelse(str_sub(rank_number, -2, -1) == "DQ",
                       str_sub(name_country, 0, -7),
                       ifelse(str_sub(rank_number, -3, -1) == "DNF" | str_sub(rank_number, -3, -1) == "DNS",
                              str_sub(rank_number, 0, -8),
                              str_sub(name_country, 0, -4)))) %>%
  mutate(result = ifelse(str_sub(rank_number, -2, -1) == "DQ", "DQ",
                         ifelse(str_sub(rank_number, -3, -1) == "DNF" | str_sub(rank_number, -3, -1) == "DNS",
                                str_sub(rank_number, -3, -1),
                                result))) %>%
  rename(nationality = country) %>%
  select(rank, name, nationality, result) %>%
  mutate(result = ifelse(str_sub(result, -2, -1) == "PB", str_sub(result, 0, -3), ifelse(str_sub(result, -2, -1) == "SB", str_sub(result, 0, -3), result))) %>%
  mutate(result = ifelse(str_sub(result, -2, -1) == "NR", str_sub(result, 0, -3), result)) %>%
  mutate(gender = "F") %>%
  mutate(event = "Marathon Women") %>%
  mutate(location = "Beijing") %>%
  mutate(year = 2008) %>%
  mutate(rank = str_trim(rank, side = "right")) %>%
  mutate(name = str_trim(name, side = "right")) %>%
  mutate(medal = ifelse(rank == "1", "G", ifelse(rank == "2", "S", ifelse(rank == "3", "B", NA)))) %>%
  filter(name != "NA") %>%
  mutate(name = str_to_title(name)) %>%
  mutate(rank = as.numeric(rank))



## 2012

## 2012 Men -------------------------------------------------------------------------------------------------------

url <- 'https://www.worldathletics.org/results/olympic-games/2012/the-xxx-olympic-games-6999193/men/marathon/final/result'
olympic_2012_raw_men <- gettxt(url)

mens_2012_data <- olympic_2012_raw_men %>%
  strsplit("\n")

mens_2012_data <- as.character(mens_2012_data[[1]][231:542])

## code from https://www.edureka.co/community/3900/how-to-extract-every-nth-element-of-a-vector-using-r
mens_2012_data <- mens_2012_data[seq(1, length(mens_2012_data), 2)]

mens_2012_data <- mens_2012_data %>%
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
  mutate(location = "London") %>%
  mutate(year = 2012) %>%
  mutate(rank = str_trim(rank, side = "right")) %>%
  mutate(name = str_trim(name, side = "right")) %>%
  mutate(medal = ifelse(rank == "1", "G", ifelse(rank == "2", "S", ifelse(rank == "3", "B", NA)))) %>%
  mutate(name = str_to_title(name)) %>%
  mutate(rank = as.numeric(rank))

mens_2012_data <- mens_2012_data %>%
  filter(name != "NA")


## 2012 Women -------------------------------------------------------------------------------------------------------

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


## 2016


## 2016 Men -------------------------------------------------------------------------------------------------------

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
  mutate(medal = ifelse(rank == "1", "G", ifelse(rank == "2", "S", ifelse(rank == "3", "B", NA)))) %>%
  mutate(name = str_to_title(name)) %>%
  mutate(rank = as.numeric(rank))


## 2016 Women -------------------------------------------------------------------------------------------------------

url <- 'https://www.worldathletics.org/results/olympic-games/2016/the-xxxi-olympic-games-7093747/women/marathon/final/result'
olympic_2016_raw_women <- gettxt(url)

womens_2016_data <- olympic_2016_raw_women %>%
  strsplit("\n")

womens_2016_data <- as.character(womens_2016_data[[1]][233:545])

## code from https://www.edureka.co/community/3900/how-to-extract-every-nth-element-of-a-vector-using-r
womens_2016_data <- womens_2016_data[seq(1, length(womens_2016_data), 2)]

womens_2016_data <- womens_2016_data %>%
  as_tibble() %>%
  # separate names and times https://www.tutorialspoint.com/how-to-separate-string-and-a-numeric-value-in-r
  separate(value, into = c("rank_number_name_country", "result"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])") %>%
  separate(rank_number_name_country, into = c("rank_number", "name_country"), sep = "(?<=[0-9])\\s*(?=[a-zA-Z])") %>%
  ## stringr code from https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
  mutate(rank = ifelse(str_sub(name_country, -3, -1) == "DNS", NA,
                       ifelse(str_sub(name_country, -3, -1) == "DNF", NA, str_sub(rank_number, 0, -5)))) %>%
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
  mutate(gender = "F") %>%
  mutate(event = "Marathon Women") %>%
  mutate(location = "Rio de Janeiro") %>%
  mutate(year = 2016) %>%
  mutate(medal = ifelse(rank == "1", "G", ifelse(rank == "2", "S", ifelse(rank == "3", "B", NA)))) %>%
  mutate(name = str_to_title(name)) %>%
  mutate(rank = as.numeric(rank))


## 2020

## 2020 Men -------------------------------------------------------------------------------------------------------

library(pdftools)

## Pulls in PDF
## pdf_text from https://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/
original_text <- pdf_text("data-raw/mens_olympic_marathon.pdf")

## Splits the PDF by new line
## idea from: https://www.charlesbordet.com/en/extract-pdf/#use-pdftoolspdf_text
split_text <- original_text %>%
  strsplit("\n")

## Combines the two pieces of the data, which were on different lines
## combine lists from https://statisticsglobe.com/combine-lists-in-r
full_data <- c(split_text[[1]][21:66], split_text[[2]][17:64], split_text[[3]][17:28])

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
  mutate(gender = "M") %>%
  mutate(event = "Marathon Men") %>%
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
mens_2020_data <- full_data_as_tibble_separated %>%
  rename(name = bib_last_first) %>%
  separate(name, into = c("last", "first", "extra_name", "extra_name2"), sep = " ") %>%
  mutate(last_name = ifelse(str_detect(first,"[[:lower:]]") == FALSE, paste0(last, " ", first), last)) %>%
  mutate(first_name = ifelse(str_detect(first,"[[:lower:]]") == FALSE, extra_name, first)) %>%
  mutate(first_name = ifelse(is.na(extra_name2) == FALSE, paste0(first_name, " ", extra_name2), first_name)) %>%
  mutate(last_name = str_to_title(last_name)) %>%
  mutate(name = paste0(first_name, " ", last_name)) %>%
  select(rank, name, nationality, result, gender, event, location, year, medal) %>%
  mutate(rank = as.numeric(rank))

## 2020 Women -------------------------------------------------------------------------------------------------------

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
  mutate(gender = "F") %>%
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
  select(rank, name, nationality, result, gender, event, location, year, medal) %>%
  mutate(rank = as.numeric(rank))

olympic_marathon <- mens_1996_data %>%
  rbind(mens_2000_data) %>%
  rbind(mens_2004_data) %>%
  rbind(mens_2008_data) %>%
  rbind(mens_2012_data) %>%
  rbind(mens_2016_data) %>%
  rbind(mens_2020_data) %>%
  rbind(womens_1996_data) %>%
  rbind(womens_2000_data) %>%
  rbind(womens_2004_data) %>%
  rbind(womens_2008_data) %>%
  rbind(womens_2012_data) %>%
  rbind(womens_2016_data) %>%
  rbind(womens_2020_data) %>%
  mutate(result = ifelse(str_sub(result, -2, -1) == "NR", str_sub(result, 0, -3), result))


usethis::use_data(olympic_marathon, overwrite = TRUE)

