library(lubridate)
library(dplyr)

source("seq_double.R")

every_1_day <- tibble(cases = seq_double(2,60), date_1 = ymd(ymd("2020-03-03") + 
                        seq(1, length.out = length(cases), by = 1))) %>%
  filter(date_1 <= today()) %>%
  mutate(days_since = as.numeric(date_1 - min(date_1)))
every_2_day <- tibble(cases = seq_double(2,60), date_2 = ymd("2020-03-03") + 
                        seq(1, length.out = length(cases), by = 2)) %>%
  filter(date_2 <= today()) %>%
  mutate(days_since = as.numeric(date_2 - min(date_2)))
every_3_day <- tibble(cases = seq_double(2,60), date_3 = ymd("2020-03-03") + 
                        seq(1, length.out = length(cases), by = 3)) %>%
  filter(date_3 <= today()) %>%
  mutate(days_since = as.numeric(date_3 - min(date_3)))
every_4_day <- tibble(cases = seq_double(2,60), date_4 = ymd("2020-03-03") + 
                        seq(1, length.out = length(cases), by = 4)) %>%
  filter(date_4 <= today()) %>%
  mutate(days_since = as.numeric(date_4 - min(date_4)))
