library(readr)
library(ggplot2)
library(dplyr)
library(zoo)
library(lubridate)
library(hrbrthemes)
library(plotly)

daily_nums <- function(metric){
  c(metric[1],rollapply(metric, 2, function(x) ifelse(is.na(x[1]), NA, x[2] - x[1])))
}


covid_daily_states <- read_csv("https://covidtracking.com/api/v1/states/daily.csv") %>%
  select(date, state, positive, negative, pending, death, 
         daily_hospitalized = hospitalizedCurrently, ) %>% 
  mutate(positive = case_when(is.na(positive) ~ 0,
                              TRUE ~ positive),
         negative = case_when(is.na(negative) ~ 0,
                              TRUE ~ negative),
         pending = case_when(is.na(pending) ~ 0,
                             TRUE ~ pending),
         death = case_when(is.na(death) ~ 0,
                           TRUE ~ death),
         daily_hospitalized = case_when(is.na(daily_hospitalized) ~ 0,
                                        TRUE ~ daily_hospitalized),
         date = ymd(date)) %>%
  filter(state %in% c("RI", "ME", "MA", "NH")) %>%
  group_by(state) %>%
  arrange(state, date) %>%
  mutate(daily_positive = daily_nums(positive), 
         daily_negative = daily_nums(negative),
         daily_tests = daily_positive + daily_negative + pending,
         daily_death = daily_nums(death),
         daily_pending = pending) %>%
  ungroup() %>%
  select(date, state, daily_positive, daily_negative, daily_pending, 
         daily_tests, daily_death, daily_hospitalized) %>%
  mutate(week = week(date)) %>%
group_by(state, week) %>%
  summarize(date = max(date),
            weekly_positive = sum(daily_positive, na.rm = TRUE),
            weekly_negative = sum(daily_negative, na.rm = TRUE),
            weekly_pending = sum(daily_pending, na.rm = TRUE),
            weekly_hospitalized = sum(daily_hospitalized, na.rm = TRUE),
            weekly_tests = weekly_positive + weekly_negative + weekly_pending,
            weekly_positivity = weekly_positive/weekly_tests * 100) %>%
  ungroup() %>%
  filter(weekly_negative != 0, date >= ymd("2020-05-01"))

weekly_cases_gg <- ggplot(covid_daily_states, aes(x = date, 
                                                  y = weekly_positive)) +
  geom_line() +
  geom_point() +
  facet_grid(. ~ state) +
  scale_y_log10() +
  theme_ipsum_rc()
ggplotly(weekly_cases_gg)  

weekly_positivity_gg <- ggplot(covid_daily_states, aes(x = date, 
                                                       y = weekly_positivity)) +
  geom_line() +
  geom_point() +
  facet_grid(. ~ state) +
  theme_ipsum_rc()
ggplotly(weekly_positivity_gg)

weekly_hospital_gg <- ggplot(covid_daily_states, aes(x = date, 
                                                       y = weekly_hospitalized)) +
  geom_line() +
  geom_point() +
  facet_grid(. ~ state) +
  theme_ipsum_rc() +
  scale_y_log10()
ggplotly(weekly_hospital_gg)

