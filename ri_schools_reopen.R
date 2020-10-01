---
  title: "2020 Maine Vacation:  Should I stay, or should I go? (updated: `r lubridate::today()`)"
output:
  flexdashboard::flex_dashboard:
  vertical_layout: scroll
editor_options: 
  chunk_output_type: console
---

```{r echo=FALSE}
library(tidyverse)
library(readr)
library(lubridate)
library(zoo)
library(hrbrthemes)
library(plotly)

daily_nums <- function(metric){
  c(metric[1],rollapply(metric, 2, function(x) ifelse(is.na(x[1]), NA, x[2] - x[1])))
}

daily <- read_csv("https://covidtracking.com/api/v1/states/daily.csv")

ri_daily <- daily %>%
  filter(state == "RI") %>%
  select(date, positive, negative, pending, death, recovered) %>% 
  mutate(positive = case_when(is.na(positive) ~ 0,
                              TRUE ~ positive),
         negative = case_when(is.na(negative) ~ 0,
                              TRUE ~ negative),
         pending = case_when(is.na(pending) ~ 0,
                             TRUE ~ pending),
         death = case_when(is.na(death) ~ 0,
                           TRUE ~ death),
         
         date = ymd(date)) %>%
  arrange(date) %>%
  mutate(daily_positive = daily_nums(positive), 
         daily_negative = daily_nums(negative),
         daily_tests = daily_positive + daily_negative + pending,
         daily_death = daily_nums(death),
         daily_pending = pending,
         daily_recovered = daily_nums(recovered)) %>%
  select(date, daily_positive:daily_recovered) %>%
  mutate(daily_positivity = daily_positive/daily_tests * 100,
         seven_day_positive = rollapply(daily_positive, 7, function(x) mean(x, na.rm = T), fill = NA),
         seven_day_positivity= rollapply(daily_positivity, 7, function(x) mean(x, na.rm = T), fill = NA))  %>%
  filter(daily_negative != 0, date >= ymd("2020-03-01"))

cases_gg <- ri_daily %>%
  #filter(state == "RI") %>% 
  ggplot(aes(x = date, y = daily_positive)) +
  geom_point(aes(x = date, y = seven_day_positive)) +
  geom_line(aes(x = date, y = seven_day_positive)) +
  geom_bar(stat = "identity") +
  scale_y_log10() +
  theme_ipsum_rc()

#ggplotly(cases_gg)

positivity_gg <- ri_daily %>%
  #filter(state == "RI") %>% 
  ggplot(aes(x = date, y = daily_positivity)) +
  geom_point(aes(x = date, y = seven_day_positivity))+
  geom_line(aes(x = date, y = seven_day_positivity)) +
  scale_y_log10() +
  theme_ipsum_rc()

#ggplotly(positivity_gg)
```

