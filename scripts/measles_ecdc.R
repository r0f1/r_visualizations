# https://atlas.ecdc.europa.eu/public/index.aspx

library(tidyverse)
library(dplyr)
library(lubridate)

df <- read_csv("data/measles_ecdc.csv") |>
  dplyr::filter(RegionCode == "AT") |>
  select(c("Time", "NumValue")) |>
  rename(year_month = Time, cases = NumValue) |>
  mutate(date = ymd(paste0(year_month, "-01"))) |>
  filter(year(date) >= 2003) |>
  mutate(cases = as.numeric(cases)) |>
  select(c("date", "cases")) |>
  mutate(quarter_date = floor_date(date, unit = "quarter")) |>
  group_by(quarter_date) |>
  summarize(cases = sum(cases, na.rm = TRUE))

ggplot(df, aes(x = quarter_date, y = cases)) +
  geom_col() +
  labs(
    title = "Value Trend Over Time",
    x = "Year",
    y = "Number of Measles Cases (N)"
  ) +
  theme_minimal()
