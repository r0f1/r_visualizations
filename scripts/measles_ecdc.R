# https://atlas.ecdc.europa.eu/public/index.aspx

library(tidyverse)
library(dplyr)
library(lubridate)
library(showtext)

font_add_google("Roboto")
font_add_google("Roboto Condensed")
showtext_auto()
showtext_opts(dpi = 100)

df_all <- read_csv("data/measles_ecdc.csv") |>
  dplyr::filter(RegionCode == "AT")

df <- df_all |>
  select(c("Time", "NumValue")) |>
  rename(year_month = Time, cases = NumValue) |>
  mutate(date = ymd(paste0(year_month, "-01"))) |>
  filter(year(date) >= 2003) |>
  mutate(cases = as.numeric(cases)) |>
  select(c("date", "cases")) |>
  mutate(quarter_date = floor_date(date, unit = "quarter")) |>
  group_by(quarter_date) |>
  summarize(cases = sum(cases, na.rm = TRUE)) |>
  mutate(
    year = year(quarter_date),
    bar_color = case_when(
      year == 2008 ~ "#e4b51a",
      year == 2015 ~ "#377EB8",
      year == 2023 ~ "#4DAF4A",
      year == 2024 ~ "#984EA3",
      TRUE ~ "grey60"
    )
  )


df_top <- df |>
  arrange(desc(cases)) |>
  slice_head(n = 5)

df_yearly <- df |>
  mutate(year = year(quarter_date)) |>
  group_by(year) |>
  summarize(cases = sum(cases, na.rm = TRUE)) |>
  arrange(cases)

p <- ggplot(df, aes(x = quarter_date, y = cases, fill = bar_color)) +
  geom_col(width = 60) +
  scale_fill_identity() +
  geom_text(data = df_top, aes(label = cases), vjust = -0.5, size = 3.5) +
  labs(
    title = "Number of Measles Cases in Austria (Jan 2003 - Oct 2025)",
    x = "",
    y = "Measles Cases (N)"
  ) +
  annotate(
    "text",
    x = as.Date("2023-05-01"),
    y = 363,
    color = "#984EA3",
    family = "Roboto Condensed",
    hjust = 1,
    label = paste0(
      "2024\n",
      (df_yearly |> filter(year == 2024))$cases,
      " cases"
    ),
    size = 4,
  ) +
  annotate(
    "text",
    x = as.Date("2022-05-01"),
    y = 89,
    color = "#4DAF4A",
    family = "Roboto Condensed",
    hjust = 1,
    label = paste0(
      "2023\n",
      (df_yearly |> filter(year == 2023))$cases,
      " cases"
    ),
    size = 4,
  ) +
  annotate(
    "text",
    x = as.Date("2014-05-01"),
    y = 114,
    color = "#377EB8",
    family = "Roboto Condensed",
    hjust = 1,
    label = paste0(
      "2015\n",
      (df_yearly |> filter(year == 2015))$cases,
      " cases"
    ),
    size = 4,
  ) +
  annotate(
    "text",
    x = as.Date("2007-08-01"),
    y = 90,
    color = "#e4b51a",
    family = "Roboto Condensed",
    hjust = 1,
    label = paste0(
      "2008\n",
      (df_yearly |> filter(year == 2008))$cases,
      " cases"
    ),
    size = 4,
  ) +
  scale_x_date(
    breaks = seq(
      from = as.Date("2003-01-01"),
      to = as.Date("2025-01-01"),
      by = "2 years"
    ),
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    breaks = seq(0, 400, by = 100),
    limit = c(0, 400),
    expand = expansion(add = c(3, 0)),
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(linewidth = 0.75),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.1, "cm"),
    axis.text = element_text(
      color = "black",
      size = 12,
    ),
    axis.title.y = element_text(
      color = "black",
      size = 12,
      margin = margin(r = 15)
    ),
    axis.title = element_text(size = 14, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    plot.title = element_text(
      size = 15,
      margin = margin(b = 20, l = -30),
    ),
    legend.position = "none",
    text = element_text(family = "Roboto Condensed"),
  )

ggsave(
  "plots/measles_ecdc.svg",
  plot = p,
  width = 700,
  height = 600,
  units = "px",
  dpi = 100,
)
