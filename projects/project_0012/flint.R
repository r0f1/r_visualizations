# Source
# https://github.com/rfordatascience/tidytuesday/tree/main/data/2025/2025-11-04

library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(patchwork)
library(showtext)
library(ggtext)
library(ggdist)

sysfonts::font_add(
  family = "Font Awesome 6 Brands",
  regular = "fontawesome/otfs/Font Awesome 7 Brands-Regular-400.otf"
)
font_add_google("Crimson Pro")
showtext_auto()
showtext_opts(dpi = 100)

social_caption <- glue::glue(
  "<span style='font-family:\"Font Awesome 6 Brands\";'>&#xf09b; </span>",
  "<span> r0f1</span>",
)

# Samples collected by Michigan Dept of Environment
flint_mdeq <- readr::read_csv("data/flint_mdeq.csv") |>
  mutate(removed = !is.na(notes), source = "ENV") |>
  select(lead, removed, source)

flint_mdeq_rem <- readr::read_csv("data/flint_mdeq.csv") |>
  mutate(removed = 0, source = "ENV REMOVED") |>
  select(lead2, removed, source) |>
  drop_na() |>
  rename(lead = lead2)

# Samples collected by Viginia Tech
flint_vt <- readr::read_csv("data/flint_vt.csv") |>
  mutate(removed = 0, source = "VT") |>
  select(lead, removed, source)

data <- bind_rows(flint_mdeq, flint_mdeq_rem, flint_vt)

quantile_90 <- data |>
  group_by(source) |>
  summarise(q90 = quantile(lead, 0.90), .groups = "drop") |>
  mutate(group_id = source)

data <- data |>
  filter(source %in% c("ENV", "VT")) |>
  mutate(source = factor(source), removed = factor(removed))

removed_obs <- data |> filter(removed == 1)

q_ok = 18
q_not_ok = 11.4
q_vt = 26.6

arrow_data <- tibble(
  x = c(104, 104),
  y = c(2.4, 2.375),
  xend = c(21, 104),
  yend = c(2.05, 2.05),
  label = c("Two measurements \nwith values 20 and \n104 were removed ...", ""),
  label_x = c(106, 0),
  label_y = c(2.4, 0)
)
arrow_data2 <- tibble(
  x = c(95),
  y = c(1.8),
  xend = c(12.5),
  yend = c(1.8),
  label = c("... which caused the 90% quantile to be drastically reduced."),
  label_x = c(55),
  label_y = c(1.63)
)

p <- ggplot(data, aes(x = lead, y = fct_rev(source), color = removed)) +
  geom_swarm(alpha = 0.85, dotsize = 0.75, shape = 19) +
  geom_segment(
    x = q_ok,
    xend = q_ok,
    y = 2 + 0.125,
    yend = 2 - 0.125,
    color = "blue",
    size = 0.5,
    inherit.aes = FALSE
  ) +
  geom_text(
    x = q_ok,
    y = 2 + 0.125,
    label = q_ok,
    color = "blue",
    vjust = -4,
    size = 3,
    show.legend = FALSE
  ) +
  geom_segment(
    x = q_not_ok,
    xend = q_not_ok,
    y = 2 + 0.125,
    yend = 2 - 0.125,
    color = "red",
    size = 0.5,
    inherit.aes = FALSE
  ) +
  geom_text(
    x = q_not_ok,
    y = 2 + 0.125,
    label = q_not_ok,
    color = "red",
    vjust = -4,
    size = 3,
    show.legend = FALSE
  ) +
  geom_segment(
    x = q_vt,
    xend = q_vt,
    y = 1 + 0.125,
    yend = 1 - 0.125,
    color = "green",
    size = 0.5,
    inherit.aes = FALSE
  ) +
  geom_text(
    x = q_vt,
    y = 1 + 0.125,
    label = q_vt,
    color = "green",
    vjust = -4,
    size = 3,
    show.legend = FALSE
  ) +
  geom_curve(
    data = arrow_data,
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    curvature = 0.1,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_curve(
    data = arrow_data2,
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    curvature = -0.1,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = arrow_data,
    aes(x = label_x, y = label_y, label = label),
    color = "black",
    size = 3.5,
    inherit.aes = FALSE,
    hjust = 0,
  ) +
  geom_text(
    data = arrow_data2,
    aes(x = label_x, y = label_y, label = label),
    color = "black",
    size = 3.5,
    inherit.aes = FALSE,
    hjust = 0.5,
  ) +
  scale_color_manual(
    values = c(
      "0" = "black",
      "1" = "red",
      "Environment Dept.0" = "blue",
      "Environment Dept.1" = "darkgreen",
      "Viginia Tech.0" = "purple"
    )
  ) +
  theme(legend.position = "none")
p
