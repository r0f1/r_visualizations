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
  mutate(removed = !is.na(notes), source = "Environment Dept") |>
  select(lead, removed, source)

# Samples collected by Viginia Tech
flint_vt <- readr::read_csv("data/flint_vt.csv") |>
  mutate(removed = 0, source = "Viginia Tech") |>
  select(lead, removed, source)

data <- bind_rows(flint_mdeq, flint_vt) |>
  mutate(source = factor(source), removed = factor(removed))

removed_obs <- data |> filter(removed == 1)

quantile_90 <- data |>
  group_by(source, removed) |>
  summarise(q90 = quantile(lead, 0.90), .groups = "drop") |>
  mutate(group_id = paste(source, removed, sep = "_"))

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
  stat_summary(
    fun = function(x) quantile(x, 0.90),
    geom = "crossbar",
    width = 0.25,
    size = 0.5,
    aes(color = interaction(source, removed))
  ) +
  geom_text(
    data = quantile_90,
    aes(
      x = q90,
      y = fct_rev(source),
      label = round(q90, 1),
      color = interaction(source, removed)
    ),
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
