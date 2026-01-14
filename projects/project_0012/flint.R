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
font_add_google("Roboto")
font_add_google("Roboto Condensed")
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
  mutate(
    source = factor(
      source,
      levels = c("ENV", "VT"),
      labels = c("MDEQ", "Virinia Tech")
    ),
    removed = factor(removed),
    color_group = paste(source, removed, sep = ".")
  )

removed_obs <- data |> filter(removed == 1)

q_ok = 18
q_not_ok = 11.4
q_vt = 26.6


height_label2 = 1.8
xpos_label2 = 120

arrow_data <- tibble(
  x = c(xpos_label2 - 2, xpos_label2 - 2),
  y = c(height_label2, height_label2 + 0.05),
  xend = c(21, 104),
  yend = c(2 - 0.05, 2 - 0.05),
  label = c("Two measurements \nwith values 20 and \n104 were removed ...", ""),
  label_x = c(xpos_label2, 0),
  label_y = c(height_label2, 0)
)

height_moved = 1.76

arrow_data2 <- tibble(
  x = c(18.5),
  y = c(height_moved),
  xend = c(11.5),
  yend = c(height_moved),
  label = c(
    "... which moved \nthe 90% quantile\nbelow the threshold."
  ),
  label_x = c(20),
  label_y = c(height_moved - 0.09)
)

xpos_label_th = 30
height_label_th = 2.45

arrow_data3 <- tibble(
  x = c(xpos_label_th - 2),
  y = c(height_label_th),
  xend = c(16.5),
  yend = c(height_label_th),
  label = c(
    "Threshold of 15 ppb.\nIf more than 10% of samples \nare above it, action is required."
  ),
  label_x = c(xpos_label_th),
  label_y = c(height_label_th)
)

line1_color = "red"
line2_color = "black"
line3_color = "black"

p <- ggplot(data, aes(x = lead, y = fct_rev(source), color = color_group)) +
  geom_swarm(alpha = 0.85, dotsize = 0.75, shape = 19) +
  geom_segment(
    x = q_ok,
    xend = q_ok,
    y = 2 + 0.125,
    yend = 2 - 0.125,
    color = line2_color,
    size = 0.5,
    inherit.aes = FALSE
  ) +
  annotate(
    "text",
    x = q_ok,
    y = 2 + 0.125,
    label = q_ok,
    color = line2_color,
    vjust = 9,
    size = 3,
  ) +
  geom_segment(
    x = q_not_ok,
    xend = q_not_ok,
    y = 2 + 0.125,
    yend = 2 - 0.125,
    color = line1_color,
    size = 0.5,
    inherit.aes = FALSE
  ) +
  annotate(
    "text",
    x = q_not_ok,
    y = 2 + 0.125,
    label = q_not_ok,
    color = line1_color,
    vjust = 9,
    size = 3,
  ) +
  geom_segment(
    x = q_vt,
    xend = q_vt,
    y = 1 + 0.125,
    yend = 1 - 0.125,
    color = line3_color,
    size = 0.5,
    inherit.aes = FALSE
  ) +
  annotate(
    "text",
    x = q_vt,
    y = 1 + 0.125,
    label = q_vt,
    color = line3_color,
    vjust = 9,
    size = 3,
  ) +
  geom_curve(
    data = arrow_data,
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    curvature = -0.1,
    color = "#D72638",
    inherit.aes = FALSE
  ) +
  geom_curve(
    data = arrow_data2,
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    curvature = -0.5,
    color = "#D72638",
    inherit.aes = FALSE
  ) +
  geom_curve(
    data = arrow_data3,
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    curvature = 0,
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
    hjust = 0,
  ) +
  geom_text(
    data = arrow_data3,
    aes(x = label_x, y = label_y, label = label),
    color = "black",
    size = 3.5,
    inherit.aes = FALSE,
    hjust = 0,
  ) +
  annotate(
    "text",
    x = 170,
    y = 2.1,
    label = "n = 69 (71)",
    hjust = 0,
    size = 4,
    color = "black"
  ) +
  annotate(
    "text",
    x = 170,
    y = 1.1,
    label = "n = 271",
    hjust = 0,
    size = 4,
    color = "black"
  ) +
  annotate(
    "text",
    x = 40,
    y = 0.75,
    label = "Analysis by Virinia Tech University\nrevealed that even more homes\nhad highly contaminated water.",
    hjust = 0,
    size = 3.5,
    color = "black"
  ) +
  geom_vline(xintercept = 15, linetype = "dashed", color = "black") +
  scale_color_manual(
    values = c(
      "MDEQ.0" = "#1D4E89",
      "MDEQ.1" = "#D72638",
      "Virinia Tech.0" = "#F79256"
    )
  ) +
  scale_x_continuous(
    breaks = seq(0, 180, by = 50),
    expand = expansion(add = c(0, 20)),
    limits = c(0, 180),
  ) +
  scale_y_discrete(expand = expansion(mult = c(0.25, 0.25))) +
  coord_cartesian(ylim = c(0.75, 2.2), clip = "off") +
  labs(
    x = "Lead Concentration [ppb]",
    y = "",
    title = "Lead in Flints Water Supply",
    subtitle = "Officials at the Michigan Department of Environmental Quality (MDEQ) insisted that the water\nsupply in Flint, Michigan was save, when it was actually contaminated with high levels of lead.",
    caption = social_caption,
  ) +
  theme_minimal() +
  theme(
    axis.line.x = element_line(linewidth = 0.25),
    axis.text.x = element_text(
      size = 11,
      color = "black",
    ),
    axis.text.y = element_text(
      size = 11,
      color = "black",
    ),
    axis.ticks.length.x = unit(0.1, "cm"),
    axis.ticks.length.y = unit(0.20, "cm"),
    axis.ticks.x = element_line(color = "black", linewidth = 0.25),
    axis.ticks.y = element_line(linewidth = 0),
    axis.title.x = element_text(
      size = 11,
      color = "black",
      margin = margin(t = 10)
    ),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold",
      size = 20,
      hjust = 0,
      margin = margin(b = 10),
    ),
    plot.caption = element_markdown(
      size = 10,
      color = "grey40",
    ),
    plot.margin = margin(15, 15, 15, 15),
    plot.subtitle = element_text(
      margin = margin(b = 25),
      lineheight = 1.22,
    ),
    legend.position = "none",
    text = element_text(family = "Roboto"),
  ) +
  patchwork::plot_annotation(
    caption = "Source: TidyTuesday 2025 / Week 44",
    theme = theme(
      plot.caption = element_text(
        size = 10,
        hjust = 0,
        margin = margin(t = -25, l = 5),
        color = "grey40",
      ),
      text = element_text(family = "Roboto Condensed"),
    )
  )

ggsave(
  here::here("projects", "project_0012", "flint.svg"),
  plot = p,
  width = 1110,
  height = 750,
  units = "px",
  dpi = 100,
)
