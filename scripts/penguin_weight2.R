# https://allisonhorst.github.io/palmerpenguins/

library(ggplot2)
library(palmerpenguins)
library(showtext)
library(tidyr)
library(dplyr)
library(ggbeeswarm)
library(ggnewscale)
library(ggtext)
library(ggpubr)


library(showtext)

font_add_google("Roboto")
font_add_google("Roboto Condensed")
showtext_auto()
showtext_opts(dpi = 100)

df <- penguins |>
  dplyr::filter(species == "Gentoo", !is.na(sex))

df_long <- df %>%
  pivot_longer(
    cols = c("body_mass_g"),
    names_to = "column",
    values_to = "value"
  )

group_col <- "sex"

quartiles <- df_long |>
  group_by(column, across(all_of(group_col))) |>
  summarise(
    q1 = quantile(value, 0.25),
    q2 = quantile(value, 0.50),
    q3 = quantile(value, 0.75),
    .groups = "drop"
  )


# Perform t-tests for each column
t_test_results <- df_long |>
  group_by(column) |>
  summarise(
    p_value = t.test(value ~ .data[[group_col]])$p.value,
    max_value = max(value),
    .groups = "drop"
  ) |>
  mutate(
    p_label = ifelse(
      p_value < 0.0001,
      "p < 0.0001",
      paste0("p = ", format(round(p_value, 4), nsmall = 4))
    ),
    y_position = max_value +
      (max(df_long$value) - min(df_long$value)) * 0.1
  )

# Get unique groups for positioning
groups <- unique(df_long[[group_col]])
n_groups <- length(groups)

# Create position offsets for each group
quartiles <- quartiles |>
  mutate(
    group_num = as.numeric(factor(.data[[group_col]], levels = groups)),
    x_offset = (group_num - (n_groups + 1) / 2) * 0.15 - 0.3
  )

# Create interaction variable for x-axis positioning
df_long <- df_long |>
  mutate(x_position = paste(column, .data[[group_col]], sep = "_"))

# Update quartiles with x_position
quartiles <- quartiles |>
  mutate(x_position = paste(column, .data[[group_col]], sep = "_"))


# Prepare significance annotations
# Get the positions for each column
column_positions <- df_long %>%
  distinct(column, x_position) %>%
  group_by(column) %>%
  summarise(
    x_min = min(as.numeric(factor(
      x_position,
      levels = unique(df_long$x_position)
    ))),
    x_max = max(as.numeric(factor(
      x_position,
      levels = unique(df_long$x_position)
    ))),
    x_mid = mean(c(x_min, x_max)),
    .groups = "drop"
  )

# Join with t-test results
sig_annotations <- t_test_results %>%
  left_join(column_positions, by = "column")

x_offset_line <- -0.15
x_offset_text_quartiles <- x_offset_line - 0.05

p <- ggplot(
  df_long,
  aes(x = x_position, y = value, color = .data[[group_col]])
) +
  geom_beeswarm(alpha = 0.75, size = 2, cex = 1.5) +

  scale_color_manual(
    values = c("female" = "salmon3", "male" = "dodgerblue3")
  ) +

  # Line segments from Q1 to Q3 for each column and group
  geom_segment(
    data = quartiles,
    aes(
      x = x_position,
      xend = x_position,
      y = q1,
      yend = q3,
      color = .data[[group_col]]
    ),
    linewidth = 0.5,
    inherit.aes = FALSE,
    position = position_nudge(x = x_offset_line)
  ) +

  # Horizontal lines for Q1
  geom_segment(
    data = quartiles,
    aes(
      x = as.numeric(factor(x_position)) + x_offset_line - 0.005,
      xend = as.numeric(factor(x_position)) + x_offset_line - 0.005 + 0.03,
      y = q1,
      yend = q1,
      color = .data[[group_col]]
    ),
    linewidth = 0.5,
    inherit.aes = FALSE,
  ) +

  # Horizontal lines for median
  geom_segment(
    data = quartiles,
    aes(
      x = as.numeric(factor(x_position)) + x_offset_line - 0.005,
      xend = as.numeric(factor(x_position)) + x_offset_line - 0.005 + 0.03,
      y = q2,
      yend = q2,
      color = .data[[group_col]]
    ),
    linewidth = 0.5,
    inherit.aes = FALSE,
  ) +

  # Horizontal lines for Q3
  geom_segment(
    data = quartiles,
    aes(
      x = as.numeric(factor(x_position)) + x_offset_line - 0.005,
      xend = as.numeric(factor(x_position)) + x_offset_line - 0.005 + 0.03,
      y = q3,
      yend = q3,
      color = .data[[group_col]]
    ),
    linewidth = 0.5,
    inherit.aes = FALSE,
  ) +

  # Add text labels for Q1
  geom_text(
    data = quartiles,
    aes(
      x = x_position,
      y = q1,
      label = round(q1, 1),
      color = .data[[group_col]]
    ),
    hjust = 1,
    fontface = "bold",
    size = 2.5,
    inherit.aes = FALSE,
    nudge_x = x_offset_text_quartiles
  ) +

  # Add text labels for median
  geom_text(
    data = quartiles,
    aes(
      x = x_position,
      y = q2,
      label = round(q2, 1),
      color = .data[[group_col]]
    ),
    hjust = 1,
    fontface = "bold",
    size = 2.5,
    inherit.aes = FALSE,
    nudge_x = x_offset_text_quartiles
  ) +

  # Add text labels for Q3
  geom_text(
    data = quartiles,
    aes(
      x = x_position,
      y = q3,
      label = round(q3, 1),
      color = .data[[group_col]]
    ),
    hjust = 1,
    fontface = "bold",
    size = 2.5,
    inherit.aes = FALSE,
    nudge_x = x_offset_text_quartiles
  ) +
  scale_x_discrete(
    # expand = expansion(mult = 0.5),
    labels = function(x) {
      sapply(strsplit(x, "_"), function(parts) tail(parts, n = 1))
    }
  ) +
  scale_y_continuous(
    breaks = seq(4000, 6500, by = 500),
    limits = c(3500, 6800),
    expand = c(0, 0),
  ) +

  # Add significance brackets
  geom_segment(
    data = sig_annotations,
    aes(x = x_min, xend = x_max, y = y_position, yend = y_position),
    color = "black",
    linewidth = 0.5,
    inherit.aes = FALSE
  ) +

  # Add vertical lines for brackets
  geom_segment(
    data = sig_annotations,
    aes(
      x = x_min,
      xend = x_min,
      y = y_position,
      yend = y_position - (max(df_long$value) - min(df_long$value)) * 0.02
    ),
    color = "black",
    linewidth = 0.5,
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = sig_annotations,
    aes(
      x = x_max,
      xend = x_max,
      y = y_position,
      yend = y_position - (max(df_long$value) - min(df_long$value)) * 0.02
    ),
    color = "black",
    linewidth = 0.5,
    inherit.aes = FALSE
  ) +

  # Add p-value labels
  geom_text(
    data = sig_annotations,
    aes(x = x_mid, y = y_position, label = p_label),
    vjust = -0.5,
    size = 3,
    color = "black",
    inherit.aes = FALSE
  ) +

  labs(
    title = "Weights of female and male Gentoo Penguins",
    x = "",
    y = "Weight [g]",
    color = tools::toTitleCase(group_col)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      size = 10,
      color = "black"
    ),
    axis.text = element_text(
      size = 10,
      color = "black",
    ),
    axis.ticks = element_line(linewidth = 0.25),
    axis.ticks.length = unit(0.25, "cm"),
    axis.title.y = element_text(
      color = "black",
      size = 12,
      margin = margin(r = 15)
    ),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 10, r = 30, b = 10, l = 10),
    plot.title = element_text(
      hjust = 0,
      size = 15,
      margin = margin(b = 20, l = -60),
    ),
    axis.line.x = element_line(linewidth = 0.25),
    axis.line.y = element_line(linewidth = 0.25),
    text = element_text(family = "Roboto Condensed"),
  )

ggsave(
  "plots/penguin_weight2.svg",
  plot = p,
  width = 600,
  height = 600,
  units = "px",
  dpi = 100,
)
