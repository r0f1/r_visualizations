# https://allisonhorst.github.io/palmerpenguins/

library(ggplot2)
library(gghalves)
library(palmerpenguins)
library(showtext)

font_add_google("Roboto Condensed")
showtext_auto()
showtext_opts(dpi = 100)

data = penguins |> na.omit()

p <- ggplot(data, aes(x = species, y = body_mass_g, fill = species)) +
  geom_half_violin(
    aes(color = species),
    side = "r",
    trim = FALSE,
  ) +

  # Q1 marker and label
  stat_summary(
    fun = function(x) quantile(x, 0.25, na.rm = TRUE),
    aes(xend = after_stat(x) - 0.1, yend = after_stat(y)),
    geom = "segment",
    linewidth = 0.5,
    color = "black"
  ) +
  stat_summary(
    fun = function(x) quantile(x, 0.25, na.rm = TRUE),
    geom = "text",
    aes(label = sprintf("%.0f", after_stat(y))),
    vjust = 2.5,
    hjust = 1,
    size = 3,
    fontface = "bold",
  ) +

  # Median marker and label
  stat_summary(
    fun = median,
    aes(xend = after_stat(x) - 0.1, yend = after_stat(y)),
    geom = "segment",
    linewidth = 0.5,
    color = "black"
  ) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = sprintf("%.0f", after_stat(y))),
    vjust = 2.5,
    size = 3,
    fontface = "bold"
  ) +

  # Q3 marker and label
  stat_summary(
    fun = function(x) quantile(x, 0.75, na.rm = TRUE),
    aes(xend = after_stat(x) - 0.1, yend = after_stat(y)),
    geom = "segment",
    linewidth = 0.5,
    color = "black"
  ) +
  stat_summary(
    fun = function(x) quantile(x, 0.75, na.rm = TRUE),
    geom = "text",
    aes(label = sprintf("%.0f", after_stat(y))),
    vjust = 2.5,
    size = 3,
    hjust = 0,
    fontface = "bold"
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Adelie" = "#377EB8",
      "Chinstrap" = "#4DAF4A",
      "Gentoo" = "#984EA3"
    ),
  ) +
  scale_color_manual(
    values = c(
      "Adelie" = "#377EB8",
      "Chinstrap" = "#4DAF4A",
      "Gentoo" = "#984EA3"
    )
  ) +
  labs(
    title = "How much do penguins weigh?",
    x = "",
    y = "Weight [g]",
  ) +
  theme_minimal() +
  theme(
    axis.ticks.x = element_line(linewidth = 0.5),
    axis.line.x = element_line(linewidth = 0.5),
    axis.text.x = element_text(
      size = 10,
      color = "black",
      margin = margin(t = 10)
    ),
    axis.text.y = element_text(
      size = 12,
      color = "black",
      hjust = 1,
      margin = margin(r = 10)
    ),
    axis.ticks.length.x = unit(0.15, "cm"),
    axis.title.x = element_text(
      size = 12,
      color = "black",
      margin = margin(t = 13)
    ),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 10, r = 30, b = 10, l = 10),
    plot.title = element_text(
      hjust = 0.5,
      size = 15,
      face = "bold",
    ),
    text = element_text(family = "Roboto Condensed"),
  )

ggsave(
  "plots/penguin_weight.svg",
  plot = p,
  width = 600,
  height = 400,
  units = "px",
  dpi = 100,
)
