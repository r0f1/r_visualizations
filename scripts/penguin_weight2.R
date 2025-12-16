library(ggplot2)
library(palmerpenguins)
library(showtext)
library(dplyr)
library(ggbeeswarm)
library(ggnewscale)
library(ggtext)
library(ggpubr)

font_add_google("Roboto", "roboto")
showtext_auto()
showtext_opts(dpi = 100)

df <- penguins |>
  dplyr::filter(species == "Gentoo") |>
  dplyr::filter(sex %in% c("male", "female"))

p <- ggplot(df, aes(x = sex, y = body_mass_g, color = sex)) +
  geom_beeswarm(size = 1.25, alpha = 0.75, cex = 2) +
  scale_color_manual(values = c("female" = "#ef8a62", "male" = "#67a9cf")) +
  new_scale_color() +
  stat_summary(
    aes(color = sex),
    fun = median,
    geom = "crossbar",
    width = 0.15,
    linewidth = 0.65
  ) +
  scale_color_manual(values = c("female" = "#d54c16", "male" = "#32779e")) +
  scale_x_discrete(expand = expansion(mult = 0.75)) +
  scale_y_continuous(
    breaks = seq(3000, 7000, by = 1000),
    limits = c(3000, 7000),
    expand = c(0, 0),
  ) +
  stat_compare_means(
    method = "t.test",
    method.args = list(alternative = "two.sided"),
    label = "p.signif",
    size = 3.5,
    comparisons = list(c("female", "male")),
    bracket.size = 0.5,
    label.y = 6500,
  ) +
  labs(
    title = "Weights of <span style='color:#d54c16'>female</span> and <span style='color:#32779e'>male</span> Gentoo Penguins",
    x = "",
    y = "Weight [g]",
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      size = 10,
      face = "bold",
      family = "roboto",
      color = "black"
    ),
    axis.text.y = element_text(
      size = 10,
      face = "bold",
      family = "roboto",
      color = "black",
      hjust = 1,
    ),
    axis.ticks.y = element_line(linewidth = 1),
    axis.line.y = element_line(linewidth = 0.75),
    axis.ticks.length.y = unit(0.15, "cm"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 10, r = 30, b = 10, l = 10),
    plot.title = element_markdown(
      hjust = 0.5,
      size = 13,
      face = "bold",
    ),
  )

ggsave(
  "plots/penguin_weight2.svg",
  plot = p,
  width = 500,
  height = 500,
  units = "px",
  dpi = 100,
)
