library(ggplot2)
library(palmerpenguins)
library(showtext)
library(dplyr)
library(ggbeeswarm)
library(ggnewscale)
library(ggtext)


# font_add_google("Roboto", "roboto")
# showtext_auto()
# showtext_opts(dpi = 100)

df <- penguins |>
  dplyr::filter(species == "Gentoo") |>
  dplyr::filter(sex %in% c("male", "female"))

ggplot(df, aes(x = sex, y = body_mass_g, color = sex)) +
  geom_beeswarm(size = 2, alpha = 0.6, cex = 1.5) +
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
  scale_y_continuous(
    breaks = seq(3000, 7000, by = 1000),
    limits = c(3000, 7000),
    expand = c(0, 0),
    # labels = function(x) ifelse(x == max(x), paste0(x, "g"), x)
  ) +
  labs(
    title = "Weights of <span style='color:#d54c16'>female</span> and <span style='color:#32779e'>male</span> Gentoos",
    x = "",
    y = "Weight [g]",
  ) +
  theme_minimal() +
  theme(
    axis.ticks.y = element_line(linewidth = 1),
    axis.line.y = element_line(linewidth = 0.75),
    axis.ticks.length.y = unit(0.15, "cm"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 10, r = 30, b = 10, l = 10),
    plot.title = element_markdown(
      hjust = 0.5,
      size = 14,
      face = "bold",
      # family = "",
    ),
  )

# ggsave(
#   "plots/penguin_weight.svg",
#   plot = p,
#   width = 600,
#   height = 400,
#   units = "px",
#   dpi = 100,
# )
