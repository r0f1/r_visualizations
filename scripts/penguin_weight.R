# https://allisonhorst.github.io/palmerpenguins/

library(ggplot2)
library(palmerpenguins)
library(showtext)

font_add_google("Roboto", "roboto")
showtext_auto()
showtext_opts(dpi = 100)

p <- ggplot(penguins, aes(x = species, y = body_mass_g, fill = species)) +
  geom_violin(aes(color = species), trim = FALSE) +
  geom_boxplot(width = 0, outlier.shape = NA) +
  stat_summary(fun = median, geom = "point", size = 2, color = "black") +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = sprintf("%dg", after_stat(y))),
    vjust = -1,
    size = 3.5,
    fontface = "bold"
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Adelie" = "#018b8b",
      "Chinstrap" = "#ff8d06",
      "Gentoo" = "#b455f3"
    )
  ) +
  scale_color_manual(
    values = c(
      "Adelie" = "#018b8b",
      "Chinstrap" = "#ff8d06",
      "Gentoo" = "#b455f3"
    )
  ) +
  scale_y_continuous(
    breaks = seq(2000, 7000, by = 1000),
    limits = c(2000, 7000),
    expand = c(0, 0),
    labels = function(x) ifelse(x == max(x), paste0(x, "g"), x)
  ) +
  labs(
    title = "How much do penguins weigh?",
    x = "",
    y = "",
  ) +
  theme_minimal() +
  theme(
    axis.ticks.x = element_line(linewidth = 1),
    axis.line.x = element_line(linewidth = 0.75),
    axis.text.x = element_text(
      size = 10,
      face = "bold",
      family = "roboto",
      color = "black"
    ),
    axis.text.y = element_text(
      size = 12,
      face = "bold",
      family = "roboto",
      color = "black",
      hjust = 1,
      margin = margin(r = 10)
    ),
    axis.ticks.length.x = unit(0.15, "cm"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 10, r = 30, b = 10, l = 10),
    plot.title = element_text(
      hjust = 0.5,
      size = 14,
      face = "bold",
      family = "roboto",
    ),
  )

ggsave(
  "plots/penguin_weight.svg",
  plot = p,
  width = 600,
  height = 400,
  units = "px",
  dpi = 100,
)
