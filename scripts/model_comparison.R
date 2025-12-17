library(ggchicklet)
library(ggplot2)
library(tibble)
library(dplyr)
library(showtext)

font_add_google("Roboto", "roboto")
showtext_auto()
showtext_opts(dpi = 100)

df <- tibble(
  model = factor(
    c(1, 2),
    levels = c(1, 2),
    labels = c("Model 1", "Model 2")
  ),
  value = c(23.8, 65.5),
)

p <- ggplot(df, aes(x = model, y = value, fill = model, color = model)) +
  geom_chicklet() +
  geom_text(
    aes(label = sprintf("%.1f%%", value)),
    vjust = -0.75,
    size = 4.25,
    color = "black"
  ) +
  scale_fill_manual(values = c("#cae1ff", "#4682b4")) +
  scale_color_manual(values = c("#274967", "#274967")) +
  labs(x = NULL, y = "Accuracy", title = "Comparison of two models") +
  scale_x_discrete(expand = expansion(add = c(0.70, 0.70))) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    breaks = seq(0, 80, by = 20),
    expand = expansion(add = c(1.5, 0)),
    limits = c(0, 80),
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.line = element_line(color = "black", size = 0.5),
    axis.text.x = element_text(
      size = 13,
      angle = 45,
      hjust = 1,
      color = "black",
      vjust = 0.90,
    ),
    axis.text.y = element_text(
      size = 13,
      color = "black",
      family = "mono",
    ),
    axis.ticks = element_line(
      color = "black",
      size = 0.5
    ),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.ticks.length.y = unit(0.2, "cm"),
    axis.title = element_text(size = 13, color = "black"),
    axis.title.y = element_text(margin = margin(r = 25)),
    plot.title = element_text(
      size = 15,
      face = "bold",
      margin = margin(b = 25, l = -72),
      lineheight = 1.2,
    ),
    panel.grid = element_blank()
  )

ggsave(
  "plots/model_comparison.svg",
  plot = p,
  width = 300,
  height = 500,
  units = "px",
  dpi = 100,
)
