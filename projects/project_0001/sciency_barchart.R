library(ggchicklet)
library(ggplot2)
library(tibble)
library(dplyr)
library(showtext)

sysfonts::font_add(
  family = "Font Awesome 6 Brands",
  regular = "fontawesome/otfs/Font Awesome 7 Brands-Regular-400.otf"
)

font_add_google("Roboto")
showtext_auto()
showtext_opts(dpi = 100)

social_caption <- glue::glue(
  "<span style='font-family:\"Font Awesome 6 Brands\";'>&#xf09b; </span>",
  "<span> r0f1</span>",
)

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
    vjust = -0.65,
    size = 3.5,
    color = "black"
  ) +
  scale_fill_manual(values = c("#cae1ff", "#4682b4")) +
  scale_color_manual(values = c("#274967", "#274967")) +
  labs(
    x = "",
    y = "Accuracy",
    title = "Comparison of two models",
    caption = social_caption,
  ) +
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
      size = 10,
      angle = 45,
      hjust = 1,
      color = "black",
      vjust = 0.95,
    ),
    axis.text.y = element_text(
      size = 10,
      color = "black",
      family = "mono",
    ),
    axis.ticks = element_line(
      color = "black",
      size = 0.5
    ),
    axis.ticks.length.x = unit(0.15, "cm"),
    axis.ticks.length.y = unit(0.15, "cm"),
    axis.title = element_text(size = 10, color = "black"),
    axis.title.y = element_text(margin = margin(r = 15)),
    plot.title = element_text(
      size = 12,
      face = "bold",
      margin = margin(b = 20, l = -50),
      lineheight = 1.2,
    ),
    panel.grid = element_blank(),
    plot.caption = ggtext::element_markdown(
      size = 8,
      color = "grey20",
    ),
  )

ggsave(
  here::here("projects", "project_0001", "sciency_barchart.svg"),
  plot = p,
  width = 250,
  height = 350,
  units = "px",
  dpi = 100,
)
