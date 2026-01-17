# Source:
# https://www.kaggle.com/datasets/muratkokludataset/acoustic-extinguisher-fire-dataset
# Acoustic Extinguisher Fire Dataset

set.seed(4)

library(showtext)
library(tidyverse)

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


fire <- farff::readARFF("data/Acoustic_Extinguisher_Fire_Dataset.arff")
data <- fire |>
  mutate(
    CLASS = factor(CLASS),
    DISTANCE = cut(DISTANCE, breaks = c(seq(0, 160, by = 20), 190)),
    FREQUENCY = cut(FREQUENCY, breaks = c(seq(0, 60, by = 10), 75)),
  )

percentages <- data |>
  group_by(DISTANCE, FREQUENCY) |>
  summarise(
    pct_class1 = mean(CLASS == "1") * 100,
    .groups = "drop"
  )

color_background = "#f4f1f1"

p <- ggplot(data, aes(FREQUENCY, DISTANCE, color = CLASS)) +
  geom_point(
    size = 0.5,
    alpha = 0.5,
    position = position_jitter(),
  ) +
  scale_color_manual(values = c("#B62203", "#0071d4")) +
  shadowtext::geom_shadowtext(
    data = percentages,
    aes(FREQUENCY, DISTANCE, label = sprintf("%.1f%%", pct_class1)),
    bg.color = color_background,
    bg.r = 0.25,
    color = "black",
    inherit.aes = FALSE,
    family = "Roboto",
    fontface = "bold",
    size = 4,
  ) +
  labs(
    title = "Extinguishing Fire With Sound Waves",
    subtitle = stringr::str_wrap(
      "Points represent experiments (N = 17442) testing an acoustic fire extinguisher emitting varying frequencies at different distances from the fire source.",
      width = 80,
    ),
    x = "Emitted Sound Frequency [Hz]",
    y = stringr::str_wrap("Distance from the fire source [cm]", width = 20),
    caption = social_caption,
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    axis.text = element_text(
      size = 11,
      color = "black",
    ),
    axis.title.x = element_text(
      hjust = 0.95,
      margin = margin(t = 15),
    ),
    axis.title.y = element_text(
      vjust = 1.05,
      hjust = 0,
      margin = margin(r = -80),
      angle = 0,
    ),
    panel.background = element_rect(
      fill = color_background,
      color = color_background,
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_rect(
      fill = color_background,
      color = color_background,
    ),
    plot.caption = ggtext::element_markdown(
      size = 10,
      color = "grey40",
      margin = margin(t = 15),
      family = "Roboto",
    ),
    plot.margin = margin(15, 15, 15, 15),
    plot.title = element_text(
      size = 20,
      margin = margin(l = -45, b = 5),
    ),
    plot.subtitle = element_text(
      size = 12,
      color = "grey20",
      margin = margin(l = -45, b = 40),
      lineheight = 1.1,
    ),
    legend.position = "none",
    text = element_text(
      family = "Roboto Condensed",
    ),
  ) +
  patchwork::plot_annotation(
    caption = "Source: Koklu & Tastinar (2021)",
    theme = theme(
      plot.background = element_rect(
        fill = color_background,
        color = color_background
      ),
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
  here::here("projects", "project_0014", "fire.png"),
  plot = p,
  width = 650,
  height = 900,
  units = "px",
  dpi = 100,
)
