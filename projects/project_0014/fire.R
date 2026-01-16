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
    DISTANCE = cut(DISTANCE, breaks = seq(0, 200, by = 20)),
    FREQUENCY = cut(FREQUENCY, breaks = seq(0, 80, by = 10)),
  )
percentages <- data |>
  group_by(DISTANCE, FREQUENCY) |>
  summarise(
    pct_class1 = mean(CLASS == "1") * 100,
    .groups = "drop"
  )

ggplot(data, aes(DISTANCE, FREQUENCY, color = CLASS)) +
  geom_point(
    size = 0.75,
    position = position_jitter()
  ) +
  geom_text(
    data = percentages,
    aes(x = DISTANCE, y = FREQUENCY, label = sprintf("%.1f%%", pct_class1)),
    color = "black",
    size = 3,
    inherit.aes = FALSE,
    family = "Roboto Condensed"
  )
