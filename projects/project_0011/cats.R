# Source
# https://github.com/rfordatascience/tidytuesday/tree/main/data/2023/2023-01-31

set.seed(4)

library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(showtext)
library(ggtext)
library(ggbeeswarm)

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

# df <- tidytuesdayR::tt_load(2023, week = 5)$cats_uk_reference

cats <- readr::read_csv("data/cats.csv")

df <- cats |>
  select(age_years, hrs_indoors, n_cats, animal_sex) |>
  mutate(animal_sex = factor(animal_sex), hrs_indoors = factor(hrs_indoors)) |>
  drop_na()

n_levels <- length(levels(df$hrs_indoors))
rect_data <- data.frame(
  ymin = seq(0.5, n_levels - 0.5, by = 2),
  ymax = seq(1.5, n_levels + 0.5, by = 2)
)
# sex_colors <- c("m" = "#4A90E2", "f" = "#E94B8A")

p <- ggplot(df, aes(x = age_years, y = hrs_indoors)) +
  geom_beeswarm(
    alpha = 0.85,
    size = 1.5,
    cex = 1.95,
  ) +
  scale_x_continuous(
    breaks = seq(0, 16, by = 2),
  ) +
  # scale_color_manual(values = sex_colors) +
  labs(
    x = "Age",
    y = "",
    tag = "Number of hours\nspent indoors",
    title = "How much time do cats spend indoors?",
    caption = social_caption,
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title.x = element_text(
      size = 11,
      margin = margin(t = 10, b = 20),
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      linetype = "dashed",
      color = "grey20",
      size = 0.125
    ),
    panel.grid.major.y = element_line(
      linetype = "dashed",
      color = "grey20",
      size = 0.25
    ),
    plot.caption = element_markdown(
      size = 10,
      color = "grey40",
    ),
    plot.margin = margin(t = 15, r = 15, b = 0, l = 15),
    plot.tag = element_text(size = 11, hjust = 0),
    plot.tag.position = c(-0.0075, 0.89),
    plot.title = element_text(
      face = "bold",
      size = 18,
      hjust = 0,
      margin = margin(b = 50, l = -42)
    ),
    legend.position = "none",
    text = element_text(family = "Roboto"),
  ) +
  plot_annotation(
    caption = "Data: McDonald JL, Cole H (2020)",
    theme = theme(
      plot.caption = element_text(
        size = 10,
        hjust = 0,
        margin = margin(t = -10, l = -3),
        color = "grey40",
      ),
      text = element_text(family = "Roboto Condensed"),
    )
  )


ggsave(
  here::here("projects", "project_0011", "cats.svg"),
  plot = p,
  width = 750,
  height = 600,
  units = "px",
  dpi = 100,
)
