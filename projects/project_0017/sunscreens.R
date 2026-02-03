# Source
# https://www.choice.com.au/health-and-body/beauty-and-personal-care/skin-care-and-cosmetics/articles/sunscreen-test

library(tidyverse)
library(gridExtra)
library(showtext)

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

sunscreens <- read_csv("data/sunscreens.csv")

data <- sunscreens |>
  mutate(
    Uv_Range = factor(Uv_Range),
    Brand = factor(Brand),
    Product_Name = factor(Product_Name),
    Claimed_SPF = factor(Claimed_SPF),
    Meets_Claim = factor(Meets_Claim),
    display_name = paste(
      Brand,
      stringr::str_wrap(Product_Name, width = 30),
      sep = "\n"
    ),
    claimed_spf = as.numeric(gsub("\\+", "", Claimed_SPF)),
    diff_spf = Measured_SPF - claimed_spf,
    midpoint = (Measured_SPF + claimed_spf) / 2,
    measured_hjust = ifelse(Measured_SPF < claimed_spf, 1.7, -0.7),
    claimed_hjust = ifelse(Measured_SPF > claimed_spf, 1.7, -0.7),
  )

color_claim = "grey20"
color_okay = "#19647E"
color_not_okay = "#C73E1D"

ggplot(
  data,
  aes(y = reorder(display_name, -Measured_SPF))
) +
  geom_segment(
    aes(
      x = Measured_SPF,
      xend = claimed_spf,
      yend = reorder(display_name, -Measured_SPF)
    ),
    linewidth = 0.5
  ) +
  geom_point(
    aes(x = claimed_spf),
    color = color_claim,
    size = 3
  ) +
  geom_text(
    aes(
      x = claimed_spf,
      label = sprintf("%.0f", claimed_spf),
      hjust = claimed_hjust
    ),
    size = 3,
    color = color_claim
  ) +
  geom_point(
    aes(x = Measured_SPF, color = Meets_Claim),
    size = 3
  ) +
  geom_text(
    aes(
      x = Measured_SPF,
      label = sprintf("%.0f", Measured_SPF),
      hjust = measured_hjust,
      color = Meets_Claim,
    ),
    size = 3,
  ) +
  scale_color_manual(
    values = c("No" = color_not_okay, "Yes" = color_okay)
  ) +
  scale_x_continuous(
    breaks = seq(0, 80, by = 10),
    limits = c(0, 80),
    expand = c(0.05, 0),
  ) +

  labs(
    title = "Sunscreen SPF Claims vs Measured Results",
    subtitle = "16 out of 20 sunscreens failed to meet their SPF claims",
    x = NULL,
    y = NULL,
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8, family = "Roboto Condensed"),
    legend.position = "none",
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(
      color = "grey90",
      size = 0.25,
      linetype = "solid",
    ),
    panel.grid.major.y = element_line(
      color = "grey90",
      size = 0.25,
      linetype = "solid",
    ),
    plot.margin = margin(l = 40, r = 15, t = 15, b = 15),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    text = element_text(family = "Roboto"),
  )
