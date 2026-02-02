# Source
# https://www.choice.com.au/health-and-body/beauty-and-personal-care/skin-care-and-cosmetics/articles/sunscreen-test

library(tidyverse)
library(gridExtra)
library(showtext)
library(ggh4x)

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
    claimed_spf = as.numeric(gsub("\\+", "", Claimed_SPF))
  )

spf_breaks <- c(4, 10, 15, 20, 30, 40, 50, 66.666, 80)
uv_blocking <- round((1 - 1 / pmax(spf_breaks, 1)) * 100, 2)

data_spf <- data |>
  mutate(
    panel = "SPF Value",
    x_value = Measured_SPF,
    x_value_claimed = claimed_spf
  )

data_uv <- data |>
  mutate(
    panel = "UV Blocking (%)",
    x_value = (1 - 1 / pmax(Measured_SPF, 1)) * 100,
    x_value_claimed = (1 - 1 / pmax(claimed_spf, 1)) * 100
  )

data_combined <- bind_rows(data_uv, data_spf) |>
  mutate(panel = factor(panel, levels = c("UV Blocking (%)", "SPF Value")))

data_combined <- data_combined |>
  mutate(
    diff_spf = Measured_SPF - claimed_spf,
    diff_uv = x_value - x_value_claimed,
    midpoint = (x_value + x_value_claimed) / 2
  )

ggplot(
  data_combined,
  aes(y = reorder(display_name, -Measured_SPF))
) +
  geom_segment(
    aes(
      x = x_value,
      xend = x_value_claimed,
      yend = reorder(display_name, -Measured_SPF)
    ),
    linewidth = 1.5
  ) +
  geom_point(aes(x = x_value), color = "#FF6B35", size = 3) +
  geom_point(aes(x = x_value_claimed), color = "#4ECDC4", size = 3) +
  geom_text(
    aes(
      x = midpoint,
      label = ifelse(
        panel == "SPF Value",
        sprintf("%+.0f", diff_spf),
        sprintf("%+.1f%%", diff_uv)
      )
    ),
    size = 2.5,
    vjust = -0.5,
    color = "gray30"
  ) +
  geom_text(
    aes(
      x = x_value,
      label = ifelse(
        panel == "SPF Value",
        sprintf("%.0f", x_value),
        sprintf("%.1f%%", x_value)
      )
    ),
    size = 2.5,
    hjust = 1.2,
    color = "#FF6B35"
  ) +
  # Add text labels for claimed values (to the right of teal points)
  geom_text(
    aes(
      x = x_value_claimed,
      label = ifelse(
        panel == "SPF Value",
        sprintf("%.0f", x_value_claimed),
        sprintf("%.1f%%", x_value_claimed)
      )
    ),
    size = 2.5,
    hjust = -0.2,
    color = "#4ECDC4"
  ) +
  facet_wrap(~panel, scales = "free_x") +
  labs(
    title = "Sunscreen SPF Claims vs Measured Results",
    subtitle = "16 out of 20 sunscreens failed to meet their SPF claims",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    strip.text = element_text(face = "bold", size = 10)
  )
