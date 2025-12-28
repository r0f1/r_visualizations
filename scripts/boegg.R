# https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-12-02/readme.md
#
# year        double   Year of Sechselauten festival.
# duration    double   Time elapsed from ignition of Boeoeg effigy until explosion, in minutes.
# tre200m0    double   Average air temperature 2 m above ground in degrees Celsius (monthly mean).
# tre200mn    double   Minimum air temperature 2 m above ground in degrees Celsius (absolute monthly minimum).
# tre200mx    double   Maximum air temperature 2 m above ground in degrees Celsius (absolute monthly maximum).
# sre000m0    double   Total sunshine duration in hours (monthly total).
# sremaxmv    double   Total sunshine duration as a percentage of the possible maximum.
# rre150m0    double   Total precipitation in mm (monthly total).
# record     logical   Years with average summer temperature above 19 degrees Celsius.

library(tidyverse)
library(lubridate)
library(showtext)
library(ggrepel)

font_add_google("Roboto Condensed")
showtext_opts(dpi = 100)
showtext_auto()

# data_path <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-02/sechselaeuten.csv"
data_path <- "data/sechselaeuten.csv"

df <- readr::read_csv(data_path) |>
  drop_na() |>
  mutate(
    year_date = make_date(as.character(as.integer(year)), 1, 1),
    year_label = as.integer(year)
  )

pearson_test <- cor.test(df$duration, df$tre200m0, method = "pearson")
spearman_test <- cor.test(df$duration, df$tre200m0, method = "spearman")

pearson_cor <- pearson_test$estimate
pearson_p <- pearson_test$p.value
spearman_cor <- spearman_test$estimate
spearman_p <- spearman_test$p.value

model <- lm(tre200m0 ~ duration, df)

df_model <- tibble(
  duration = df$duration,
  actual = df$tre200m0,
  predicted = predict(model),
  residuals = residuals(model),
  year_label = df$year_label
)

p <- ggplot(df_model, aes(x = duration, y = actual)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +

  geom_text_repel(
    aes(label = year_label),
    size = 3,
    color = "black",
    max.overlaps = Inf,
    box.padding = 0.35,
    point.padding = 0.3,
    segment.size = 0.2,
    segment.color = "black"
  ) +

  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = paste0(
      "Pearson ρ = ",
      round(pearson_cor, 3),
      " (p = ",
      format.pval(pearson_p, digits = 3, eps = 0.001),
      ")\n",
      "Spearman ρ = ",
      round(spearman_cor, 3),
      " (p = ",
      format.pval(spearman_p, digits = 3, eps = 0.001),
      ")"
    ),
    hjust = 1.05,
    vjust = 1.5,
    size = 4,
    color = "black",
    family = "Roboto Condensed"
  ) +

  scale_x_continuous(
    breaks = seq(0, 60, by = 15),
    limits = c(0, 60),
    expand = c(0, 0),
  ) +
  labs(
    title = "Snowman Burn Duration and Average Temperatures (N=65)",
    subtitle = "TidyTuesday 2025 / Week 48",
    x = "Snowman Burn Duration [mins]",
    y = "Average Air Temperature 2m Above Ground [°C]"
  ) +

  coord_cartesian(clip = "off") +

  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title.x = element_text(
      size = 12,
      color = "black",
      hjust = 1.015,
      margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      size = 12,
      color = "black",
      hjust = 1.025,
      margin = margin(r = 10)
    ),
    axis.ticks = element_line(linewidth = 0.25),
    axis.ticks.length = unit(0.25, "cm"),
    axis.line = element_line(linewidth = 0.25),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 15, 15, 15),
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0
    ),
    plot.subtitle = element_text(
      margin = margin(b = 25)
    ),
    legend.position = "none",
    text = element_text(family = "Roboto Condensed"),
  )

ggsave(
  "plots/boegg.svg",
  plot = p,
  width = 900,
  height = 900,
  units = "px",
  dpi = 100,
)
