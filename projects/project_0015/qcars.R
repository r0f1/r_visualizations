# Columns:
#    origin: The country associated with the car brand
#    make The brand of the car, such as Toyota or Land Rover
#    model The specific type of car, such as Land Cruiser or Defender
#    length, width, and height: Length, width, and height of the car (in meters)
#    seating: Number of seats in the car
#    trunk: Capacity or volume of the trunk (in liters)
#    economy: Fuel economy of the car (in liters per 100 km)
#    horsepower: Car horsepower
#    price: Price of the car in 2025 Qatari riyals
#    mass: Mass of the car (in kg)
#    performance: Time to accelerate from 0 to 100 km/h (in seconds)
#    type: The type of the car, such as coupe, sedan, or SUV
#    enginetype: The type of engine: electric, hybrid, or petrol

library(tidyverse)
library(qatarcars)
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

cars = qatarcars

data = cars |>
  mutate(
    price = price * 0.24096,
    origin = factor(
      origin,
      levels = c(
        "PR China",
        "France",
        "Germany",
        "Italy",
        "Japan",
        "South Korea",
        "Sweden",
        "UK",
        "USA"
      ),
      labels = c(
        "China",
        "France",
        "Germany",
        "Italy",
        "Japan",
        "South Korea",
        "Sweden",
        "United Kingdom",
        "United States"
      )
    )
  )

p <- ggplot(
  data,
  aes(x = price, y = fct_rev(origin), color = enginetype, shape = enginetype)
) +
  ggbeeswarm::geom_beeswarm(size = 2.25, cex = 2.2) +
  scale_x_continuous(
    trans = "log10",
    breaks = c(10000, 100000, 1000000, 10000000),
    minor_breaks = c(
      seq(10000, 90000, 10000), # 10k, 20k, 30k, ..., 90k
      seq(100000, 900000, 100000), # 100k, 200k, ..., 900k
      seq(1000000, 9000000, 1000000) # 1M, 2M, ..., 9M
    ),
    labels = scales::label_number(big.mark = ",", accuracy = 1),
    guide = guide_axis(minor.ticks = TRUE),
    expand = expansion(add = c(0, 0)),
    limits = c(6000, 10000000),
  ) +
  scale_color_manual(
    values = c(
      "Electric" = "#08a018",
      "Hybrid" = "#a2af9f",
      "Petrol" = "#3f4a3c"
    )
  ) +
  labs(
    x = "Price (EUR)",
    y = "",
    title = "<span style='color:#3f4a3c;'>Petrol</span>, <span style='color:#a2af9f;'>Hybrid</span> or <span style='color:#08a018;'>Electric</span>?",
    subtitle = stringr::str_wrap(
      "Analysis of N = 99 vehicles across nine countries, comparing engine types and prices.",
      width = 100,
    ),
    caption = social_caption,
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    axis.line.x = element_line(linewidth = 0.25),
    axis.text.x = element_text(
      size = 11,
      color = "black",
    ),
    axis.text.y = element_text(
      size = 11,
      color = "black",
    ),

    axis.ticks.x = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.ticks.y = element_line(linewidth = 0),
    axis.title.x = element_text(
      size = 11,
      color = "black",
      margin = margin(t = 10)
    ),

    panel.grid.major.x = element_line(
      linetype = "solid",
      color = "grey50",
      size = 0.25,
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_line(
      linetype = "dotted",
      color = "grey50",
      size = 0.25,
    ),

    plot.title = ggtext::element_textbox_simple(
      face = "bold",
      size = 20,
      hjust = 0,
      margin = margin(l = -95, b = 10),
      lineheight = 1.2,
    ),
    plot.caption = ggtext::element_markdown(
      size = 10,
      color = "grey40",
    ),
    plot.margin = margin(15, 40, 15, 15),
    plot.subtitle = element_text(
      margin = margin(l = -95, b = 20),
      lineheight = 1.22,
    ),
    legend.position = "none",
    text = element_text(family = "Roboto"),
  ) +
  patchwork::plot_annotation(
    caption = "Source: R package {qatarcars}",
    theme = theme(
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
  here::here("projects", "project_0015", "qcars.svg"),
  plot = p,
  width = 650,
  height = 750,
  units = "px",
  dpi = 100,
)
