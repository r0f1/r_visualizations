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
    claimed_spf = as.numeric(gsub("\\+", "", Claimed_SPF)),
    diff_spf = Measured_SPF - claimed_spf,
    midpoint = (Measured_SPF + claimed_spf) / 2,
    measured_hjust = ifelse(Measured_SPF < claimed_spf, 1.7, -0.7),
    claimed_hjust = ifelse(Measured_SPF > claimed_spf, 1.7, -0.7),

    wrapped_name = stringr::str_wrap(Product_Name, width = 25),
    wrapped_name = stringr::str_replace_all(wrapped_name, "\\n", "<br>"),
    display_name = glue::glue("<b>{Brand}</b><br>{wrapped_name}"),
    display_name = forcats::fct_reorder(
      display_name,
      as.character(Brand),
      .desc = TRUE
    ),
  )


color_claim = "#807219"
color_claim_font = "#4d440f"
color_measured = "#FF9505"
color_measured_font = "#cc7704"

p <- ggplot(data, aes(x = Measured_SPF, y = display_name)) +
  geom_segment(
    data = data,
    aes(y = display_name, yend = display_name),
    x = -2,
    xend = 82,
    color = "grey90",
    linewidth = 0.25,
  ) +
  geom_segment(
    aes(xend = claimed_spf, yend = display_name),
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
    color = color_claim_font,
  ) +
  geom_point(
    aes(x = Measured_SPF),
    color = color_measured,
    size = 3
  ) +
  geom_text(
    aes(
      x = Measured_SPF,
      label = sprintf("%.0f", Measured_SPF),
      hjust = measured_hjust,
    ),
    color = color_measured_font,
    size = 3,
  ) +
  geom_text(
    aes(x = -5, label = Brand),
    hjust = 1,
    fontface = "bold",
    family = "Roboto Condensed",
    nudge_y = 0.2,
    size = 3.5
  ) +
  geom_text(
    aes(x = -5, label = stringr::str_wrap(Product_Name, width = 40)),
    hjust = 1,
    fontface = "plain",
    family = "Roboto Condensed",
    nudge_y = -0.2,
    size = 3,
    lineheight = 0.9
  ) +
  annotate(
    "text",
    x = 26,
    y = 20.5,
    label = "Measured SPF",
    hjust = 0.5,
    color = color_measured,
    size = 3,
    fontface = "bold",
  ) +
  annotate(
    "text",
    x = 50,
    y = 20.5,
    label = "Claimed SPF",
    hjust = 0.5,
    color = color_claim,
    size = 3,
    fontface = "bold",
  ) +
  annotate(
    "text",
    x = 80,
    y = 15,
    label = stringr::str_wrap(
      "Only 4 / 20 tested products reached or exceeded their claims.",
      width = 20
    ),
    hjust = 1,
    color = "grey20",
    size = 3,
  ) +
  scale_x_continuous(
    breaks = seq(0, 80, by = 10),
    limits = c(-40, 80),
    expand = c(0.05, 0),
    oob = scales::squish,
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = glue::glue(
      "<span style='color:{color_claim};'>Claimed</span> vs <span style='color:{color_measured};'>Measured</span> Sun Protection Factor"
    ),
    subtitle = "16 out of 20 sunscreens failed to meet their SPF claims",
    x = NULL,
    y = NULL,
    caption = social_caption,
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.grid.major.x = element_line(
      color = "grey90",
      size = 0.25,
      linetype = "solid",
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.caption = ggtext::element_markdown(
      size = 10,
      color = "grey40",
      margin = margin(t = 20),
    ),
    plot.margin = margin(l = 40, r = 15, t = 15, b = 15),
    plot.title = ggtext::element_textbox_simple(
      face = "bold",
      size = 20,
      hjust = 0,
      margin = margin(l = -5, b = 10),
      lineheight = 1.2,
    ),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    text = element_text(family = "Roboto"),
  ) +
  patchwork::plot_annotation(
    caption = "Source: CHOICE",
    theme = theme(
      plot.caption = element_text(
        size = 10,
        hjust = 0,
        margin = margin(t = -25),
        color = "grey40",
      ),
      text = element_text(family = "Roboto Condensed"),
    )
  )


# p

ggsave(
  here::here("projects", "project_0017", "sunscreens.svg"),
  plot = p,
  width = 650,
  height = 1000,
  units = "px",
  dpi = 100,
)
