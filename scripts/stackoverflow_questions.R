# Source: https://data.stackexchange.com/stackoverflow/query/1926661

library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(showtext)
library(ggtext)

sysfonts::font_add(
  family = "Font Awesome 6 Brands",
  regular = "fontawesome/otfs/Font Awesome 7 Brands-Regular-400.otf"
)
font_add_google("Roboto Condensed")
showtext_auto()
showtext_opts(dpi = 100)

social_caption <- glue::glue(
  "<span style='font-family:\"Font Awesome 6 Brands\";'>&#xf09b; </span>",
  "<span> r0f1</span>",
)

# Launch of SO was Sept 15, 2008.
# Jan 2026 is incomplete, therefore filtered out.
data <- read_csv(
  "data/stackoverflow_questions.csv",
  col_types = cols(
    Month = col_date(format = "%Y-%m-%d %H:%M:%S"),
    Questions = col_integer()
  )
) |>
  filter(Month >= as.Date("2008-08-15") & Month <= as.Date("2025-12-31"))

raised <- as.Date("2010-05-01")
acquisition <- as.Date("2021-06-01")
chatgpt_release <- as.Date("2022-11-30")

p <- ggplot(data, aes(x = Month, y = Questions)) +
  geom_area(fill = "#ffa225", alpha = 0.5) +
  geom_line(color = "#f48024", size = 0.5) +
  geom_vline(
    xintercept = raised,
    color = "black",
    size = 0.25,
  ) +
  annotate(
    "text",
    x = raised,
    y = max(data$Questions) * 0.95,
    label = "Raised $6 million\n(May 2010)",
    hjust = 1.1,
    color = "#E63946",
    size = 4,
    fontface = "bold"
  ) +
  geom_vline(
    xintercept = acquisition,
    color = "black",
    size = 0.25,
  ) +
  annotate(
    "text",
    x = acquisition,
    y = max(data$Questions) * 0.95,
    label = "Sold for $1.8 billion\n(June 2021)",
    hjust = 1.1,
    color = "#E63946",
    size = 4,
    fontface = "bold"
  ) +
  geom_vline(
    xintercept = chatgpt_release,
    color = "black",
    size = 0.25,
  ) +
  annotate(
    "text",
    x = chatgpt_release,
    y = max(data$Questions) * 0.9,
    label = "ChatGPT Release\n(Nov 2022)",
    hjust = -0.1,
    color = "#E63946",
    size = 4,
    fontface = "bold"
  ) +
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "2 years",
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "k"),
    breaks = seq(0, 200000, by = 50000),
    expand = expansion(mult = c(0, 0.25))
  ) +

  labs(
    title = "Questions Over Time",
    subtitle = "July 2008 - December 2025",
    x = "",
    y = "",
    tag = "Number of questions asked on\nStack Overflow per month",
    caption = social_caption,
  ) +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.ticks.x = element_line(size = 0.3),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      linetype = "dashed",
      color = "black",
      size = 0.25
    ),
    plot.caption = element_markdown(
      size = 10,
      color = "grey20",
      margin = margin(t = 10),
    ),
    plot.margin = margin(t = 15, r = 15, b = 0, l = 15),
    plot.tag = element_text(size = 13, hjust = 0),
    plot.tag.position = c(0.012, 0.84),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    text = element_text(family = "Roboto Condensed"),
  ) +
  plot_annotation(
    caption = "Source: Stack Exchange",
    theme = theme(
      plot.caption = element_text(
        size = 10,
        hjust = 0,
        margin = margin(t = -10, l = 15),
        color = "grey20",
      ),
      text = element_text(family = "Roboto Condensed"),
    )
  )

p

# ggsave(
#   "plots/stackoverflow_questions.svg",
#   plot = p,
#   width = 700,
#   height = 600,
#   units = "px",
#   dpi = 100,
# )
