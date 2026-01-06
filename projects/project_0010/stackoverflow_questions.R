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

marker_color <- "grey20"
marker_linewidth <- 0.5
marker_offset <- 100

p <- ggplot(data, aes(x = Month, y = Questions)) +
  geom_area(fill = "#ffa225", alpha = 0.3) +
  geom_line(color = "#f48024", size = 1) +

  geom_segment(
    data = data.frame(
      x = raised,
      xend = raised,
      y = data$Questions[which.min(abs(data$Month - raised))],
      yend = max(data$Questions) * 0.7
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    color = marker_color,
    linewidth = marker_linewidth,
    inherit.aes = FALSE,
  ) +
  annotate(
    "text",
    x = raised + 500,
    y = max(data$Questions) * 0.8,
    label = "May 2010\nRaised $6 million",
    hjust = 1,
    color = "grey20",
    size = 4,
    family = "Roboto Condensed",
  ) +
  geom_segment(
    data = data.frame(
      x = acquisition,
      xend = acquisition,
      y = data$Questions[which.min(abs(data$Month - acquisition))],
      yend = max(data$Questions) * 1.1
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    color = marker_color,
    linewidth = marker_linewidth,
    inherit.aes = FALSE,
  ) +
  annotate(
    "text",
    x = acquisition - marker_offset,
    y = max(data$Questions) * 1.05,
    label = "June 2021\nSold for $1.8 billion",
    hjust = 1,
    color = "grey20",
    size = 4,
    family = "Roboto Condensed",
  ) +
  geom_segment(
    data = data.frame(
      x = chatgpt_release,
      xend = chatgpt_release,
      y = data$Questions[which.min(abs(data$Month - chatgpt_release))],
      yend = max(data$Questions) * 0.7
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    color = marker_color,
    linewidth = marker_linewidth,
    inherit.aes = FALSE,
  ) +
  annotate(
    "text",
    x = chatgpt_release + marker_offset,
    y = max(data$Questions) * 0.65,
    label = "Nov 2022\nChatGPT Release",
    hjust = 0,
    color = "grey20",
    size = 4,
    family = "Roboto Condensed",
  ) +
  scale_x_date(
    date_labels = "%Y",
    date_breaks = "2 years",
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "k"),
    breaks = seq(0, 200000, by = 50000),
    expand = expansion(mult = c(0, 0.1))
  ) +

  labs(
    title = "The Rise and Fall of Stack Overflow",
    subtitle = "Since 2021 the number of questions asked on Stack Overflow has been declining.\nThe advent of Large Language Models has been accelerating that trend.",
    x = "",
    y = "",
    tag = "Number of questions asked\nper month on Stack Overflow",
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
      color = "grey20",
      size = 0.25
    ),
    plot.caption = element_markdown(
      size = 10,
      color = "grey20",
    ),
    plot.margin = margin(t = 15, r = 15, b = 0, l = 15),
    plot.tag = element_text(size = 12, hjust = 0),
    plot.tag.position = c(0.012, 0.8),
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0,
      margin = margin(b = 8, l = -35)
    ),
    plot.subtitle = element_text(
      size = 12,
      color = "gray40",
      hjust = 0,
      margin = margin(b = 10, l = -35)
    ),
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

ggsave(
  here::here("projects", "project_0010", "stackoverflow_questions.svg"),
  plot = p,
  width = 700,
  height = 600,
  units = "px",
  dpi = 100,
)
