# Source: https://data.stackexchange.com/stackoverflow/query/1926661#resultSets

library(readr)
library(dplyr)
library(ggplot2)

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

# Create the time series plot
ggplot(data, aes(x = Month, y = Questions)) +
  geom_line(color = "#2E86AB", size = 1.2) +
  geom_point(color = "#A23B72", size = 3) +
  geom_vline(
    xintercept = as.numeric(raised),
    linetype = "dashed",
    color = "#E63946",
    size = 1
  ) +
  annotate(
    "text",
    x = raised,
    y = max(data$Questions, na.rm = TRUE) * 0.95,
    label = "Raised $6 million\n(May 2010)",
    hjust = 1.1,
    color = "#E63946",
    size = 4,
    fontface = "bold"
  ) +
  geom_vline(
    xintercept = as.numeric(acquisition),
    linetype = "dashed",
    color = "#E63946",
    size = 1
  ) +
  annotate(
    "text",
    x = acquisition,
    y = max(data$Questions, na.rm = TRUE) * 0.95,
    label = "Sold for $1.8 billion\n(June 2021)",
    hjust = 1.1,
    color = "#E63946",
    size = 4,
    fontface = "bold"
  ) +
  geom_vline(
    xintercept = as.numeric(chatgpt_release),
    linetype = "dashed",
    color = "#E63946",
    size = 1
  ) +
  annotate(
    "text",
    x = chatgpt_release,
    y = max(data$Questions, na.rm = TRUE) * 0.9,
    label = "ChatGPT Release\n(Nov 2022)",
    hjust = -0.1,
    color = "#E63946",
    size = 4,
    fontface = "bold"
  ) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Questions Over Time",
    subtitle = "July 2008 - December 2025",
    x = "Year",
    y = "Number of Questions"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank()
  )
