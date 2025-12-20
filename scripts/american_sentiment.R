# Data Source
# https://today.yougov.com/politics/articles/52137-how-americans-rate-donald-trumps-handling-of-52-issues

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(showtext)

font_add_google("Roboto", "roboto")
showtext_auto()
showtext_opts(dpi = 100)

df <- read_csv(
  "data/data-kTk51.csv",
  col_names = c(
    "issue",
    "very_important",
    "somewhat_important",
    "not_very_important",
    "unimportant"
  ),
  col_types = "ccccc",
  skip = 1
) |>
  filter(row_number() %% 2 == 0) |>
  mutate(across(
    -1,
    ~ as.numeric(gsub("%", "", .)) / 100
  )) |>
  arrange(desc(very_important)) |>
  mutate(rank = row_number()) |>
  mutate(
    issue = factor(
      issue,
      levels = issue,
    )
  ) |>
  pivot_longer(
    cols = c(
      very_important,
      somewhat_important,
      not_very_important,
      unimportant
    ),
    names_to = "importance",
    values_to = "percentage"
  ) |>
  mutate(
    importance = factor(
      importance,
      levels = c(
        "unimportant",
        "not_very_important",
        "somewhat_important",
        "very_important"
      ),
      labels = c(
        "Unimportant",
        "Not Very Important",
        "Somewhat Important",
        "Very Important"
      )
    )
  )

p <- ggplot(
  df,
  aes(x = percentage, y = reorder(issue, -rank), fill = importance)
) +
  geom_col(width = 0.9) +
  scale_x_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(
    values = c(
      "Unimportant" = "#ff412c",
      "Not Very Important" = "#ff8d80",
      "Somewhat Important" = "#c57fff",
      "Very Important" = "#9f29ff"
    )
  ) +
  geom_text(
    aes(label = sprintf("%.f", 100 * percentage)),
    position = position_stack(vjust = 0),
    hjust = -0.20,
    color = "white",
    size = 3.5,
  ) +
  labs(
    title = "Which issues do Americans say are important?",
    x = "Percentage",
    y = "",
    fill = "Importance Level"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 8.25),
    legend.box.margin = margin(l = -20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.25)
  ) +
  guides(fill = guide_legend(reverse = TRUE))

ggsave(
  "plots/american_sentiment.svg",
  plot = p,
  width = 720,
  height = 750,
  units = "px",
  dpi = 100,
)
