# https://www.kaggle.com/datasets/berkayalan/stack-overflow-annual-developer-survey-2024

library(tidyverse)
library(ggchicklet)
library(showtext)

font_add_google("Roboto", "roboto")
showtext_auto()
showtext_opts(dpi = 100)

df <- read_csv("data/survey_results_public.csv") |>
  rename_with(tolower) |>
  mutate(
    mainbranch = factor(
      mainbranch,
      levels = c(
        "I am a developer by profession",
        "I am not primarily a developer, but I write code sometimes as part of my work/studies",
        "I used to be a developer by profession, but no longer am",
        "I am learning to code",
        "I code primarily as a hobby",
        "None of these"
      ),
      labels = c(
        "Professional",
        "Partly",
        "Former",
        "Learner",
        "Hobbyist",
        "None"
      ),
      ordered = TRUE
    ),
  )

df_summary <- df |>
  count(mainbranch) |>
  mutate(
    percentage = n / sum(n) * 100
  )

p <- ggplot(df_summary, aes(x = fct_rev(mainbranch), y = percentage)) +
  geom_chicklet(fill = "royalblue") +
  coord_flip() +
  geom_text(
    aes(label = sprintf("%.1f%%", percentage)),
    position = position_stack(vjust = 1),
    hjust = -0.20,
    color = "black",
    family = "roboto",
    size = 4,
    fontface = "bold",
  ) +
  scale_x_discrete(expand = expansion(add = c(0.70, 0.70))) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.20))) +
  labs(
    title = "Which option best describes you today?",
    x = "",
    y = "",
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(
      family = "roboto",
      size = 12,
      color = "black",
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      family = "roboto",
      size = 15,
      face = "bold",
      margin = margin(b = 8, l = -72),
    ),
    plot.margin = margin(t = 10, r = 10, b = 1, l = 10)
  )

ggsave(
  "plots/developer_status.svg",
  plot = p,
  width = 500,
  height = 230,
  units = "px",
  dpi = 100,
)
