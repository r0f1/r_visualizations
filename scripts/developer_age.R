library(tidyverse)
library(ggchicklet)
library(showtext)

font_add_google("Roboto", "roboto")
showtext_auto()
showtext_opts(dpi = 100)

df <- read_csv("data/survey_results_public.csv") |>
  rename_with(tolower) |>
  mutate(
    age = factor(
      age,
      levels = c(
        "Under 18 years old",
        "18-24 years old",
        "25-34 years old",
        "35-44 years old",
        "45-54 years old",
        "55-64 years old",
        "65 years or older",
        "Prefer not to say"
      ),
      ordered = TRUE
    )
  )


df_summary <- df |>
  count(age) |>
  mutate(
    percentage = n / sum(n) * 100
  )

p <- ggplot(df_summary, aes(x = fct_rev(age), y = percentage)) +
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
    title = "How old are you?",
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
      margin = margin(b = 8, l = -104),
    ),
    plot.margin = margin(t = 10, r = 10, b = 1, l = 10)
  )

ggsave(
  "plots/developer_age.svg",
  plot = p,
  width = 500,
  height = 320,
  units = "px",
  dpi = 100,
)
