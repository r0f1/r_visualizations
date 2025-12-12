library(ggplot2)
library(palmerpenguins)
library(showtext)
library(dplyr)
library(ggbeeswarm)

font_add_google("Roboto", "roboto")
showtext_auto()
showtext_opts(dpi = 100)

df <- penguins |>
  dplyr::filter(species == "Gentoo") |>
  dplyr::filter(sex %in% c("male", "female"))

ggplot(df, aes(x = sex, y = body_mass_g, color = sex)) +
  geom_beeswarm(size = 2, alpha = 0.6, cex = 1.5) +
  stat_summary(
    fun = median,
    geom = "crossbar",
    width = 0.25,
    color = "black",
    linewidth = 0.5
  ) +
  scale_color_manual(values = c("female" = "#E69F00", "male" = "#56B4E9")) +
  labs(
    title = "Comparing female and male Gentoos",
    x = "",
    y = "",
  ) +
  theme_minimal()

# ggsave(
#   "plots/penguin_weight.svg",
#   plot = p,
#   width = 600,
#   height = 400,
#   units = "px",
#   dpi = 100,
# )
