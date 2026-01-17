# Source
# https://github.com/rfordatascience/tidytuesday/tree/main/data/2023/2023-02-14

set.seed(4)

library(tidyverse)
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

age_gaps <- readr::read_csv("data/age_gaps.csv")

data <- age_gaps |>
    mutate(
        rel_type = case_when(
            (character_1_gender == "woman" &
                character_2_gender == "man") ~ -1,
            (character_1_gender == "man" &
                character_2_gender == "woman") ~ 1,
        ),
        age_difference = rel_type * age_difference,
    ) |>
    select(release_year, age_difference, rel_type, movie_name) |>
    drop_na()

n_unique_movies <- length(unique(data$movie_name))
min_year <- min(data$release_year)
max_year <- max(data$release_year)

smooth <- loess(age_difference ~ release_year, data)
first_smooth <- predict(smooth, newdata = data.frame(release_year = min_year))
last_smooth <- predict(smooth, newdata = data.frame(release_year = max_year))

theme_color1 = "#22577a"
theme_color2 = "#38a3a5"
theme_color3 = "#dd2d4a"
zero_color = "grey70"
color_background = "#ebf4f9ff"

p <- ggplot(
    data,
    mapping = aes(
        x = release_year,
        y = age_difference,
        color = factor(sign(age_difference)),
    )
) +
    geom_point(
        size = 0.75,
        position = position_jitter(width = 0.4, height = 0)
    ) +
    geom_smooth(
        aes(
            x = release_year,
            y = age_difference,
        ),
        data,
        method = "loess",
        se = FALSE,
        inherit.aes = FALSE,
        color = theme_color3,
    ) +
    scale_color_manual(
        values = c("-1" = theme_color2, "1" = theme_color1, "0" = zero_color),
        guide = "none"
    ) +
    scale_x_continuous(
        breaks = seq(1930, 2025, by = 10),
    ) +
    scale_y_continuous(
        breaks = seq(-50, 50, by = 10),
    ) +
    annotate(
        "text",
        x = 1950,
        y = 45,
        label = "Man Older Than Woman",
        color = theme_color1,
        size = 4.75,
        family = "Roboto Condensed",
    ) +
    annotate(
        "text",
        x = 1950,
        y = -15,
        label = "Women Older Than Man",
        color = theme_color2,
        size = 4.75,
        family = "Roboto Condensed",
    ) +
    annotate(
        "text",
        x = 1943,
        y = 18.5,
        label = "Average Age Difference",
        color = theme_color3,
        size = 3,
        family = "Roboto Condensed",
        angle = -3.75,
    ) +
    annotate(
        "text",
        x = min_year,
        y = first_smooth,
        label = round(first_smooth, 1),
        hjust = 1.2,
        color = theme_color3,
        family = "Roboto Condensed",
    ) +
    annotate(
        "text",
        x = max_year,
        y = last_smooth,
        label = paste0(round(last_smooth, 1), " years"),
        hjust = -0.1,
        color = theme_color3,
        family = "Roboto Condensed",
    ) +
    labs(
        x = "",
        y = "",
        title = "Age Differences in Movies",
        subtitle = "Analysis of 1132 romantic relationships in 830 movies from 1935 - 2022.",
        caption = social_caption,
        tag = "Years",
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal() +
    theme(
        axis.text = element_text(
            size = 11,
            color = "grey10",
        ),
        panel.background = element_rect(
            fill = color_background,
            color = color_background,
        ),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(
            linetype = "dotted",
            color = "grey30",
            size = 0.25
        ),
        plot.background = element_rect(
            fill = color_background,
            color = color_background,
        ),
        plot.caption = ggtext::element_markdown(
            size = 10,
            color = "grey40",
            margin = margin(t = 15),
            family = "Roboto",
        ),
        plot.margin = margin(15, 15, 15, 15),
        plot.tag = element_text(size = 10, hjust = 1),
        plot.tag.position = c(0.05, 0.87),
        plot.title = element_text(
            size = 20,
            margin = margin(l = -28, b = 8),
        ),
        plot.subtitle = element_text(
            size = 12,
            color = "grey20",
            margin = margin(l = -28, b = 20),
            lineheight = 1.22,
        ),
        legend.position = "none",
        text = element_text(
            family = "Roboto Condensed",
        ),
    ) +
    patchwork::plot_annotation(
        caption = "Source: TidyTuesday 2023 / Week 7",
        theme = theme(
            plot.background = element_rect(
                fill = color_background,
                color = color_background
            ),
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
    here::here("projects", "project_0013", "age_gaps.svg"),
    plot = p,
    width = 800,
    height = 630,
    units = "px",
    dpi = 100,
)
