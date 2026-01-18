# Source
# https://github.com/rfordatascience/tidytuesday/tree/main/data/2022/2022-12-06

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

# elevators <- readr::read_csv(
#   'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-12-06/elevators.csv'
# )

elevators <- readr::read_csv("data/elevators.csv")

map_file <- "data/nyc_map.rds"

if (file.exists(map_file)) {
  nyc_map <- readRDS(map_file)
} else {
  key <- "YOUR_API_KEY"
  ggmap::register_stadiamaps(key)
  nyc_map <- ggmap::get_stadiamap(
    bbox = c(left = -74.3, bottom = 40.49, right = -73.7, top = 40.92),
    zoom = 12,
    maptype = "alidade_smooth"
  )
  saveRDS(nyc_map, map_file)
}

data <- elevators |>
  filter(LONGITUDE > -77) |>
  select(Borough, LATITUDE, LONGITUDE) |>
  drop_na() |>
  rename(c(long = LONGITUDE, lat = LATITUDE))


# Plot the map with your points
ggmap::ggmap(nyc_map) +
  geom_point(
    data = data,
    aes(x = long, y = lat, color = Borough),
    alpha = 0.7,
    size = 2
  ) +
  labs(title = "New York Locations by Type", color = "Type") +
  theme_minimal()

# ggplot() +
#   geom_polygon(
#     data = maps::map_data("county"),
#     aes(x = long, y = lat, group = group),
#     fill = "gray90",
#     color = "white"
#   ) +
#   geom_point(
#     data = data,
#     aes(x = long, y = lat, color = Borough),
#     alpha = 0.7,
#     size = 2
#   ) +
#   theme_minimal() +
#   labs(title = "Locations by Type", color = "Type") +
#   coord_fixed(1.3, xlim = c(-74.3, -73.7), ylim = c(40.5, 40.9))
