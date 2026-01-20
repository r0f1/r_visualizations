library(tidyverse)
library(ggmap)
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

elevators <- read_csv("data/elevators.csv")

maps <- paste0("data/nyc_map", 1:5, ".rds")
if (all(file.exists(maps))) {
  map_list <- lapply(maps, readRDS)
} else {
  ggmap::register_stadiamaps(Sys.getenv("STADIAMAPS_KEY"))
  bboxes <- list(
    c(-74.022, 40.697, -73.907, 40.882), # Manhattan
    c(-73.94, 40.79, -73.77, 40.915), # Bronx
    c(-74.05, 40.56, -73.83, 40.745), # Brooklyn
    c(-73.963, 40.54, -73.70, 40.80), # Queens
    c(-74.26, 40.49, -74.05, 40.652) # Staten Island
  )

  # Calculate width and height for each bbox
  widths <- sapply(bboxes, \(b) b[3] - b[1])
  heights <- sapply(bboxes, \(b) b[4] - b[2])

  max_width <- max(widths)
  max_height <- max(heights)

  # Adjust bboxes to have uniform dimensions
  bboxes <- lapply(bboxes, \(b) {
    current_width <- b[3] - b[1]
    current_height <- b[4] - b[2]
    center_long <- (b[1] + b[3]) / 2
    center_lat <- (b[2] + b[4]) / 2

    c(
      center_long - max_width / 2, # left
      center_lat - max_height / 2, # bottom
      center_long + max_width / 2, # right
      center_lat + max_height / 2 # top
    )
  })

  offset = 0.02
  bboxes[[1]] <- c(
    -74.022 - offset,
    40.697 - offset,
    -73.907 + offset,
    40.882 + offset
  )

  map_list <- lapply(bboxes, \(b) {
    get_stadiamap(
      bbox = c(left = b[1], bottom = b[2], right = b[3], top = b[4]),
      zoom = if (b[1] == -74.022 - offset) 13 else 11,
      maptype = "stamen_toner_lines",
    )
  })
  mapply(saveRDS, map_list, maps)
}

data <- elevators |>
  filter(LONGITUDE > -77) |>
  select(Borough, LATITUDE, LONGITUDE) |>
  drop_na() |>
  rename(long = LONGITUDE, lat = LATITUDE)

boroughs <- c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island")


plots <- map2(map_list, boroughs, \(m, b) {
  borough_data <- filter(data, Borough == b)
  n_elevators <- nrow(borough_data)

  if (b == "Manhattan") {
    bin_data <- ggplot_build(
      ggplot(borough_data, aes(long, lat)) +
        geom_bin2d(binwidth = c(0.0012 * 2, 0.0009 * 2))
    )$data[[1]]

    top_5_bins <- bin_data |>
      arrange(desc(count)) |>
      slice(1:5) |>
      select(x, y, count)
  }

  margin_axis_text <- case_when(
    b == "Manhattan" ~ 10,
    b == "Bronx" ~ 5,
    b == "Brooklyn" ~ 1,
    b == "Queens" ~ 1,
    b == "Staten Island" ~ 4
  )
  ggmap(m) +
    geom_bin2d(
      data = borough_data,
      aes(long, lat),
      binwidth = c(0.0012 * 2, 0.0009 * 2),
      alpha = 0.75,
    ) +
    scale_fill_viridis_b(
      alpha = 0.9,
      breaks = c(0, 10, 20, 50, 100, 200, 500),
      # guide = guide_coloursteps(label.vjust = -0.5),
      name = "Elevators",
      labels = function(x) ifelse(x == 500, "", x),
      limits = c(0, 500),
      oob = scales::squish,
      option = "plasma",
    ) +
    {
      if (b == "Manhattan") {
        geom_segment(
          x = top_5_bins[1, "x"],
          xend = top_5_bins[1, "x"] - 0.01,
          y = top_5_bins[1, "y"],
          yend = top_5_bins[1, "y"] + 0.01,
          label = top_5_bins[1, "count"],
          arrow = arrow(length = unit(0.2, "cm")),
          color = "grey10",
          size = 0.8
        )
      }
    } +
    {
      if (b == "Manhattan") {
        geom_label(
          x = top_5_bins[1, "x"] - 0.01,
          y = top_5_bins[1, "y"] + 0.01,
          label = top_5_bins[1, "count"],
          hjust = 1,
          vjust = 0,
          size = 3,
          color = "red",
          fill = "white"
        )
      }
    } +
    geom_label(
      x = Inf,
      y = -Inf,
      label = paste0(
        "N = ",
        format(n_elevators, big.mark = ".", scientific = FALSE)
      ),
      hjust = 1.1,
      vjust = if (b == "Manhattan") -0.4 else -0.6,
      fill = "white",
      label.size = 0,
      size = if (b == "Manhattan") 4.5 else 3.5,
    ) +
    labs(x = b, y = "") +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title.x = element_text(
        margin = margin(t = margin_axis_text)
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "white", color = NA),
      legend.justification = c(0, 0),
      legend.position = if (b == "Manhattan") c(0.05, 0.8) else "none",
      legend.title = element_text(margin = margin(b = 10)),
      plot.margin = margin(b = 10),
      text = element_text(family = "Roboto"),
    )
})

text_plot <- ggplot() +
  xlim(-1, 1) +
  ylim(-1, 1) +
  annotate(
    "text",
    x = -1,
    y = 0,
    label = "Elevators\nof New York",
    hjust = 0,
    size = 7,
    fontface = "bold",
  ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

library(patchwork)

# After creating plots and text_plot:
layout <- (plots[[2]] / plots[[3]] / plots[[4]] / plots[[5]] / text_plot) |
  (plots[[1]])


layout +
  plot_layout(widths = c(0.3, 0.7)) +
  theme(plot.margin = margin(15, 30, 15, 30))
