library(gghalves)
library(ggbeeswarm)
library(showtext)
library(palmerpenguins)

font_add_google("Roboto Condensed")
showtext_auto()
showtext_opts(dpi = 100)

data <- penguins |>
  dplyr::filter(species == "Gentoo", !is.na(sex))

dabest_obj <- dabestr::load(
  data,
  x = sex,
  y = body_mass_g,
  idx = c("female", "male"),
)

dabest_result <- dabestr::mean_diff(dabest_obj)
mean_diff_est <- dabest_result$boot_result$difference

diff_dist_df <- tidyr::tibble(
  boot_dist = unlist(dabest_result$boot_result$bootstraps),
  group = "Mean Difference",
)

mean1 <- mean((data |> dplyr::filter(sex == "male"))$body_mass_g)
mean2 <- mean((data |> dplyr::filter(sex == "female"))$body_mass_g)
min_mean <- min(mean1, mean2)

# Plot 1: Two groups with beeswarm
p1 <- ggplot(data, aes(x = sex, y = body_mass_g, color = sex)) +
  geom_beeswarm(alpha = 0.75, cex = 2.5, size = 2) +

  scale_color_manual(
    values = c("female" = "salmon3", "male" = "dodgerblue3")
  ) +

  geom_hline(
    yintercept = mean1,
    linetype = "solid",
    color = "gray60",
    linewidth = 0.3
  ) +

  geom_hline(
    yintercept = mean2,
    linetype = "solid",
    color = "gray60",
    linewidth = 0.3
  ) +

  stat_summary(
    fun.data = function(y) {
      data.frame(
        y = mean(y),
        ymin = mean(y) - sd(y),
        ymax = mean(y) + sd(y)
      )
    },
    geom = "errorbar",
    color = "black",
    linewidth = 0.5,
    width = 0
  ) +

  stat_summary(
    fun = mean,
    geom = "point",
    color = "black",
    size = 3
  ) +

  labs(
    x = "",
    y = "Value",
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title.y = element_text(size = 12, color = "black"),
    axis.ticks = element_line(linewidth = 0.25),
    axis.ticks.length = unit(0.25, "cm"),
    axis.line = element_line(linewidth = 0.25),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 0, 10, 10),
    legend.position = "none",
    text = element_text(family = "Roboto Condensed"),
  )

# Plot 2: Effect size distribution
p2 <- ggplot(diff_dist_df, aes(x = 0, y = boot_dist)) +
  geom_half_violin(fill = "#2ca02c", alpha = 0.5, trim = TRUE, color = NA) +
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    color = "gray60",
    linewidth = 0.3
  ) +
  geom_hline(
    yintercept = mean_diff_est,
    linetype = "solid",
    color = "gray60",
    linewidth = 0.3
  ) +

  stat_summary(
    fun.data = function(y) {
      data.frame(
        y = mean(y),
        ymin = mean(y) - sd(y),
        ymax = mean(y) + sd(y)
      )
    },
    geom = "errorbar",
    color = "black",
    linewidth = 0.5,
    width = 0
  ) +

  stat_summary(
    fun = mean,
    geom = "point",
    color = "black",
    size = 3
  ) +

  scale_x_continuous(
    breaks = 0,
    limits = c(-0.5, 0.5),
  ) +
  scale_y_continuous(
    position = "right",
    breaks = seq(0, 1000, by = 200),
    limits = function(limits) {
      c(min(data$body_mass_g) - min_mean, max(data$body_mass_g) - min_mean)
    }
  ) +
  coord_cartesian(
    ylim = c(min(data$body_mass_g) - min_mean, max(data$body_mass_g) - min_mean)
  ) +
  labs(
    x = "",
    y = "Mean Difference",
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(linewidth = 0.25),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.ticks.y = element_line(linewidth = 0.25),
    axis.ticks.length = unit(0.25, "cm"),
    axis.title.y = element_text(size = 12, color = "black"),
    axis.title.y.right = element_text(angle = 90),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 32.5, 0),
    text = element_text(family = "Roboto Condensed"),
  )

combined_plot <- gridExtra::grid.arrange(
  p1,
  p2,
  ncol = 2,
  widths = c(3, 1),
  top = grid::textGrob(
    sprintf(
      "Estimation Plot with Bootstrap Confidence Intervals\nMean Difference = %.2f [95%% CI: %.2f, %.2f]",
      mean_diff_est,
      dabest_result$boot_result$bca_ci_low,
      dabest_result$boot_result$bca_ci_high
    ),
    gp = grid::gpar(fontsize = 14, fontfamily = "Roboto Condensed")
  )
)

ggsave(
  "plots/effect_size_visualization.svg",
  plot = combined_plot,
  width = 600,
  height = 600,
  units = "px",
  dpi = 100,
)
