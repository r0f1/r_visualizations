library(ggplot2)
library(boot)
library(gridExtra)
library(grid)
library(gghalves)
library(ggbeeswarm)
library(showtext)

font_add_google("Roboto")
font_add_google("Roboto Condensed")
showtext_auto()
showtext_opts(dpi = 100)

# Function to calculate BCa confidence interval
bca_ci <- function(data, statistic_func, conf_level = 0.95, R = 5000) {
  n <- length(data)

  # Calculate the observed statistic
  theta_hat <- statistic_func(data)

  # Bootstrap resampling
  set.seed(123)
  theta_star <- replicate(R, {
    boot_sample <- sample(data, n, replace = TRUE)
    statistic_func(boot_sample)
  })

  # Calculate bias-correction factor (z0)
  prop_less <- sum(theta_star < theta_hat) / R
  z0 <- qnorm(prop_less)

  # Calculate acceleration factor using jackknife
  theta_jack <- numeric(n)
  for (i in 1:n) {
    theta_jack[i] <- statistic_func(data[-i])
  }

  theta_jack_mean <- mean(theta_jack)
  numerator <- sum((theta_jack_mean - theta_jack)^3)
  denominator <- 6 * (sum((theta_jack_mean - theta_jack)^2))^1.5

  if (denominator == 0) {
    acc <- 0
  } else {
    acc <- numerator / denominator
  }

  # Calculate adjusted quantiles for BCa
  alpha <- (1 - conf_level) / 2
  z_alpha <- qnorm(c(alpha, 1 - alpha))

  adjusted_quantiles <- pnorm(z0 + (z0 + z_alpha) / (1 - acc * (z0 + z_alpha)))

  # Extract confidence interval from bootstrap distribution
  ci <- quantile(theta_star, probs = adjusted_quantiles)

  list(
    estimate = theta_hat,
    ci_lower = ci[1],
    ci_upper = ci[2],
    bootstrap_dist = theta_star
  )
}

# Generate example data for two groups
set.seed(42)
group1 <- rnorm(30, mean = 5, sd = 1.5)
group2 <- rnorm(30, mean = 7, sd = 1.8)

# Calculate summary statistics
mean1 <- mean(group1)
mean2 <- mean(group2)
sd1 <- sd(group1)
sd2 <- sd(group2)

# Calculate mean difference with BCa confidence interval
mean_diff_result <- bca_ci(
  data = c(group1, group2),
  statistic_func = function(x) {
    n1 <- length(group1)
    mean(x[(n1 + 1):length(x)]) - mean(x[1:n1])
  },
  conf_level = 0.95,
  R = 5000
)

# Prepare data for plotting
data_df <- data.frame(
  group = rep(c("Group 1", "Group 2"), each = 30),
  value = c(group1, group2),
  x_pos = rep(c(1, 2), each = 30)
)

# Add jitter for beeswarm effect
set.seed(123)
data_df$x_jitter <- data_df$x_pos + rnorm(nrow(data_df), 0, 0.05)

# Create mean difference distribution data
diff_dist_df <- data.frame(
  diff = mean_diff_result$bootstrap_dist,
  group = "Mean Difference"
)

# Plot 1: Two groups with beeswarm
p1 <- ggplot(data_df, aes(x = x_pos, y = value, , color = group)) +

  geom_beeswarm(alpha = 0.75, cex = 3, size = 2) +
  scale_color_manual(values = c("Group 1" = "#1f77b4", "Group 2" = "#ff7f0e")) +

  geom_segment(
    data = summary_df,
    aes(
      x = x_pos,
      xend = x_pos,
      y = mean_val - sd_val,
      yend = mean_val + sd_val,
    ),
    color = "black",
    linewidth = 0.5,
  ) +

  geom_point(
    data = summary_df,
    aes(x = x_pos, y = mean_val),
    color = "black",
    size = 3,
  ) +
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Group 1", "Group 2"),
    limits = c(0.5, 2.5)
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

p2 <- ggplot(diff_dist_df, aes(x = 0, y = diff)) +
  geom_half_violin(fill = "#2ca02c", alpha = 0.5, trim = TRUE) +
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    color = "gray60",
    linewidth = 0.3
  ) +

  annotate(
    "point",
    x = 0,
    y = mean_diff_result$estimate + mean1,
    color = "black",
    size = 3
  ) +

  annotate(
    "segment",
    x = -0.5,
    xend = 0,
    y = mean1,
    yend = mean1,
    linetype = "dashed",
    color = "gray40",
    linewidth = 0.5
  ) +
  annotate(
    "segment",
    x = -0.5,
    xend = 0,
    y = mean2,
    yend = mean2,
    linetype = "dashed",
    color = "gray40",
    linewidth = 0.5
  ) +

  scale_x_continuous(
    breaks = 0,
    labels = "Mean Difference",
    limits = c(-0.5, 0.5)
  ) +

  scale_y_continuous(position = "right") +

  labs(
    x = "",
    y = "Mean Difference",
  ) +

  # coord_cartesian(ylim = range(data_df$value)) +
  theme_minimal() +
  theme(
    axis.line = element_line(linewidth = 0.25),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_text(size = 12, color = "black"),
    axis.title.y.right = element_text(angle = 90),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 32.5, 0),
    text = element_text(family = "Roboto Condensed"),
  )


combined_plot <- grid.arrange(
  p1,
  p2,
  ncol = 2,
  widths = c(4, 1),
  top = textGrob(
    sprintf(
      "Estimation Plot with Bootstrap Confidence Intervals\nMean Difference = %.2f [95%% CI: %.2f, %.2f]",
      mean_diff_result$estimate,
      mean_diff_result$ci_lower,
      mean_diff_result$ci_upper
    ),
    gp = gpar(fontsize = 14, fontface = "bold")
  )
)
