library(survival)
library(survminer)
library(tibble)
library(tidyr)
library(dplyr)
library(patchwork)
library(pammtools)
library(showtext)
library(sysfonts)


font_add_google("Roboto Condensed")
showtext_auto()
showtext_opts(dpi = 100)


fit <- survfit(Surv(time, status) ~ sex, data = lung)
df <- tibble(
  time_days = fit$time,
  n.risk = fit$n.risk,
  n.event = fit$n.event,
  n.censor = fit$n.censor,
  surv = fit$surv,
  std.err = fit$std.err,
  lower = fit$lower,
  upper = fit$upper,
  strata = rep(names(fit$strata), fit$strata)
) |>
  bind_rows(
    tibble(
      time_days = 0,
      n.risk = NA,
      n.event = NA,
      n.censor = NA,
      surv = 1,
      std.err = NA,
      lower = 1,
      upper = 1,
      strata = "sex=1",
    ),
    tibble(
      time_days = 0,
      n.risk = NA,
      n.event = NA,
      n.censor = NA,
      surv = 1,
      std.err = NA,
      lower = 1,
      upper = 1,
      strata = "sex=2",
    )
  ) |>
  mutate(
    strata = factor(
      strata,
      levels = c("sex=1", "sex=2"),
      labels = c("Men", "Women")
    )
  ) |>
  mutate(time_months = time_days / 30.44)

df_censor <- df |>
  filter(n.censor > 0)

label_data <- tibble(
  strata = factor(c("Men", "Women"), levels = c("Men", "Women")),
  time_months = c(3, 16.5),
  surv = c(0.65, 0.65)
)

df_median <- surv_median(fit) |>
  as_tibble() |>
  mutate(
    strata = factor(
      strata,
      levels = c("sex=1", "sex=2"),
      labels = c("Men", "Women")
    ),
    median_time = median / 30.44 # convert days to months
  )

median_text <- paste0(
  "Median Survival:\n",
  "Men = ",
  round(df_median$median_time[df_median$strata == "Men"], 1),
  " months\n",
  "Women = ",
  round(df_median$median_time[df_median$strata == "Women"], 1),
  " months"
)

p1 <- ggplot(
  df,
  aes(x = time_months, y = surv, color = strata, fill = strata)
) +
  geom_stepribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.3,
  ) +
  geom_step(linewidth = 1.2) +
  geom_point(
    data = df_censor,
    aes(x = time_months, y = surv),
    shape = 3,
    size = 3,
    stroke = 1.2,
    alpha = 0.7,
  ) +
  geom_text(
    data = label_data,
    aes(x = time_months, y = surv, label = strata, color = strata),
    # fontface = "bold",
    family = "Roboto Condensed",
    size = 5
  ) +
  geom_hline(
    yintercept = 0.5,
    color = "black",
    linetype = "dashed",
  ) +
  geom_label(
    x = -1,
    y = 0.45,
    color = "black",
    family = "Roboto Condensed",
    fill = "white",
    hjust = 0,
    label = median_text,
    label.size = NA,
    size = 4.5,
    vjust = 1,
  ) +
  labs(x = "Time [Months]", y = "Survival probability") +
  scale_color_manual(values = c("Women" = "#ef8a62", "Men" = "#67a9cf")) +
  scale_fill_manual(values = c("Women" = "#ef8a62", "Men" = "#67a9cf")) +
  scale_x_continuous(
    breaks = seq(0, max(df$time_months), by = 6),
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25),
    labels = scales::number_format(accuracy = 0.01),
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(linewidth = 0.75),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.1, "cm"),
    axis.text = element_text(
      size = 12,
      color = "black",
    ),
    axis.title = element_text(size = 14, color = "black"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.major.x = element_line(
      color = "gray90",
      linewidth = 0.3,
      linetype = "solid",
    ),
    panel.grid.minor = element_blank(),
    text = element_text(family = "Roboto Condensed"),
  )

risk_data <- df |>
  filter(!is.na(n.risk)) |>
  mutate(time_group = floor(time_months / 6) * 6) |>
  group_by(strata, time_group) |>
  slice(1) |>
  ungroup() |>
  group_by(strata) |>
  mutate(
    initial_n = first(n.risk),
    pct_at_risk = round((n.risk / initial_n) * 100, 1),
    label = paste0(n.risk, " (", pct_at_risk, "%)")
  ) |>
  ungroup() |>
  select(time_months = time_group, strata, label) |>
  complete(
    time_months = seq(0, max(time_months), by = 6),
    strata,
    fill = list(label = "0 (0%)")
  ) |>
  group_by(strata) |>
  fill(label, .direction = "down") |>
  ungroup()

# Risk table plot
p2 <- ggplot(
  risk_data,
  aes(x = time_months, y = strata, label = label, color = strata)
) +
  geom_text(size = 3.5, fontface = "bold") +
  scale_color_manual(values = c("Women" = "#ef8a62", "Men" = "#67a9cf")) +
  scale_x_continuous(
    breaks = seq(0, max(df$time_months), by = 6),
    limits = c(0, max(df$time_months))
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(0, 5.5, 5.5, 5.5)
  ) +
  labs(x = "Time [Months]")

# Combine plots
p1 / p2 + plot_layout(heights = c(4, 1))
