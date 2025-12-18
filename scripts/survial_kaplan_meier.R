library(survival)
library(survminer)
library(tibble)
library(dplyr)

fit <- survfit(Surv(time, status) ~ sex, data = lung)
df <- tibble(
  time = fit$time,
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
      time = 0,
      n.risk = NA,
      n.event = NA,
      n.censor = NA,
      surv = 1,
      std.err = NA,
      lower = NA,
      upper = NA,
      strata = "sex=1",
    ),
    tibble(
      time = 0,
      n.risk = NA,
      n.event = NA,
      n.censor = NA,
      surv = 1,
      std.err = NA,
      lower = NA,
      upper = NA,
      strata = "sex=2",
    )
  ) |>
  mutate(
    strata = factor(
      strata,
      levels = c("sex=1", "sex=2"),
      labels = c("male", "female")
    )
  ) |>
  mutate(time_years = time / 365.25)

ggplot(df, aes(x = time_years, y = surv, color = strata)) +
  geom_step(linewidth = 1.2) +
  labs(x = "Time (Years)", y = "Survival Probability") +
  scale_color_manual(values = c("female" = "#ef8a62", "male" = "#67a9cf")) +
  scale_x_continuous(breaks = seq(0, max(df$time_years), by = 0.5)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      color = "gray90",
      linewidth = 0.3,
      linetype = "solid"
    ),
  )
