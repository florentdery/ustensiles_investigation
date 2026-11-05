#______________________________________________________________#
# Project: Office Cutlery Disappearance Monitoring (2018–2019)
# Author: Florent D.
#
# Script: Analytical Workflow
#
# Description:
#   This script explores patterns of utensil disappearance
#   from the office drawer using both observed and simulated data.
#
#   Main objectives:
#     1. Visualize changes in utensil composition over time.
#     2. Estimate spoon survival probabilities using
#        Kaplan-Meier methods.
#     3. Model disappearance risk using Cox proportional
#        hazards regression.
#     4. Examine potential effects of spoon characteristics
#        (type, weight, and distinctiveness) on survival.
#     5. Generate publication-quality figures.
#
# Input files:
#   - ustensiles.R
#   - simulate_dataset_from_real_data.R
#
# Outputs:
#   - km_curves.jpeg
#   - hr_ratios.jpeg
#   - (optional) utensil_composition_in_drawer_over_time.jpeg
#______________________________________________________________#


#______________________________________________________________
# INITIALIZATION ####
#______________________________________________________________
# Load observed utensil data and simulated survival dataset.
#
# Notes:
#   The simulation script produces a long-format dataset
#   suitable for time-to-event analyses.
#______________________________________________________________
--------------------------------------------------------------

source("ustensiles.R")
source("simulate_dataset_from_real_data.R")


#______________________________________________________________
# 1. UTENSIL COUNTS OVER TIME ####
#______________________________________________________________
#
# Purpose:
#   Track the evolution of utensil availability within the
#   drawer throughout the monitoring period.
#
# Method:
#   - Convert observation dates to Date format.
#   - Reshape data to long format.
#   - Aggregate utensils into broader categories:
#       * Knives
#       * Spoons
#       * Forks
#   - Plot counts through time.
#
# Interpretation:
#   Declining counts indicate disappearance events.
#______________________________________________________________

cuttelry %>%
  mutate(
    date_sampled = dmy(
      paste(day, month, year, sep = "-")
    )
  ) %>%
  pivot_longer(
    cols = butter_k:fork,
    values_to = "n",
    names_to = "ustensile"
  ) %>%
  mutate(
    ustensile_group = case_when(
      grepl("_k", ustensile) ~ "Knives",
      grepl("_s", ustensile) ~ "Spoons",
      TRUE ~ "Forks"
    )
  ) %>%
  group_by(date_sampled, ustensile_group) %>%
  summarise(
    tot_remaining = sum(n)
  ) %>%
  mutate(
    time_days = as.integer(
      date_sampled - dmy("01-10-2018")
    )
  ) %>%
  ggplot(
    aes(
      x = time_days,
      y = tot_remaining,
      color = ustensile_group
    )
  ) +
  geom_point() +
  geom_line() +
  scale_color_manual(
    "Utensil\nclass",
    values = c(
      "orange",
      "seagreen",
      "black"
    )
  ) +
  scale_x_continuous(
    "Time since monitoring start (days)",
    breaks = seq(0, 150, 25)
  ) +
  scale_y_continuous(
    "Number of utensils\nin drawer",
    breaks = seq(0, 35, 5)
  ) +
  theme_bw()


# Optional export
# ggsave(
#   "utensile_composition_in_drawer_over_time.jpeg",
#   width = 6,
#   height = 5,
#   dpi = 300
# )


#______________________________________________________________
# 2. KAPLAN-MEIER SURVIVAL ANALYSIS ####
#______________________________________________________________
#
# Purpose:
#   Estimate disappearance probabilities through time.
#
# Outcome:
#   Spoon disappearance from the drawer.
#
# Time variable:
#   Days since 01-10-2018.
#
# Grouping factors:
#   - Similitude
#   - Spoon type
#   - Weight class
#
# Notes:
#   A disappearance event occurs when a spoon is no longer
#   observed in subsequent drawer inspections.
#______________________________________________________________

require(ggsurvfit)

sim_long <-
  sim_long %>%
  mutate(
    time_disappearance =
      as.integer(date - dmy("01-10-2018")),
    weight =
      factor(
        weight,
        levels = c(
          "light",
          "heavy",
          "normal"
        )
      )
  )


#______________________________________________________________
# Global survival curve
#______________________________________________________________
# Provides a baseline estimate of spoon persistence
# independent of spoon characteristics.

glob <-
  survfit2(
    Surv(time_disappearance, event) ~ 1,
    data = sim_long
  ) |>
  ggsurvfit() +
  add_confidence_interval() +
  add_censor_mark()


#______________________________________________________________
# Survival curves by similitude
#______________________________________________________________

# Tests whether spoons that closely resemble others are
# less likely to disappear.
#______________________________________________________________

simi <-
  survfit2(
    Surv(time_disappearance, event) ~
      factor(similitude),
    data = sim_long
  ) |>
  ggsurvfit()


#______________________________________________________________
# Survival curves by spoon type
#______________________________________________________________
# Compares soup versus dessert spoons.
#______________________________________________________________

type <-
  survfit2(
    Surv(time_disappearance, event) ~
      factor(type),
    data = sim_long
  ) |>
  ggsurvfit()


#______________________________________________________________
# Survival curves by weight
#______________________________________________________________
# Evaluates whether heavier spoons remain longer
# in circulation.
#______________________________________________________________

wei <-
  survfit2(
    Surv(time_disappearance, event) ~
      factor(weight),
    data = sim_long
  ) |>
  ggsurvfit()


#______________________________________________________________
# Figure assembly
#______________________________________________________________
# Combines all Kaplan-Meier curves into a single panel.
#______________________________________________________________

require(patchwork)

(glob | type) /
  (simi | wei)

# ggsave(
#   "km_curves.jpeg",
#   width = 9,
#   height = 6,
#   dpi = 300
# )


#______________________________________________________________
# 3. EVENT SUMMARY DATASET ####
#______________________________________________________________
#
# Purpose:
#   Create one-record-per-spoon survival dataset suitable
#   for Cox regression.
#
# Output:
#   sim_sum
#
# Includes:
#   - Disappearance time
#   - Spoon characteristics
#   - Event indicator
#______________________________________________________________

dizz <-
  sim_long %>%
  mutate(
    time_disappearance =
      as.integer(date - dmy("01-10-2018"))
  ) |>
  arrange(id, time_disappearance) |>
  subset(event == 1) |>
  select(
    id,
    time_disappearance
  )

sim_sum <-
  sim_summary |>
  left_join(dizz) |>
  select(-event, -date_event) |>
  mutate(
    event = 1,
    carac =
      paste(
        type,
        similitude,
        sep = "_"
      )
  )

#______________________________________________________________
# 4. COX PROPORTIONAL HAZARDS MODELLING
#______________________________________________________________
#
# Purpose:
#   Model disappearance risk as a function of spoon
#   characteristics.
#
# Predictors:
#   - Weight class
#   - Combined type/similitude variable
#
# Response:
#   Time until disappearance.
#______________________________________________________________

cox_sim <- coxph(
  Surv(time_disappearance, event) ~
    weight + carac,
  data = sim_sum
)

summary(cox_sim)


#______________________________________________________________
# Proportional hazards diagnostics
#______________________________________________________________
# Evaluate proportional hazards assumptions using
# Schoenfeld residuals.
#______________________________________________________________

zph <- survival::cox.zph(cox_sim)

plot(zph)

# Additional investigation of weight-specific effects
plot(zph, var = "weightnormal")
plot(zph, var = "weightheavy")


#______________________________________________________________
# 5. TIME-VARYING COX MODEL
#______________________________________________________________
#
# Purpose:
#   Address non-proportional effects of weight.
#
# Method:
#   Weight coefficients are permitted to change with time
#   through log(time) interactions.
#______________________________________________________________

cox_tv <- coxph(
  Surv(time_disappearance, event) ~
    weight +
    carac +
    tt(I(weight == "heavy")) +
    tt(I(weight == "normal")),
  data = sim_sum,
  tt = function(x, t, ...) {
    x * log(t)
  }
)

summary(cox_tv)


#______________________________________________________________
# 6. HAZARD RATIO VISUALIZATION
#______________________________________________________________
#
# Purpose:
#   Visualize estimated hazard ratios and confidence
#   intervals from the final Cox model.
#
# Figures:
#   1. Time-varying effect of spoon weight
#   2. Hazard ratios for spoon type/similitude categories
#______________________________________________________________

# Extract coefficients
b <- coef(cox_tv)

# Variance-covariance matrix
V <- vcov(cox_tv)


#______________________________________________________________
# Helper function
#______________________________________________________________
# Calculates hazard ratio trajectories through time
# for time-dependent Cox effects.
#______________________________________________________________

get_tv_hr <- function(b_main, b_time, V, log_time) {
  
  log_hr <- b[b_main] + b[b_time] * log_time
  
  var_log_hr <-
    V[b_main, b_main] +
    log_time^2 * V[b_time, b_time] +
    2 * log_time * V[b_main, b_time]
  
  se <- sqrt(var_log_hr)
  
  tibble(
    log_hr = as.numeric(log_hr),
    hr = exp(log_hr),
    lower = exp(log_hr - 1.96 * se),
    upper = exp(log_hr + 1.96 * se)
  )
}


#______________________________________________________________
# Weight effect trajectories
#______________________________________________________________
# Estimates changing disappearance risk relative
# to light spoons.
#______________________________________________________________


# Time sequence for weight effects
hr_weight <- tibble(
  time = seq(
    min(sim_sum$time_disappearance),
    max(sim_sum$time_disappearance),
    length.out = 200
  )
) %>%
  mutate(
    log_time = log(time)
  )

# Heavy vs light
hr_heavy <- get_tv_hr(
  "weightheavy",
  "tt(I(weight == \"heavy\"))",
  V,
  hr_weight$log_time
)

# Normal vs light
hr_normal <- get_tv_hr(
  "weightnormal",
  "tt(I(weight == \"normal\"))",
  V,
  hr_weight$log_time
)

# Combine
hr_weight <- bind_rows(
  mutate(hr_heavy, time = hr_weight$time, weight = "Lourde"),
  mutate(hr_normal, time = hr_weight$time, weight = "Normale")
)


hrw=
  ggplot(
    hr_weight,
    aes(x = time, y = hr, colour = weight, fill = weight)
  ) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.5,
    colour = NA
  ) +
  geom_line(linewidth = 1) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    colour = "grey40"
  ) +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0,150,21),
                     labels = seq(0,150,21)/7)+
  labs(
    x = "Temps depuis l'introduction\n(semaines)",
    y = "",
    colour = "Poids",
    fill = "Poids"
  ) +
  scale_fill_manual(values  = c("gold", "purple"))+
  scale_color_manual(values  = c("orange", "purple4"))+
  theme_classic() +
  theme(
    legend.position = "right"
  )


#______________________________________________________________
# Characteristic-level hazard ratios
#______________________________________________________________
# Summarizes fixed effects from the Cox model.
#______________________________________________________________



hr_carac <- tibble(
  term = c("caracsoup_alike", "caracsoup_different")
) %>%
  rowwise() %>%
  mutate(
    log_hr = b[term],
    se = sqrt(V[term, term]),
    hr = exp(log_hr),
    lower = exp(log_hr - 1.96 * se),
    upper = exp(log_hr + 1.96 * se)
  ) %>%
  ungroup() %>%
  mutate(
    carac = recode(
      term,
      "caracsoup_alike" = "Soupe, similaire",
      "caracsoup_different" = "Soupe, différente"
    )
  )


hr_carac

carac = ggplot(
  hr_carac,
  aes(x = carac, y = hr, ymin = lower, ymax = upper)
) +
  geom_pointrange(linewidth = 0.7) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    colour = "grey40"
  ) +
  scale_y_log10() +
  labs(
    x = "Cuiller à :\n(Ref. = Cuiller à dessert différente )" ,
    y = "Risque de disparaître ± 95% IC"
  ) +
  theme_bw()

cowplot::plot_grid(carac, hrw, rel_widths = c(.4,.6), ncol = 2)
ggsave("hr_ratios.jpeg", width = 8, height = 4, dpi =300)




#______________________________________________________________
# Figure export
#______________________________________________________________

cowplot::plot_grid(
  carac,
  hrw,
  rel_widths = c(.4, .6),
  ncol = 2
)

ggsave(
  "hr_ratios.jpeg",
  width = 8,
  height = 4,
  dpi = 300
)


#______________________________________________________________
# 7. CONTRAST TESTS
#______________________________________________________________
#
# Purpose:
#   Compare disappearance risk between:
#
#     Soup spoon (similar)
#       vs
#     Soup spoon (different)
#
# Method:
#   Wald test using variance-covariance matrix.
#
# Output:
#   - Hazard ratio
#   - 95% confidence interval
#   - p-value
#______________________________________________________________

contrast <-
  b["caracsoup_alike"] -
  b["caracsoup_different"]

se_contrast <- sqrt(
  V["caracsoup_alike", "caracsoup_alike"] +
    V["caracsoup_different",
      "caracsoup_different"] -
    2 *
    V["caracsoup_alike",
      "caracsoup_different"]
)

z <- contrast / se_contrast

p_value <- 2 * pnorm(-abs(z))

exp(contrast)
exp(
  contrast +
    c(-1, 1) *
    1.96 *
    se_contrast
)
p_value
