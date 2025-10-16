source("ustensiles.R") # build raw dataset


#### Simulate spoon survival data (n = 200) using empirical properties
#### - Uses your original `spoons` and `visits` data as templates
#### - Extends visits from 2018-10-01 to 2022-10-01 by repeating observed intervals
#### - Fits a discrete-time logistic model to empirical events (v1..v15)
#### - Simulates 200 spoons by sampling covariates from original spoons
#### - Produces long and wide output suitable for survival analysis (coxph / survfit)

# Required packages
library(tidyverse)
library(survival)

# ---------------------------
# 1. Build original visit dates and inter-visit intervals
# ---------------------------
orig_vis_dates <- with(visits, as.Date(sprintf("%04d-%02d-%02d", year, month, day)))
visits <- visits %>% mutate(date = orig_vis_dates, visit_index = row_number())

# compute observed inter-visit day intervals (differences between consecutive original visits)
intervals <- diff(sort(orig_vis_dates))
# intervals is a difftime vector (days)
intervals_days <- as.integer(intervals)

# We'll extend the visits by repeating this sequence of intervals cyclically until 2022-10-01.
end_date_target <- as.Date("2022-10-01")
extended_dates <- orig_vis_dates
i <- 1
while (tail(extended_dates, 1) < end_date_target) {
  # cycle through intervals
  next_int <- intervals_days[((i-1) %% length(intervals_days)) + 1]
  extended_dates <- c(extended_dates, tail(extended_dates,1) + next_int)
  i <- i + 1
}
# Name visits v1...vN for the extended timeline
extended_vis_df <- tibble(
  visit_index = seq_along(extended_dates),
  date = as.Date(extended_dates),
  visit_name = paste0("v", visit_index)
)
# prune so final date equals or matches target exactly? We keep visits up to <= target
extended_vis_df <- extended_vis_df %>% filter(date <= end_date_target)


# ---------------------------
# 2. Convert original spoons wide presence to long event structure
#    We'll build a discrete-time dataset per spoon × visit (for visits 1..15)
# ---------------------------
# helper to pivot the original spoons presence to long
spoon_presence_long <- spoons %>%
  select(id, attractiveness, weight, similitude, type, starts_with("v")) %>%
  pivot_longer(cols = starts_with("v"), names_to = "visit_name", values_to = "present") %>%
  mutate(visit_index = as.integer(str_remove(visit_name, "^v")) ) %>%
  arrange(id, visit_index)

# For each spoon, find the first visit where present == 0 (if any)
first_absence <- spoon_presence_long %>%
  group_by(id) %>%
  summarize(first_zero = 
              if (any(present == 0)) min(visit_index[present == 0])
            else NA_integer_,
            .groups = "drop")

# We'll build "interval" rows: for each spoon, 
# visits 1..T where T is first_zero (event) or last visit (censor)
# We want a per-interval event indicator: event == 1 at the visit 
# where it becomes absent (i.e., first_zero)
spoon_dt <- 
  spoon_presence_long %>%
  left_join(first_absence, by = "id") %>%
  group_by(id) %>%
  mutate(
    last_obs = max(visit_index), # should be 15 for the original
    # keep rows up to censor/event
    include = if_else(is.na(first_zero),
                      visit_index <= last_obs, 
                      visit_index <= first_zero),
    event = if_else(!is.na(first_zero) & visit_index == first_zero & present == 0,
                    1L, 
                    0L),
    # If spoon never absent among v1..v15, it's right-censored (no event)
    censored = if_else(is.na(first_zero), 
                       1L, 
                       0L)
  ) %>%
  filter(include) %>%
  ungroup() %>%
  # ensure covariates remain
  select(id, attractiveness, weight, similitude, type, visit_index, present, event, censored)

# Quick check: number of events in the original sample
orig_events <- sum(spoon_dt$event)
orig_censored_spoons <- sum(spoon_dt %>% group_by(id) %>% summarize(any(event)==0) %>% pull())
message("Original number of events (disappearances within v1..v15): ", orig_events)
message("Original number of right-censored spoons (no disappearance by v15): ", orig_censored_spoons)

# ---------------------------
# 3. Fit a discrete-time logistic model (per-interval hazard)
#    Use visit_index as numeric covariate (to capture time trend without overfitting factor levels),
#    and the measured spoon-level covariates. This gives an empirical per-interval hazard model.
# ---------------------------
# Convert categorical covariates to factors (as in typical ecological models)
spoon_dt <- spoon_dt %>%
  mutate(
    weight = factor(weight, levels = c("light","normal","heavy")),
    similitude = factor(similitude),
    type = factor(type),
    attractiveness = as.integer(attractiveness)
  )

# Fit the model: event ~ attractiveness + weight + similitude + type + visit_index
hazard_model <- glm(event ~ attractiveness + weight + similitude + type + visit_index,
                    family = binomial(link = "logit"),
                    data = spoon_dt)

summary(hazard_model)

# ---------------------------
# 4. Simulate 200 spoons by sampling covariates from empirical distribution
#    and then simulate per-visit event outcomes using the fitted hazard model
# ---------------------------
set.seed(42) # reproducible

n_sim <- 200

# Sample spoon covariates with replacement from original spoons (to replicate trait distributions)
cov_sample <- spoons %>%
  select(attractiveness, weight, similitude, type) %>%
  slice_sample(n = n_sim, replace = TRUE) %>%
  mutate(id = paste0("sim", sprintf("%03d", seq_len(n_sim)))) %>%
  select(id, everything())

# Prepare to simulate across extended visits (extended_vis_df)
n_vis_extended <- nrow(extended_vis_df)
message("Extended monitoring visits: ", n_vis_extended, " (from ", min(extended_vis_df$date), " to ", max(extended_vis_df$date), ")")

# to use the hazard_model, we need visit_index values that match how the model was trained (1..15)
# We'll use the numeric visit index as predictor directly — for extended visits, continue visit_index = 1..N_extended

# Simulate discrete-time hazard per visit: P(event at visit t | alive at t-1) = plogis(Xb)
sim_records <- vector("list", n_sim)

for (i in seq_len(n_sim)) {
  row_cov <- cov_sample[i, ]
  alive <- TRUE
  event_occurred <- FALSE
  sim_rows <- list()
  for (t in seq_len(n_vis_extended)) {
    if (!alive) break
    # build a newdata row for prediction
    newd <- tibble(
      attractiveness = as.integer(row_cov$attractiveness),
      weight = factor(row_cov$weight, levels = levels(spoon_dt$weight)),
      similitude = factor(row_cov$similitude, levels = levels(spoon_dt$similitude)),
      type = factor(row_cov$type, levels = levels(spoon_dt$type)),
      visit_index = t
    )
    # Predicted probability of event this interval
    p_event <- predict(hazard_model, newdata = newd, type = "response")
    # Draw whether event occurs at this visit
    ev <- rbinom(1, 1, p_event)
    # Record presence: if no event yet, present = 1 at this visit, else for the visit of event present should be 0
    # We'll define presence as 1 if no event has happened up to that visit (inclusive)
    present <- if_else(ev == 1, 0L, 1L) # but careful: when event triggers, present == 0 at that visit
    # The ecologically sensible encoding: presence at visit t is 1 if not yet disappeared before t.
    # We want event==1 at the visit when present == 0 for the first time. Above sim does that.
    sim_rows[[t]] <- tibble(
      id = row_cov$id,
      attractiveness = row_cov$attractiveness,
      weight = row_cov$weight,
      similitude = row_cov$similitude,
      type = row_cov$type,
      visit_index = t,
      date = extended_vis_df$date[t],
      p_event = p_event,
      event = ev,
      # present: if event == 1 at this visit, then present == 0 (disappeared at observation)
      present = as.integer(ifelse(ev == 1, 0, 1))
    )
    if (ev == 1) {
      alive <- FALSE
      event_occurred <- TRUE
    }
    # if ev == 0, remain alive and continue
  }
  # If never event across all visits, mark censored = 1
  sim_df_i <- bind_rows(sim_rows)
  sim_df_i <- sim_df_i %>%
    group_by(id) %>%
    mutate(
      # if event occurred, event_visit is first event==1; otherwise NA
      event_visit = if_else(any(event == 1), min(visit_index[event == 1]), NA_integer_),
      censored = if_else(is.na(event_visit), 1L, 0L),
      # convert presence to reflect cumulative disappearance (if event happened earlier, subsequent visits absent)
      present = {
        if (!is.na(event_visit)) {
          as.integer(ifelse(visit_index < event_visit, 1L, ifelse(visit_index == event_visit, 0L, 0L)))
        } else {
          # never disappeared
          rep(1L, n())
        }
      }
    ) %>% ungroup()
  sim_records[[i]] <- sim_df_i
}

sim_long <- bind_rows(sim_records)

# derive a per-spoon summary: time (in visits) and status (1=event, 0=censored)
sim_summary <- sim_long %>%
  group_by(id, attractiveness, weight, similitude, type) %>%
  summarize(
    time_visits = if_else(any(event == 1), min(visit_index[event == 1]), max(visit_index)),
    event = if_else(any(event == 1), 1L, 0L),
    date_event = if_else(event == 1, date[which.min(ifelse(event == 1, visit_index, Inf))], as.Date(NA)),
    .groups = "drop"
  )

# Wide format: presence at each visit (v1..vN) for each simulated spoon
sim_wide <- sim_long %>%
  select(id, visit_name = paste0("v", visit_index), visit_index, present) %>%
  pivot_wider(names_from = visit_index, values_from = present, names_prefix = "v") %>%
  left_join(sim_summary, by = "id") %>%
  left_join(cov_sample, by = "id")

# ---------------------------
# 5. Diagnostics: compare empirical original vs simulated properties
# ---------------------------

# event rate (proportion that disappeared by end) - original versus simulated
orig_event_rate <- (spoon_dt %>% group_by(id) %>% summarize(any(event == 1)) %>% pull()) %>% mean()
sim_event_rate <- mean(sim_summary$event)
message("Original event rate (disappeared by v15): ", round(orig_event_rate,3))
message("Simulated event rate (disappeared by final extended visit): ", round(sim_event_rate,3))

# distribution of covariates : compare original vs sampled (should be similar because we sampled with replacement)
orig_cov_counts <- spoons %>% count(type)
sim_cov_counts <- cov_sample %>% count(type)
message("Original types counts:\n"); print(orig_cov_counts)
message("Simulated sampled types counts:\n"); print(sim_cov_counts)

# Example survival curve from simulated summary (time in visits)
surv_fit_sim <- survfit(Surv(time_visits, event) ~ 1, data = sim_summary)
print(summary(surv_fit_sim))

# Fit a Cox model on simulated data (example) using covariates
# (time_visits in visits; we treat visit intervals as equally spaced here)
cox_sim <- coxph(Surv(time_visits, event) ~ attractiveness + weight + similitude + type, data = sim_summary)
summary(cox_sim)

# ---------------------------
# 6. Outputs to return to user
#    - sim_long : long-format per-spoon × visit (presence, event, p_event)
#    - sim_summary : one-row-per-spoon (time_visits, event, covariates)
#    - sim_wide : wide presence matrix v1..vN + summary
# ---------------------------

# Make these available in your environment:
# sim_long, sim_summary, sim_wide, extended_vis_df, hazard_model

# Save outputs to disk if desired:
# write_csv(sim_summary, "sim_spoon_summary.csv")
# write_csv(sim_long, "sim_spoon_long.csv")
# write_csv(sim_wide, "sim_spoon_wide.csv")

message("Simulation complete. Objects available: sim_long, sim_summary, sim_wide, extended_vis_df, hazard_model.")

