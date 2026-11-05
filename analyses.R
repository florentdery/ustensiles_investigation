# Analyses cuttlery and spoons ####

source("ustensiles.R")
source("simulate_dataset_from_real_data.R")


# 1. ustensile counts in drawer ####

cuttelry %>% 
  mutate(date_sampled = dmy(paste(day, month, year, sep = "-"))) %>% 
  pivot_longer(cols = butter_k:fork, 
               values_to = "n",
               names_to = "ustensile"
              ) %>% 
  mutate(ustensile_group = case_when(grepl("_k", ustensile)~"Knives",
                                grepl("_s", ustensile)~"Spoons",
                                T~"Forks"
  )) %>% 
  group_by(date_sampled, ustensile_group) %>% 
  summarise(tot_remaining = sum(n)) %>% 
  mutate(time_days = as.integer(date_sampled- dmy("01-10-2018"))) %>% 
  ggplot(aes(x = time_days, y = tot_remaining, color = ustensile_group))+
  geom_point()+
  geom_line()+
  scale_color_manual("Ustensile\nclass", 
                     values = c("orange", "seagreen", "black"))+
  scale_x_continuous("Temps depuis début du suivi (jours)",
                     breaks = seq(0,150, 25))+
  scale_y_continuous("Nombre d'ustensiles\ndans le tiroir", 
                     seq(0,35,5))+
  theme_bw()+
  theme()


# ggsave("ustensile_composition_in_drawer_over_time.jpeg",
#        width = 6, height = 5, dpi = 300)




# 2. Kapplan-Meir curves for spoons ####

require(ggsurvfit)

sim_long = 
  sim_long %>% 
  mutate(time_disappearance = as.integer(date-dmy("01-10-2018")), 
         weight=factor(weight, levels = c("light", "heavy", "normal")))
# fast data exploration   ####
glob = survfit2(Surv(time_disappearance, event) ~ 1, data = sim_long) |>
  ggsurvfit()+
  add_confidence_interval() +
  add_censor_mark()+
  scale_x_continuous("Temps depuis début du suivi (semaines)",
                     breaks = seq(0,150, 21), 
                     labels = seq(0,150, 21)/7)+
  scale_y_continuous("Probabilité de survie",
                     breaks = seq(0,1,0.2))


(simi = 
  survfit2(Surv(time_disappearance, event) ~ factor(similitude), data = sim_long) |>
  ggsurvfit()+
  add_confidence_interval() +
  add_censor_mark()+
    scale_x_continuous("Temps depuis début du suivi (semaines)",
                       breaks = seq(0,150, 21), 
                       labels = seq(0,150, 21)/7)+
    scale_y_continuous("Probabilité de survie",
                     breaks = seq(0,1,0.2))+
    scale_fill_manual(values  = c("red", "darkblue"))+
    scale_color_manual(values  = c("red", "darkblue"))+
    theme_bw()
    
    
)

(type =
  survfit2(Surv(time_disappearance, event) ~ factor(type), data = sim_long) |>
  ggsurvfit()+
  add_confidence_interval() +
  add_censor_mark()+
  scale_x_continuous("Temps depuis début du suivi (semaines)",
                     breaks = seq(0,150, 21), 
                     labels = seq(0,150, 21)/7)+
  scale_y_continuous("Probabilité de survie",
                     breaks = seq(0,1,0.2))+
    scale_fill_manual(values  = c("seagreen", "black"))+
    scale_color_manual(values  = c("seagreen", "black"))+
    theme_bw()
)


(wei = 
  survfit2(Surv(time_disappearance, event) ~ factor(weight), data = sim_long) |>
  ggsurvfit()+
  add_confidence_interval() +
  add_censor_mark()+
    scale_x_continuous("Temps depuis début du suivi (semaines)",
                       breaks = seq(0,150, 21), 
                       labels = seq(0,150, 21)/7)+
    scale_y_continuous("Probabilité de survie",
                     breaks = seq(0,1,0.2))+
  scale_fill_manual(values  = c("black","gold", "purple"))+
  scale_color_manual(values  = c("black","orange", "purple4"))+
    theme_bw()
  )

require(patchwork)
(glob|type)/(simi|wei)

ggsave("km_curves.jpeg", width = 9, height = 6, dpi =300)


# append real time of disapearance: 
dizz = 
  sim_long %>% 
  mutate(time_disappearance = as.integer(date-dmy("01-10-2018")) 
  ) |> 
  arrange(id, time_disappearance) |> 
  subset(event ==1) |> 
  select(id, time_disappearance) 

sim_sum =
  sim_summary  |>
  left_join(dizz) |>
  select(-event, -date_event) |> 
  mutate(event = 1, 
         carac = paste(type, similitude, sep = "_") )


glimpse(sim_sum) 
summary(sim_sum)



# Cox modelling ####

cox_sim <- coxph(Surv(time_disappearance, event) ~ 
                   weight + carac,
                 data = sim_sum)
summary(cox_sim)

#Test zph

(zph <- survival::cox.zph(cox_sim))
plot(zph, var = "weight")

# Investigate weight level differences
zph <- cox.zph(cox_sim, terms = FALSE)
print(zph)

par(mfrow = c(1, 2))
plot(zph, var = "weightnormal",
     main = "Normal vs. light")

plot(zph, var = "weightheavy",
     main = "Heavy vs. light")

par(mfrow = c(1, 1))

# build a  cox model with a time varying effect for weight
cox_tv <- coxph(
  Surv(time_disappearance, event) ~
    weight + carac +
    tt(I(weight == "heavy"))+
  tt(I(weight == "normal")),
  data = sim_sum,
  tt = function(x, t, ...) x * log(t)
)

summary(cox_tv)

subdf = 
  subset(sim_sum, weight =="light") 
cox_2 <- coxph(
  Surv(time_disappearance, event) ~
    carac ,
  data = subdf)
summary(cox_2)

survival::cox.zph(cox_2)


# Plots ####

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# Extract coefficients and variance-covariance matrix
b <- coef(cox_tv)
V <- vcov(cox_tv)

# Coefficient names
names(b)

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

# Function to calculate time-varying HR and CI
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




# Contrasts test ####
b <- coef(cox_tv)
V <- vcov(cox_tv)

# Contraste entre les deux catégories
contrast <- b["caracsoup_alike"] -
  b["caracsoup_different"]

se_contrast <- sqrt(
  V["caracsoup_alike", "caracsoup_alike"] +
    V["caracsoup_different", "caracsoup_different"] -
    2 * V["caracsoup_alike", "caracsoup_different"]
)

z <- contrast / se_contrast
p_value <- 2 * pnorm(-abs(z))

# Résultats
exp(contrast)
exp(contrast + c(-1, 1) * 1.96 * se_contrast)
p_value




































# ARCHIVES ####
ggplot(hr_weight, 
       aes(x = term, y = estimate,
                      ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(size = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  scale_y_log10() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Hazard ratio (95% CI)"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title.x = element_text(size = 13)
  )


library(ggplot2)

library(tidyverse)

# Extract coefficients and variance-covariance matrix
b <- coef(cox_tv2)
V <- vcov(cox_tv2)

# Time points
hr_time <- tibble(
  time = seq(1, max(sim_summary$time_disappearance), length.out = 200)
) %>%
  mutate(
    log_time = log(time),
    
    # Linear predictor
    log_hr = b["weightheavy"] +
      b["tt(I(weight == \"heavy\")"] * log_time
  )

ggplot(hr_time, aes(x = time, y = hr)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_log10() +
  labs(
    x = "Time until disappearance (days)",
    y = "Hazard ratio (heavy vs. light)"
  ) +
  theme_classic()



glimpse(sim_sum)
summary(sim_sum)


cox_tv <- coxph(
  Surv(time_disappearance, event) ~
    weight + carac +
    tt(I(weight == "heavy"))+
    tt(I(weight == "normal")),
  data = sim_sum,
  tt = function(x, t, ...) x * log(t)
)

summary(cox_tv)

