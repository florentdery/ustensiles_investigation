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
  mutate(time_disappearance = as.integer(date-dmy("01-10-2018")) 
         )
  

survfit2(Surv(time_disappearance, event) ~ 1, data = sim_long) |>
  ggsurvfit()+
  add_confidence_interval() +
  add_censor_mark()

survfit2(Surv(time_disappearance, event) ~ factor(similitude), data = sim_long) |>
  ggsurvfit()+
  add_confidence_interval() +
  add_censor_mark()

survfit2(Surv(time_disappearance, event) ~ factor(type), data = sim_long) |>
  ggsurvfit()+
  add_confidence_interval() +
  add_censor_mark()

survfit2(Surv(time_disappearance, event) ~ factor(weight), data = sim_long) |>
  ggsurvfit()+
  add_confidence_interval() +
  add_censor_mark()


cox_sim <- coxph(Surv(time_visits, event) ~ attractiveness + weight + similitude + type, data = sim_summary)
summary(cox_sim)