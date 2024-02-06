## Load necessary libraries

library(tidyverse)
library(readxl)
library(ggplot2)
library(scales)
library(patchwork)


options(scipen = 999)

## Load data and create individual dataframes for each sheet

## Change so you're using reg_file

reg_file = "RegressionFullEverCLC031520_r.xlsx"
reg_file_chronic = "RegressionFullChronicCLC031520_r.xlsx"

reg_file_mortality = "MortalityRegressionFullEver031520_r.xlsx"

dd_base = read_excel(reg_file, sheet = 1)
dd_edu_neigh = read_excel(reg_file, sheet = 2)
dd_neuro_base = read_excel(reg_file, sheet = 3)
dd_neuro = read_excel(reg_file, sheet = 4)
dd_cesd9404 = read_excel(reg_file, sheet = 5)
dd_cesd0614 = read_excel(reg_file, sheet = 6)

ba_base = read_excel(reg_file, sheet = 7)
ba_edu_neigh = read_excel(reg_file, sheet = 8)
ba_neuro_base = read_excel(reg_file, sheet = 9)
ba_neuro = read_excel(reg_file, sheet = 10)
ba_cesd9404 = read_excel(reg_file, sheet = 11)
ba_cesd0614 = read_excel(reg_file, sheet = 12)

m_base = read_excel(reg_file_mortality, sheet = 1)
m_edu_neigh = read_excel(reg_file_mortality, sheet = 2)
m_neuro_base = read_excel(reg_file_mortality, sheet = 3)
m_neuro = read_excel(reg_file_mortality, sheet = 4)
m_cesd9404 = read_excel(reg_file_mortality, sheet = 5)
m_cesd0614 = read_excel(reg_file_mortality, sheet = 6)

dd_base_chronic = read_excel(reg_file_chronic, sheet = 1)


## Create function to tidy Mortality and Diseases & Disabilities dataframes
## Required Inputs: dataframe and character string of model type (e.g. base, sat, etc.)

tidy_dd_ever = function(dd_model, x){
  
  dd_model %>% 
    mutate(exposure = (1:n() - 1) %/% 6) %>% 
    mutate(exposure = factor(exposure, labels = c("Social6", "Social10", "UCLA"))) %>% 
    group_by(exposure) %>% 
    mutate(outcome = (1:n() - 1) %/% 2) %>%
    mutate(outcome = factor(outcome, labels = c("ADL", "IADL", "Chronic Disease"))) %>% 
    group_by(exposure, outcome) %>% 
    filter(lb != 999.000000) %>% 
    mutate(level = (1:n() - 1) %/% 1) %>% 
    mutate(level = factor(level, labels = c("Ever"))) %>%
    mutate(model = x) %>% 
    select(model, exposure, outcome, level, "HR/IRR", lb:N)
  
}


tidy_dd_chronic = function(dd_model, x){

  dd_model %>% 
    mutate(exposure = (1:n() - 1) %/% 9) %>% 
    mutate(exposure = factor(exposure, labels = c("Social6", "Social10", "UCLA"))) %>% 
    group_by(exposure) %>% 
    mutate(outcome = (1:n() - 1) %/% 3) %>%
    mutate(outcome = factor(outcome, labels = c("ADL", "IADL", "Chronic Disease"))) %>% 
    group_by(exposure, outcome) %>% 
    filter(IRR != 999.000000) %>% 
    mutate(level = (1:n() - 1) %/% 1) %>% 
    mutate(level = factor(level, labels = c("Intermittent", "Persistent"))) %>%
    mutate(model = x) %>% 
    select(model, exposure, outcome, level, IRR:N)

}


tidy_m_ever = function(m_model, x){
  
  m_model %>% 
    mutate(exposure = (1:n() - 1) %/% 1) %>% 
    mutate(exposure = factor(exposure, labels = c("Social6", "Social10", "UCLA"))) %>% 
    group_by(exposure) %>% 
    mutate(outcome = (1:n() - 1) %/% 1) %>%
    mutate(outcome = factor(outcome, labels = c("Mortality"))) %>% 
    group_by(exposure, outcome) %>% 
    mutate(level = (1:n() - 1) %/% 1) %>% 
    mutate(level = factor(level, labels = c("Ever"))) %>%
    mutate(model = x) %>% 
    select(model, exposure, outcome, level, "HR/IRR", lb:N)
  
}

## Use function to tidy all of the Diseases & Disabilities dataframes and save as new dataframes

tidy_dd_base = tidy_dd_ever(dd_base, "Base")
tidy_dd_edu_neigh = tidy_dd_ever(dd_edu_neigh, "Education + Neighborhood")
tidy_dd_neuro_base = tidy_dd_ever(dd_neuro_base, "Neuroticism (baseline)")
tidy_dd_neuro = tidy_dd_ever(dd_neuro, "Neuroticism (06-14)")
tidy_dd_cesd9404 = tidy_dd_ever(dd_cesd9404, "CES-D (94-04)")
tidy_dd_cesd0614 = tidy_dd_ever(dd_cesd0614, "CES-D (06-14)")

tidy_dd_chronic = tidy_dd_chronic(dd_base_chronic, "Base")

## Use function to tidy all of the Mortality dataframes and save as new dataframes

tidy_m_base = tidy_m_ever(m_base, "Base")
tidy_m_edu_neigh = tidy_m_ever(m_edu_neigh, "Education + Neighborhood")
tidy_m_neuro_base = tidy_m_ever(m_neuro_base, "Neuroticism (baseline)")
tidy_m_neuro = tidy_m_ever(m_neuro, "Neuroticism (06-14)")
tidy_m_cesd9404 = tidy_m_ever(m_cesd9404, "CES-D (94-04)")
tidy_m_cesd0614 = tidy_m_ever(m_cesd0614, "CES-D (06-14)")

## Append all Diseases & Disabilities dataframes together

dd_final = rbind(tidy_dd_base, tidy_dd_edu_neigh, tidy_dd_neuro_base, tidy_dd_neuro, tidy_dd_cesd9404, tidy_dd_cesd0614) %>% 
  mutate(model = factor(model, levels = c("Base", "Education + Neighborhood", "Neuroticism (baseline)", "Neuroticism (06-14)", "CES-D (94-04)", "CES-D (06-14)")))

## Append all Biological Aging dataframes together

ba_final = rbind(tidy_ba_base, tidy_ba_edu_neigh, tidy_ba_neuro_base, tidy_ba_neuro, tidy_ba_cesd9404, tidy_ba_cesd0614) %>% 
  mutate(model = factor(model, levels = c("Base", "Education + Neighborhood", "Neuroticism (baseline)", "Neuroticism (06-14)", "CES-D (94-04)", "CES-D (06-14)")))

## Append all Mortality dataframes together

m_final = rbind(tidy_m_base, tidy_m_edu_neigh, tidy_m_neuro_base, tidy_m_neuro, tidy_m_cesd9404, tidy_m_cesd0614) %>% 
  mutate(model = factor(model, levels = c("Base", "Education + Neighborhood", "Neuroticism (baseline)", "Neuroticism (06-14)", "CES-D (94-04)", "CES-D (06-14)")))

## Combine Mortality and Diseases & Disability dataframes

mdd_final = rbind(dd_final, m_final)

########################################
##  Figure 1 -- Covariate Adjustment  ##
########################################

## Social 6

mdd_social6_cov = mdd_final %>% 
  ungroup() %>% 
  filter(model != "Neuroticism (06-14)", model != "CES-D (06-14)", exposure == "Social6") %>% 
  mutate(`HR/IRR` = `HR/IRR` - 1, ub = ub - 1, lb = lb - 1) %>% 
  mutate(outcome = factor(outcome, levels = c("Chronic Disease", "IADL", "ADL", "Mortality")), model = factor(model, levels = c("CES-D (94-04)", "Neuroticism (baseline)", "Education + Neighborhood", "Base"))) %>% 
  ggplot(aes(x = outcome, y = `HR/IRR`, fill = factor(model))) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  geom_bar(stat = "identity", width = 0.75, position = "dodge", alpha = 0.75) +
  geom_errorbar(aes(ymin = lb, ymax = ub, width = 0.15), position = position_dodge(width = 0.75)) +
  coord_flip(ylim = c(0.0,1.2)) +
  theme_bw() +
  theme(legend.position = "none", strip.text.y = element_text(size = 30), legend.title = element_text(size = 10), legend.text = element_text(size = 10), axis.title.x = element_text(size = 25), axis.text.x = element_text(size = 20), axis.text.y = element_text(angle = 90, hjust = 0.5, size = 20)) +
  labs(fill = "Added Covariates") +
  xlab("") +
  scale_x_discrete(labels = wrap_format(10)) +
  scale_y_continuous(breaks = c(0.0, 0.4, 0.8, 1.2), labels = number(c(1.0, 1.4, 1.8, 2.2), accuracy = 0.1)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(labels = c("Depressive Symptoms", "Neuroticism", "Socioeconomic Disadvantage", "Base"), values = c("#021324", "#083a6c", "#0fb493", "#B40F20"))

## UCLA

mdd_ucla_cov = mdd_final %>% 
  ungroup() %>% 
  filter(model != "Neuroticism (06-14)", model != "CES-D (06-14)", exposure == "UCLA") %>% 
  mutate(`HR/IRR` = `HR/IRR` - 1, ub = ub - 1, lb = lb - 1) %>% 
  mutate(outcome = factor(outcome, levels = c("Chronic Disease", "IADL", "ADL", "Mortality")), model = factor(model, levels = c("CES-D (94-04)", "Neuroticism (baseline)", "Education + Neighborhood", "Base"))) %>% 
  ggplot(aes(x = outcome, y = `HR/IRR`, fill = factor(model))) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  geom_bar(stat = "identity", width = 0.75, position = "dodge", alpha = 0.75) +
  geom_errorbar(aes(ymin = lb, ymax = ub, width = 0.15), position = position_dodge(width = 0.75)) +
  coord_flip(ylim = c(0.0, 1.2)) +
  theme_bw() +
  theme(legend.position = "none", strip.text.y = element_text(size = 30), legend.title = element_text(size = 10), legend.text = element_text(size = 10), axis.title.x = element_text(size = 25), axis.text.x = element_text(size = 20), axis.text.y = element_text(angle = 90, hjust = 0.5, size = 20)) +
  labs(fill = "Added Covariates") +
  xlab("") +
  scale_x_discrete(labels = wrap_format(10)) +
  scale_y_continuous(breaks = c(0.0, 0.4, 0.8, 1.2), labels = number(c(1.0, 1.4, 1.8, 2.2), accuracy = 0.1)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(labels = c("Depressive Symptoms", "Neuroticism", "Socioeconomic Disadvantage", "Base"), values = c("#021324", "#083a6c", "#0fb493", "#B40F20"))

## Social 10

mdd_social10_cov = mdd_final %>% 
  ungroup() %>% 
  filter(model != "Neuroticism (06-14)", model != "CES-D (06-14)", exposure == "Social10") %>% 
  mutate(`HR/IRR` = `HR/IRR` - 1, ub = ub - 1, lb = lb - 1) %>% 
  mutate(outcome = factor(outcome, levels = c("Chronic Disease", "IADL", "ADL", "Mortality")), model = factor(model, levels = c("CES-D (94-04)", "Neuroticism (baseline)", "Education + Neighborhood", "Base"))) %>% 
  ggplot(aes(x = outcome, y = `HR/IRR`, fill = factor(model))) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  geom_bar(stat = "identity", width = 0.75, position = "dodge", alpha = 0.75) +
  geom_errorbar(aes(ymin = lb, ymax = ub, width = 0.15), position = position_dodge(width = 0.75)) +
  coord_flip(ylim = c(0.0, 1.2)) +
  theme_bw() +
  theme(legend.position = "none", strip.text.y = element_text(size = 30), legend.title = element_text(size = 10), legend.text = element_text(size = 10), axis.title.x = element_text(size = 25), axis.text.x = element_text(size = 20), axis.text.y = element_text(angle = 90, hjust = 0.5, size = 20)) +
  labs(fill = "Added Covariates") +
  xlab("") +
  scale_x_discrete(labels = wrap_format(10)) +
  scale_y_continuous(breaks = c(0.0, 0.4, 0.8, 1.2), labels = number(c(1.0, 1.4, 1.8, 2.2), accuracy = 0.1)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(labels = c("Depressive Symptoms", "Neuroticism", "Socioeconomic Disadvantage", "Base"), values = c("#021324", "#083a6c", "#0fb493", "#B40F20"))

ggsave("mdd_social6_cov.png", mdd_social6_cov, width = 6, height = 6, units = "in")
ggsave("mdd_ucla_cov.png", mdd_ucla_cov, width = 6, height = 6)
ggsave("mdd_social10_cov.png", mdd_social10_cov, width = 6, height = 6)

###################################
##  Figure 2 - Incident Analysis ##
###################################

## Load data and create individual dataframes for each sheet

reg_file_inc = "inc_covariates031520_r.xlsx"
reg_file_inc_chronic = "inc_covariates_chronic031520_r.xlsx"

dd_inc_ever = read_excel(reg_file_inc, sheet = 1)
dd_inc_chronic = read_excel(reg_file_inc_chronic, sheet = 1)

dd_inc_base = read_excel(reg_file_inc, sheet = 1)
dd_inc_socio = read_excel(reg_file_inc, sheet = 2)
dd_inc_neurobase = read_excel(reg_file_inc, sheet = 3)
dd_inc_neuro0614 = read_excel(reg_file_inc, sheet = 4)
dd_inc_cesdbase = read_excel(reg_file_inc, sheet = 5)
dd_inc_cesd0614 = read_excel(reg_file_inc, sheet = 6)

## Create function to tidy Diseases & Disabilities dataframes
## Required Inputs: dataframe and character string of model type (e.g. base, sat, etc.)

tidy_dd_ever = function(dd_model, x){
  
  dd_model %>% 
    mutate(exposure = (1:n() - 1) %/% 6) %>% 
    mutate(exposure = factor(exposure, labels = c("Social6", "Social10", "UCLA"))) %>% 
    group_by(exposure) %>% 
    mutate(outcome = (1:n() - 1) %/% 2) %>%
    mutate(outcome = factor(outcome, labels = c("ADL", "IADL", "Chronic Disease"))) %>% 
    group_by(exposure, outcome) %>% 
    filter(IRR != 999.000000) %>% 
    mutate(level = (1:n() - 1) %/% 1) %>% 
    mutate(level = factor(level, labels = c("Ever"))) %>%
    mutate(model = x) %>% 
    select(model, exposure, outcome, level, IRR:N)
  
}

tidy_dd_chronic = function(dd_model, x){
  
  dd_model %>% 
    mutate(exposure = (1:n() - 1) %/% 9) %>% 
    mutate(exposure = factor(exposure, labels = c("Social6", "Social10", "UCLA"))) %>% 
    group_by(exposure) %>% 
    mutate(outcome = (1:n() - 1) %/% 3) %>%
    mutate(outcome = factor(outcome, labels = c("ADL", "IADL", "Chronic Disease"))) %>% 
    group_by(exposure, outcome) %>% 
    filter(IRR != 999.000000) %>% 
    mutate(level = (1:n() - 1) %/% 1) %>% 
    mutate(level = factor(level, labels = c("Intermittent", "Persistent"))) %>%
    mutate(model = x) %>% 
    select(model, exposure, outcome, level, IRR:N)
  
}

## Use function to tidy all of the Diseases & Disabilities dataframes and save as new dataframes

tidy_dd_inc_ever = tidy_dd_ever(dd_inc_ever, "Incident")

tidy_dd_inc_chronic = tidy_dd_chronic(dd_inc_chronic, "Incident")

dd_final_ever = tidy_dd_inc_ever %>% 
  mutate(model = factor(model, levels = c("Incident")))

dd_final_chronic = tidy_dd_inc_chronic %>% 
  mutate(model = factor(model, levels = c("Incident")))

tidy_dd_inc_base = tidy_dd_ever(dd_inc_base, "Base")
tidy_dd_inc_socio = tidy_dd_ever(dd_inc_socio, "Socioeconomic Disadvantage")
tidy_dd_inc_neurobase = tidy_dd_ever(dd_inc_neurobase, "Neuroticism")
tidy_dd_inc_neuro0614 = tidy_dd_ever(dd_inc_neuro0614, "Neuro (06-14)")
tidy_dd_inc_cesdbase = tidy_dd_ever(dd_inc_cesdbase, "Depressive Symptoms")
tidy_dd_inc_cesd0614 = tidy_dd_ever(dd_inc_cesd0614, "CES-D (06-14)")

dd_final_inc = rbind(tidy_dd_inc_base, tidy_dd_inc_socio, tidy_dd_inc_neurobase, tidy_dd_inc_neuro0614, tidy_dd_inc_cesdbase, tidy_dd_inc_cesd0614)

## Ever vs. Never

## Social 6 & UCLA -- Incident

dd_social6_ucla_additional_ever_inc = dd_final_ever %>% 
  ungroup() %>% 
  filter(exposure == "Social6" | exposure == "UCLA") %>% 
  mutate(IRR = IRR - 1, ub = ub - 1, lb = lb - 1) %>% 
  mutate(outcome = factor(outcome, levels = c("Chronic Disease", "IADL", "ADL"))) %>% 
  mutate(model = factor(model, levels = c("Incident", "Base"))) %>% 
  filter(model == "Incident") %>% 
  ggplot(aes(x = outcome, y = IRR, fill = factor(exposure))) +
  geom_bar(stat = "identity", width = 0.75, position = "dodge", alpha = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  geom_errorbar(size = 1, aes(ymin = lb, ymax = ub, width = 0.15), position = position_dodge(width = 0.75)) +
  coord_flip(ylim = c(-0.20,1.0)) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30), legend.title = element_text(size = 20), legend.text = element_text(size = 15), legend.position = "none", legend.direction = "horizontal", axis.title.x = element_text(size = 30, margin = margin(15,0,0,0)), axis.text.y = element_text(angle = 90, hjust = 0.75, size = 20), axis.text.x = element_text(size = 20)) +
  xlab("") + 
  labs(fill = "Model") +
  scale_x_discrete(labels = wrap_format(10)) +
  scale_y_continuous(breaks = c(-0.2, 0.0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = number(c(0.8, 1.00, 1.2, 1.4, 1.6, 1.8, 2.0), accuracy = 0.1)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c( "#43060C", "#B40F20"))

dd_social6_ucla_additional_ever_inc_legend = dd_final_ever %>% 
  ungroup() %>% 
  filter(exposure == "Social6" | exposure == "UCLA") %>% 
  mutate(IRR = IRR - 1, ub = ub - 1, lb = lb - 1) %>% 
  mutate(outcome = factor(outcome, levels = c("Chronic Disease", "IADL", "ADL"))) %>% 
  mutate(model = factor(model, levels = c("Incident", "Base"))) %>% 
  filter(model == "Incident") %>% 
  ggplot(aes(x = outcome, y = IRR, fill = factor(exposure))) +
  geom_bar(stat = "identity", width = 0.75, position = "dodge", alpha = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  geom_errorbar(size = 1, aes(ymin = lb, ymax = ub, width = 0.15), position = position_dodge(width = 0.75)) +
  coord_flip(ylim = c(-0.20,1.0)) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30), legend.title = element_text(size = 20), legend.text = element_text(size = 15), legend.position = "right", legend.direction = "vertical", axis.title.x = element_text(size = 30, margin = margin(15,0,0,0)), axis.text.y = element_text(angle = 90, hjust = 0.75, size = 20), axis.text.x = element_text(size = 20)) +
  xlab("") + 
  labs(fill = "Model") +
  scale_x_discrete(labels = wrap_format(10)) +
  scale_y_continuous(breaks = c(-0.2, 0.0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = number(c(0.8, 1.00, 1.2, 1.4, 1.6, 1.8, 2.0), accuracy = 0.1)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("#43060C", "#B40F20"))


## Social 10 -- Incident

dd_social10_additional_ever_inc = dd_final_ever %>% 
  ungroup() %>% 
  filter(exposure == "Social10") %>% 
  mutate(IRR = IRR - 1, ub = ub - 1, lb = lb - 1) %>% 
  mutate(outcome = factor(outcome, levels = c("Chronic Disease", "IADL", "ADL"))) %>% 
  mutate(model = factor(model, levels = c("Incident", "Base"))) %>% 
  filter(model == "Incident") %>% 
  ggplot(aes(x = outcome, y = IRR, fill = factor(level))) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  geom_bar(stat = "identity", width = 0.75, position = "dodge", alpha = 0.75) +
  geom_errorbar(size = 1, aes(ymin = lb, ymax = ub, width = 0.15), position = position_dodge(width = 0.75)) +
  coord_flip(ylim = c(-0.2,1.0)) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 30), legend.title = element_text(size = 20), legend.text = element_text(size = 15), legend.position = "none", legend.direction = "horizontal", axis.title.x = element_text(size = 30, margin = margin(15,0,0,0)), axis.text.y = element_text(angle = 90, hjust = 0.75, size = 20), axis.text.x = element_text(size = 20)) +
  xlab("") + 
  labs(fill = "Model") +
  scale_x_discrete(labels = wrap_format(10)) +
  scale_y_continuous(breaks = c(-0.2, 0.0, 0.2, 0.4, 0.6, 0.8, 1.0), labels = number(c(0.8, 1.00, 1.2, 1.4, 1.6, 1.8, 2.0), accuracy = 0.1)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("#B40F20"))

ggsave("dd_social6_ucla_ever_inc.png", dd_social6_ucla_additional_ever_inc, width = 6, height = 6)
ggsave("dd_social6_ucla_ever_inc_legend.png", dd_social6_ucla_additional_ever_inc_legend, width = 6, height = 6)
ggsave("dd_social10_ever_inc.png", dd_social10_additional_ever_inc, width = 6, height = 6)




















## Supplemental Covariate Adjustment

## Mortality

social6_m = create_m_plot(m_final, "Mortality", "Social6") 
ggsave("social6_m_supp_cov.png", social6_m, width = 6, height = 6)

ucla_m = create_m_plot(m_final, "Mortality", "UCLA") 
ggsave("ucla_m_supp_cov.png", ucla_m, width = 6, height = 6)

social10_m = create_m_plot(m_final, "Mortality", "Social10") 
ggsave("social10_m_supp_cov.png", social10_m, width = 6, height = 6)

## Prevalent ADL

social6_adl = create_dd_plot(dd_final, "ADL", "Social6")
ggsave("social6_adl_supp_cov.png", social6_adl, width = 6, height = 6)

ucla_adl = create_dd_plot(dd_final, "ADL", "UCLA")
ggsave("ucla_adl_supp_cov.png", ucla_adl, width = 6, height = 6)

social10_adl = create_dd_plot(dd_final, "ADL", "Social10")
ggsave("social10_adl_supp_cov.png", social10_adl, width = 6, height = 6)

## Prevalent IADL

social6_iadl = create_dd_plot(dd_final, "IADL", "Social6")
ggsave("social6_iadl_supp_cov.png", social6_iadl, width = 6, height = 6)

ucla_iadl = create_dd_plot(dd_final, "IADL", "UCLA")
ggsave("ucla_iadl_supp_cov.png", ucla_iadl, width = 6, height = 6)

social10_iadl = create_dd_plot(dd_final, "IADL", "Social10")
ggsave("social10_iadl_supp_cov.png", social10_iadl, width = 6, height = 6)

## Prevalent Chronic

social6_chronic = create_dd_plot(dd_final, "Chronic Disease", "Social6")
ggsave("social6_chronic_supp_cov.png", social6_chronic, width = 6, height = 6)

ucla_chronic = create_dd_plot(dd_final, "Chronic Disease", "UCLA")
ggsave("ucla_chronic_supp_cov.png", ucla_chronic, width = 6, height = 6)

social10_chronic = create_dd_plot(dd_final, "Chronic Disease", "Social10")
ggsave("social10_chronic_supp_cov.png", social10_chronic, width = 6, height = 6)


## BA

social6_ba = create_ba_plot(ba_final, "Levine BA", "Social6")
ggsave("social6_ba_supp_cov.png", social6_ba, width = 6, height = 6)

ucla_ba = create_ba_plot(ba_final, "Levine BA", "UCLA")
ggsave("ucla_ba_supp_cov.png", ucla_ba, width = 6, height = 6)

social10_ba = create_ba_plot(ba_final, "Levine BA", "Social10")
ggsave("social10_ba_supp_cov.png", social10_ba, width = 6, height = 6)
