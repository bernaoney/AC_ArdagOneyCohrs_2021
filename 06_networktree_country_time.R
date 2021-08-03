
load("data/ISSP_NI_M23_comp_redu_comb.RData")
colnames(dat)
library(networktree)

nodevar_names <- paste(names(dat[,12:23]), collapse = "+")
splitvars_names_country_time <- paste(names(dat[,2:6]), collapse = "+")
nw_f_country_time <- as.formula(paste(c(nodevar_names, splitvars_names_country_time), collapse= "~"))

ni_nwt_country <- networktree(nw_f_country_time, data = dat, method = "ctree", model = "correlation", transform = "glasso")
plot(ni_nwt_country, type = "glasso", layout = "circle", maximum = 1, edge.labels = T, edge.label.cex = 1, theme = "colorblind")

library(tidyverse)

country_data <- readxl::read_excel("data/00_data_prep.R_country_level_data_wQs.xlsx") %>%
  select(country:wave_t1,GDPPPP_q:egaD_q)

# below median  -- poorer countries
country_data %>% filter(GDPPPP_q %in% "<med" & GINI_q %in% "<med" & egaD_q %in% "<med" & wave_t1 %in% "wave 2 (T1)") # SK KR
country_data %>% filter(GDPPPP_q %in% "<med" & GINI_q %in% "<med" & egaD_q %in% "<med" & wave_t1 %in% "wave 3 (T2)") # HU SK KR

country_data %>% filter(GDPPPP_q %in% "<med" & GINI_q %in% "<med" & egaD_q %in% ">med" & wave_t1 %in% "wave 2 (T1)" & libD_q %in% "<med") # SI
country_data %>% filter(GDPPPP_q %in% "<med" & GINI_q %in% "<med" & egaD_q %in% ">med" & wave_t1 %in% "wave 2 (T1)" & libD_q %in% ">med") # CZ ES

country_data %>% filter(GDPPPP_q %in% "<med" & GINI_q %in% "<med" & egaD_q %in% ">med" & wave_t1 %in% "wave 3 (T2)" & libD_q %in% "<med") # CZ
country_data %>% filter(GDPPPP_q %in% "<med" & GINI_q %in% "<med" & egaD_q %in% ">med" & wave_t1 %in% "wave 3 (T2)" & libD_q %in% ">med") # SI

country_data %>% filter(GDPPPP_q %in% "<med" & GINI_q %in% ">med" & libD_q %in% "<med" & wave_t1 %in% "wave 2 (T1)") # HU RU PH IL LV ZA
country_data %>% filter(GDPPPP_q %in% "<med" & GINI_q %in% ">med" & libD_q %in% "<med" & wave_t1 %in% "wave 3 (T2)") # RU PH IL ES LV ZA

country_data %>% filter(GDPPPP_q %in% "<med" & GINI_q %in% ">med" & libD_q %in% ">med" & egaD_q %in% ">med") # PT
country_data %>% filter(GDPPPP_q %in% "<med" & GINI_q %in% ">med" & libD_q %in% ">med" & egaD_q %in% "<med") # PT

# above median  -- richer countries
country_data %>% filter(GDPPPP_q %in% ">med" & libD_q %in% "<med" & egaD_q %in% "<med") # IE
country_data %>% filter(GDPPPP_q %in% ">med" & libD_q %in% "<med" & egaD_q %in% ">med") # JP JP

country_data %>% filter(GDPPPP_q %in% ">med" & libD_q %in% ">med" & egaD_q %in% "<med" & wave_t1 %in% "wave 2 (T1)") # US
country_data %>% filter(GDPPPP_q %in% ">med" & libD_q %in% ">med" & egaD_q %in% "<med" & wave_t1 %in% "wave 3 (T2)") # US IE

country_data %>% filter(GDPPPP_q %in% ">med" & libD_q %in% ">med" & egaD_q %in% ">med" & GINI_q %in% "<med" & wave_t1 %in% "wave 2 (T1)") #
# DE NO SE FR DK FI
country_data %>% filter(GDPPPP_q %in% ">med" & libD_q %in% ">med" & egaD_q %in% ">med" & GINI_q %in% "<med" & wave_t1 %in% "wave 3 (T2)") #
# DE NO SE FR DK CH FI

country_data %>% filter(GDPPPP_q %in% ">med" & libD_q %in% ">med" & egaD_q %in% ">med" & GINI_q %in% ">med" & wave_t1 %in% "wave 2 (T1)") # GB
country_data %>% filter(GDPPPP_q %in% ">med" & libD_q %in% ">med" & egaD_q %in% ">med" & GINI_q %in% ">med" & wave_t1 %in% "wave 3 (T2)") # GB
