
library(readxl)
w2 <- read_excel("data/00_data_prep.R_country_level_data_wave2.xlsx", na = "-999")
w3 <- read_excel("data/00_data_prep.R_country_level_data_wave3.xlsx", na = "-999")

library(tidyverse)

ntile_na <- function(var, ntile_n)
{
  notna <- !is.na(var)
  out <- rep(NA_real_,length(var))
  out[notna] <- ntile(var[notna],ntile_n)
  return(out)
}

w2$GDPPPP_q <- ntile_na(w2$GDPPPP,2)
unique(w2$GDPPPP_q)
w2$GDPPPP_q <- factor(w2$GDPPPP_q, levels = 1:2, labels = c("<med", ">med"))
unique(w2$GDPPPP_q)

w2$GINI_q <- ntile_na(w2$GINI,2)
unique(w2$GINI_q)
w2$GINI_q <- factor(w2$GINI_q, levels = 1:2, labels = c("<med", ">med"))
unique(w2$GINI_q)

w2 <- w2 %>% mutate(libD_q = ntile(LibD, 2))
unique(w2$libD_q)
w2$libD_q <- factor(w2$libD_q, levels = 1:2, labels = c("<med", ">med"))
unique(w2$libD_q)

w2 <- w2 %>% mutate(egaD_q = ntile(EgaD, 2))
unique(w2$egaD_q)
w2$egaD_q <- factor(w2$egaD_q, levels = 1:2, labels = c("<med", ">med"))
unique(w2$egaD_q)

w3$GDPPPP_q <- ntile_na(w3$GDPPPP,2)
unique(w3$GDPPPP_q)
w3$GDPPPP_q <- factor(w3$GDPPPP_q, levels = 1:2, labels = c("<med", ">med"))
unique(w3$GDPPPP_q)

w3$GINI_q <- ntile_na(w3$GINI,2)
unique(w3$GINI_q)
w3$GINI_q <- factor(w3$GINI_q, levels = 1:2, labels = c("<med", ">med"))
unique(w3$GINI_q)

w3 <- w3 %>% mutate(libD_q = ntile(LibD, 2))
unique(w3$libD_q)
w3$libD_q <- factor(w3$libD_q, levels = 1:2, labels = c("<med", ">med"))
unique(w3$libD_q)

w3 <- w3 %>% mutate(egaD_q = ntile(EgaD, 2))
unique(w3$egaD_q)
w3$egaD_q <- factor(w3$egaD_q, levels = 1:2, labels = c("<med", ">med"))
unique(w3$egaD_q)

both_waves <- bind_rows(w2,w3)
rio::export(both_waves, file = "data/00_data_prep.R_country_level_data_wQs.xlsx")
