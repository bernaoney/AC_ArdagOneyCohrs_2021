
library(future)
plan(multisession)
library(tidyverse)
library(haven)

#########################################################################################
#
# 2003
#
#########################################################################################

# ISSP 2003 - "National Identity II" - ZA No. 3910
# publicly available @
# https://www.gesis.org/issp/modules/issp-modules-by-topic/national-identity/2003

d03 <- read_stata("data/ZA3910_v2-1-0.dta") %>%
  as_tibble() %>% zap_labels() %>%
  select(C_ALPHAN,v9,v11,v12,v13,v14,v15,v16,v17,v18,v19,v21,v22,v23,v26,v27,v28,v29,v30,v31,v32,v33,v34,v35,v38,v47,v50,v51,v52,v53,v55,
         sex,age,educyrs,topbot,party_lr) %>%
  filter(C_ALPHAN %in% c("CZ","DK","FI","FR","DE-E","DE-W","GB-GBN","HU","IE","IL-A","IL-J",
                         "JP","LV","NO","PH","PT","RU","SI","SK","ZA","KR","ES","SE","CH","TW","US")) %>%
  dplyr::rename(cntr=C_ALPHAN,
                clC=v9,
                brn=v11,ctz=v12,lve=v13,lng=v14,rlg=v15,rsp=v16,fel=v17,anc=v18,
                Nci=v19,Nwb=v21,Ncb=v22,
                Pbs=v23,
                Pde=v26,Piw=v27,Pec=v28,Pss=v29,Psc=v30,Psp=v31,Par=v32,Paf=v33,Phi=v34,Peq=v35,
                Pbi=v38,
                ShC=v47,
                Icr=v50,Ige=v51,Itj=v52,Icl=v53,Inm=v55,
                gender=sex,ageG=age,eduG=educyrs,tbB=topbot,POLR=party_lr)

d03$cntr <- as_factor(d03$cntr)
d03$cntr <- fct_collapse(d03$cntr,
                         DE=c("DE-E","DE-W"),
                         IL=c("IL-A","IL-J"))
d03$cntr <- recode_factor(d03$cntr,`GB-GBN` = "GB")
unique(d03$cntr)

library(naniar)
d03 <- d03 %>% replace_with_na(replace = list(eduG =c(94,95,96,97),
                                              POLR = c(6,7)))

d03 <- d03 %>% dplyr::mutate_at(c("Nci", "Nwb", "Ncb", "Pbs", "Pbi","ShC","Icr","Ige","Itj"),
                                funs(dplyr::recode(., `5`=1, `4`=2, `3`=3, `2`=4, `1`=5, .default = NaN)))

d03 <- d03 %>% dplyr::mutate_at(c("clC",
                                  "brn","ctz","lve","lng","rlg","rsp","fel","anc",
                                  "Pde","Piw","Pec","Pss","Psc","Psp","Par","Paf","Phi","Peq"),
                                funs(dplyr::recode(., `4`=1, `3`=2, `2`=3, `1`=4, .default = NaN)))

d03$gender <- factor(d03$gender, levels = c(1,2), labels = c("Male", "Female"))

d03$POLR <- ordered(d03$POLR,
                    levels = c(1,2,3,4,5),
                    labels = c("Far_Left", "Left",
                               "Center",
                               "Right","Far_Right"))
library(psych)
describe(d03)

d03 <- d03 %>% mutate(ageG = ntile(ageG, 4))
unique(d03$ageG)
d03$ageG <- factor(d03$ageG, levels = 1:4, labels = c("1Q", "2Q", "3Q", "4Q"))

d03 <- d03 %>% mutate(eduG = ntile(eduG, 4))
unique(d03$eduG)
d03$eduG <- factor(d03$eduG, levels = 1:4, labels = c("1Q", "2Q", "3Q", "4Q"))

d03 <- d03 %>% mutate(tbB = ntile(tbB, 2))
unique(d03$tbB)
d03$tbB <- factor(d03$tbB, levels = 1:2, labels = c("<med", ">med"))

d03$time <- "T1(2003)"

w03_country_level <- readxl::read_excel("data/00_data_prep.R_country_level_data_wQs.xlsx") %>%
  filter(wave_t1%in%"wave 2 (T1)") %>%
  select(country, GDPPPP_q:egaD_q) %>% rename(cntr = country) 

d03 <- d03 %>% full_join(w03_country_level, by="cntr")

factor_cols <- c("time", "GDPPPP_q", "GINI_q", "libD_q", "egaD_q")
d03[factor_cols] <- lapply(d03[factor_cols], factor)

save(d03, file = "data/ISSP_NI_M2_2003.RData")

gdata::keep(list = c("d03", "factor_cols"), sure = T)

#########################################################################################
#
# 2013
#
#########################################################################################

# ISSP 2013 - "National Identity III" - ZA No. 5950
# publicly available @
# https://www.gesis.org/issp/modules/issp-modules-by-topic/national-identity/2013/

# rescale ZA conceptions of nationhood items
#-------------------------------------------------------------------------------------------------------------------------------------
d_temp_ZA_CoN <- read_stata("data/ZA5950_v2-0-0.dta") %>% 
  as_tibble() %>% zap_labels() %>% 
  select(C_ALPHAN, ZA_V9, ZA_V10, ZA_V11, ZA_V12, ZA_V13, ZA_V14, ZA_V15, ZA_V16) %>%
  filter(C_ALPHAN %in% c("ZA"))
d_temp_ZA_CoN <- d_temp_ZA_CoN %>% replace_with_na_at(.vars = c("ZA_V9", "ZA_V10", "ZA_V11", "ZA_V12", "ZA_V13", "ZA_V14", "ZA_V15", "ZA_V16"), condition = ~.x == 0)
d_temp_ZA_CoN <- d_temp_ZA_CoN %>% replace_with_na_at(.vars = c("ZA_V9", "ZA_V10", "ZA_V11", "ZA_V12", "ZA_V13", "ZA_V14", "ZA_V15", "ZA_V16"), condition = ~.x == 8)
d_temp_ZA_CoN <- d_temp_ZA_CoN %>% replace_with_na_at(.vars = c("ZA_V9", "ZA_V10", "ZA_V11", "ZA_V12", "ZA_V13", "ZA_V14", "ZA_V15", "ZA_V16"), condition = ~.x == 9)

scales_rescale <- function(v) {
  v_rescaled <- scales::rescale(v, to = c(1, 4), from = range(v, na.rm = F, finite = T))
  v_rescaled_rounded <- round(v_rescaled, 0)
  return(v_rescaled_rounded)
}

d_temp_ZA_CoN$ZA_V9 <- scales_rescale(d_temp_ZA_CoN$ZA_V9)
d_temp_ZA_CoN$ZA_V10 <- scales_rescale(d_temp_ZA_CoN$ZA_V10)
d_temp_ZA_CoN$ZA_V11 <- scales_rescale(d_temp_ZA_CoN$ZA_V11)
d_temp_ZA_CoN$ZA_V12 <- scales_rescale(d_temp_ZA_CoN$ZA_V12)
d_temp_ZA_CoN$ZA_V13 <- scales_rescale(d_temp_ZA_CoN$ZA_V13)
d_temp_ZA_CoN$ZA_V14 <- scales_rescale(d_temp_ZA_CoN$ZA_V14)
d_temp_ZA_CoN$ZA_V15 <- scales_rescale(d_temp_ZA_CoN$ZA_V15)
d_temp_ZA_CoN$ZA_V16 <- scales_rescale(d_temp_ZA_CoN$ZA_V16)

d_temp_ZA_CoN <- d_temp_ZA_CoN %>% rename(Born = ZA_V9, Citz = ZA_V10,
                                          Live = ZA_V11, Lang = ZA_V12,
                                          Relg = ZA_V13, Resp = ZA_V14,
                                          Feel = ZA_V15, Ancs = ZA_V16)
describe(d_temp_ZA_CoN[,2:9])

d13 <- read_stata("data/ZA5950_v2-0-0.dta") %>%
  as_tibble() %>% zap_labels() %>%
  select(C_ALPHAN,
         V7,
         V9, V10, V11,  V12, V13, V14, V15, V16, 
         V17, V19, V20, V21,
         V25, V26, V27, V28, V29, V30, V31, V32, V33, V34,
         V37, V45,
         V48, V49, V50, V51, V56,
         SEX, AGE, EDUCYRS, TOPBOT,PARTY_LR) %>%
  filter(C_ALPHAN %in% c("CZ","DK","FI","FR","DE-E","DE-W","GB-GBN","HU","IE","IL-A","IL-J",
                         "JP","LV","NO","PH","PT","RU","SI","SK","ZA","KR","ES","SE","CH","TW","US")) %>%
  dplyr::rename(cntr=C_ALPHAN,
                clC=V7,
                brn=V9,ctz=V10,lve=V11,lng=V12,rlg=V13,rsp=V14,fel=V15,anc=V16,
                Nci=V17,Nwb=V19,Ncb=V20,
                Pbs=V21,
                Pde=V25,Piw=V26,Pec=V27,Pss=V28,Psc=V29,Psp=V30,Par=V31,Paf=V32,Phi=V33,Peq=V34,
                Pbi=V37,
                ShC=V45,
                Icr=V48,Ige=V49,Itj=V50,Icl=V51,Inm=V56,
                gender=SEX,ageG=AGE,eduG=EDUCYRS,tbB=TOPBOT,POLR=PARTY_LR)

d13[20664:23402,3:10] <- NA
d13[20664:23402,3:10] <- d_temp_ZA_CoN[,2:9]
rm(d_temp_ZA_CoN)

d13$cntr <- as_factor(d13$cntr)
d13$cntr <- fct_collapse(d13$cntr,
                         DE=c("DE-E","DE-W"),
                         IL=c("IL-A","IL-J"))
d13$cntr <- recode_factor(d13$cntr,`GB-GBN` = "GB")

unique(d13$cntr)

d13 <- d13 %>% replace_with_na(replace = list(ageG =(999),
                                              eduG = c(95,96,98,99),
                                              tbB = c(98,99,0),
                                              POLR = c(0,6,7,97,98,99)))

d13 <- d13 %>% replace_with_na_at(.vars = c("clC",
                                            "brn","ctz","lve","lng","rlg","rsp","fel","anc",
                                            "Nci","Nwb","Ncb",
                                            "Pbs",
                                            "Pde","Piw","Pec","Pss","Psc","Psp","Par","Paf","Phi","Peq",
                                            "Pbi",
                                            "ShC",
                                            "Icr","Ige","Itj","Icl","Inm"), condition = ~.x == 8)
d13 <- d13 %>% replace_with_na_at(.vars = c("clC",
                                            "brn","ctz","lve","lng","rlg","rsp","fel","anc",
                                            "Nci","Nwb","Ncb",
                                            "Pbs",
                                            "Pde","Piw","Pec","Pss","Psc","Psp","Par","Paf","Phi","Peq",
                                            "Pbi",
                                            "ShC",
                                            "Icr","Ige","Itj","Icl","Inm",
                                            "gender"), condition = ~.x == 9)

d13 <- d13 %>% dplyr::mutate_at(c("Nci", "Nwb", "Ncb", "Pbs", "Pbi","ShC","Icr","Ige","Itj"),
                                funs(dplyr::recode(., `5`=1, `4`=2, `3`=3, `2`=4, `1`=5, .default = NaN)))

d13 <- d13 %>% dplyr::mutate_at(c("clC",
                                  "brn","ctz","lve","lng","rlg","rsp","fel","anc",
                                  "Pde","Piw","Pec","Pss","Psc","Psp","Par","Paf","Phi","Peq"),
                                funs(dplyr::recode(., `4`=1, `3`=2, `2`=3, `1`=4, .default = NaN)))

d13$gender <- factor(d13$gender, levels = c(1,2), labels = c("Male", "Female"))

d13$POLR <- ordered(d13$POLR,
                    levels = c(1,2,3,4,5),
                    labels = c("Far_Left", "Left",
                               "Center",
                               "Right","Far_Right"))

describe(d13)

d13 <- d13 %>% mutate(ageG = ntile(ageG, 4))
unique(d13$ageG)
d13$ageG <- factor(d13$ageG, levels = 1:4, labels = c("1Q", "2Q", "3Q", "4Q"))

d13 <- d13 %>% mutate(eduG = ntile(eduG, 4))
unique(d13$eduG)
d13$eduG <- factor(d13$eduG, levels = 1:4, labels = c("1Q", "2Q", "3Q", "4Q"))

d13 <- d13 %>% mutate(tbB = ntile(tbB, 2))
unique(d13$tbB)
d13$tbB <- factor(d13$tbB, levels = 1:2, labels = c("<med", ">med"))

d13$time <- "T2(2013)"


w13_country_level <- readxl::read_excel("data/00_data_prep.R_country_level_data_wQs.xlsx") %>%
  filter(wave_t1%in%"wave 3 (T2)") %>%
  select(country, GDPPPP_q:egaD_q) %>% rename(cntr = country)

d13 <- d13 %>% full_join(w13_country_level, by="cntr")

d13[factor_cols] <- lapply(d13[factor_cols], factor)

save(d13, file = "data/ISSP_NI_M3_2013.RData")

gdata::keep(list = c("d03", "d13"), sure = T)

d <- bind_rows(d03,d13)

d <- d %>% select(cntr, time:egaD_q, gender:POLR, clC:Inm) %>%
  rename(GDPPPP = GDPPPP_q,
         GINI = GINI_q,
         libD = libD_q,
         egaD = egaD_q)

describe(d)
unique(d$cntr)
n_distinct(d$cntr)

save(d, file = "data/ISSP_NI_M23.RData")
