
load("data/ISSP_NI_M23.RData")
colnames(d)

library(tidyverse)
num_var <- d %>% select(clC:Inm)
library(reshape2)
bi <- ggplot(melt(num_var),aes(x=value)) + geom_density() + facet_wrap(~variable) + theme_bw() +
  labs(subtitle = "Before imputation")

naniar::vis_miss(num_var, warn_large_data = F)

library(missRanger)
non_miss <- rowSums(!is.na(d[,12:41]))
d_num_imp <- num_var %>%
  missRanger(verbose = 1, formula = . ~ ., num.trees = 100,
             maxiter = 100, pmm.k = 10,
             seed = 666, case.weights = non_miss)
ai <- ggplot(melt(d_num_imp),aes(x=value)) + geom_density() + facet_wrap(~variable) + theme_bw() +
  labs(subtitle = "Before imputation")
gridExtra::grid.arrange(bi, ai, nrow=2, top = "Density plots of all survey-items used in study")

d[,12:41] <- d_num_imp
save(d, file = "data/ISSP_NI_M23_comp_miss_ranger.RData")
