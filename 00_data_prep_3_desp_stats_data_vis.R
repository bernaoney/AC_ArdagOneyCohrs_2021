
load("data/ISSP_NI_M23.RData")

library(tidyverse)
library(psych)
library(knitr)
ds_tab <- as_tibble(describe(d[,12:41])) %>% select(n:se)
ds_tab$n <- 1-(ds_tab$n/65630)
ds_tab %>% select(n:median,min:max,skew:se) %>% rename(miss_perc = n) %>% kable(digits = 3)

library(reshape2)
ggplot(melt(d [, c(12:41)]), aes(x=value)) + geom_histogram() + facet_wrap(~variable) + theme_bw()

library(janitor)
ct_d <- d %>% tabyl(cntr, time)
ct_d %>% adorn_percentages("row") %>% adorn_pct_formatting(digits = 1) %>% adorn_ns() %>% kable()

df <- d %>% select(cntr, time) %>% group_by(cntr, time) %>% summarise(counts = n())

ggplot(df, aes(x = cntr, y = counts)) +
  geom_bar(aes(color = time, fill = time), stat = "identity") +
  theme_bw() +
  #scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))+
  #scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +
  geom_text(aes(label = counts, group = time), color = "white") +
  #ggrepel::geom_label_repel(label=df$counts, size=2, check_overlap = T) +
  xlab("Sample % of Country by time") +
  ylab("Frequency") + 
  labs(title="National Identity Modules 2 & 3", 
       caption = "Source: International Social Survey Program")

library(strengejacke)
plot_frq(d$cntr, sort.frq = "asc", type = "bar", geom.colors = "grey", vjust = "middle") + 
  theme_bw() +
  xlab("Sample % of Country by time") +
  ylab("Frequency") + 
  labs(title="National Identity Modules 2 & 3", 
       caption = "Source: International Social Survey Program")

library(readxl)
cld <- read_excel("data/00_data_prep.R_country_level_data_wQs.xlsx")

unique(cld$country)
n_distinct(cld$country) # 24

`%notin%` <- Negate(`%in%`)

cld <- cld %>% filter(country%notin%c("JP", "CH", "TW"))
unique(cld$country)
n_distinct(cld$country) # 21

gdp <- cld %>% ggplot(aes(x=GDPPPP)) + geom_histogram() + theme_bw() + ggtitle("GDP per capita T1 (2003) & T2 (2013)")
gdp_bar <- cld %>% ggplot(aes(x=GDPPPP_q)) + geom_bar() + theme_bw() + ggtitle("GDP per capita T1 (2003) & T2 (2013) Binarized")
gini <- cld %>% ggplot(aes(x=GINI)) + geom_histogram() + theme_bw() + ggtitle("GINI T1 (2003) & T2 (2013)")
gini_bar <- cld %>% ggplot(aes(x=GINI_q)) + geom_bar() + theme_bw() + ggtitle("GINI T1 (2003) & T2 (2013) Binarized")
LibD <- cld %>% ggplot(aes(x=LibD)) + geom_histogram() + theme_bw() + ggtitle("vDEM Liberal Democracy Score T1 (2003) & T2 (2013)")
LibD_bar <- cld %>% ggplot(aes(x=libD_q)) + geom_bar() + theme_bw() + ggtitle("vDEM Liberal Democracy Score T1 (2003) & T2 (2013) Binarized")
EgaD <- cld %>% ggplot(aes(x=EgaD)) + geom_histogram() + theme_bw() + ggtitle("vDEM Egalitarian Democracy Score T1 (2003) & T2 (2013)")
EgaD_bar <- cld %>% ggplot(aes(x=egaD_q)) + geom_bar() + theme_bw() + ggtitle("vDEM Egalitarian Democracy Score T1 (2003) & T2 (2013) Binarized")

library(gridExtra)
grid.arrange(gdp,gdp_bar,
             gini,gini_bar,
             LibD,LibD_bar,
             EgaD,EgaD_bar,
             nrow=4, ncol=2)
