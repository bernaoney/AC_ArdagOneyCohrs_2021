
library(future)
plan(multisession)

load("data/ISSP_NI_M23_comp_miss_ranger.RData")
colnames(d)

round(prop.table(table(d$cntr)),2)
#   CH   CZ   DE   DK   ES   FI   FR   GB   HU   IE   IL   JP   KR   LV   NO   PH   PT   RU   SE   SI   SK   TW   US   ZA 
# 0.03 0.05 0.05 0.04 0.04 0.04 0.06 0.03 0.03 0.03 0.04 0.04 0.04 0.03 0.05 0.04 0.04 0.06 0.03 0.03 0.04 0.06 0.04 0.08 
round(prop.table(table(d$gender)),2)
# Male Female 
# 0.46   0.54
round(prop.table(table(d$time)),2)
#T1(2003) T2(2013) 
#     0.5      0.5

library(tidyverse)
set.seed(123)
train <- d %>% group_by(cntr, time, gender) %>% sample_frac(.70)
test <- d %>% anti_join(train)

round(prop.table(table(train$cntr)),2)
#   CH   CZ   DE   DK   ES   FI   FR   GB   HU   IE   IL   JP   KR   LV   NO   PH   PT   RU   SE   SI   SK   TW   US   ZA 
# 0.03 0.05 0.05 0.04 0.04 0.04 0.06 0.03 0.03 0.03 0.04 0.04 0.04 0.03 0.05 0.04 0.04 0.06 0.03 0.03 0.04 0.06 0.04 0.08 
round(prop.table(table(train$gender)),2)
# Male Female 
# 0.46   0.54
round(prop.table(table(train$time)),2)
#T1(2003) T2(2013) 
#     0.5      0.5
save(train, file = "data/train.RData")

round(prop.table(table(test$cntr)),2)
#   CH   CZ   DE   DK   ES   FI   FR   GB   HU   IE   IL   JP   KR   LV   NO   PH   PT   RU   SE   SI   SK   TW   US   ZA 
# 0.03 0.05 0.05 0.04 0.04 0.04 0.06 0.03 0.03 0.03 0.04 0.04 0.04 0.03 0.05 0.04 0.04 0.06 0.03 0.03 0.04 0.06 0.04 0.08
round(prop.table(table(test$gender)),2)
# Male Female 
# 0.46   0.54
round(prop.table(table(test$time)),2)
#T1(2003) T2(2013) 
#     0.5      0.5
save(test, file = "data/test.RData")

vars <- names(train[,12:41])

library(psychonetrics)
ggm_train <- ggm(train, vars = vars, estimator = "FIML") %>%
        runmodel %>%
        prune(alpha = 0.001, adjust = "fdr", recursive = T) %>% 
        stepup(alpha = 0.001, greedy = T, greedyadjust = "fdr", verbose = T)

adjacency <- 1*(getmatrix(ggm_train, "omega")!=0)
confirmatory <- ggm(test, vars = vars, omega = adjacency) %>% runmodel
confirmatory %>% fit

library(bootnet)
ni_nw_ggm <- estimateNetwork(d[,12:41], default = "EBICglasso", tuning = 0.5,
                             missing = "pairwise", threshold = T, lambda.min.ratio=0.01)
network <- 1*(ni_nw_ggm$graph != 0)

model_frombootnet <- ggm(train, vars = vars, omega = network) %>% runmodel
bootnet_net <- getmatrix(model_frombootnet, "omega")
confirmatory_net <- getmatrix(confirmatory, "omega")

ggm_train_omega <- ggm_train@modelmatrices[["fullsample"]][["omega"]]

library(qgraph)
L <- averageLayout(ggm_train_omega, bootnet_net, confirmatory_net)

ni_nw_comm_lo_chr <- c("comparative",
                       "CoN", "CoN", "CoN", "CoN", "CoN", "CoN", "CoN", "CoN",
                       "comparative", "comparative", "comparative", "out",
                       "NatPri", "NatPri", "NatPri", "NatPri", "NatPri", "NatPri", "NatPri", "NatPri", "NatPri", "NatPri", 
                       "out", "out", "out", "out", "out", "out", "out")

layout(t(1:3))
qgraph(ni_nw_ggm$graph, labels = vars, theme = "colorblind", 
       title = "National Identity Attitude Network
                \nStatistically Regularized Network",
       layout = L, groups = ni_nw_comm_lo_chr, legend=F, maximum=1, edge.labels=T, edge.label.cex=1, vTrans = 200)
qgraph(ggm_train_omega, labels = vars, theme = "colorblind", 
       title = "National Identity Attitude Network
                \nConfirmatory Model
                \nTraining Set", layout = L,
       groups = ni_nw_comm_lo_chr, legend=F, maximum=1, edge.labels=T, edge.label.cex=1, vTrans = 200)
qgraph(confirmatory_net, labels = vars, theme = "colorblind", 
       title = "National Identity Attitude Network
                \nConfirmatory Model
                \nTest Set", layout = L,
       groups = ni_nw_comm_lo_chr, legend=F, maximum=1, edge.labels=T, edge.label.cex=1, vTrans = 200)
dev.off()
