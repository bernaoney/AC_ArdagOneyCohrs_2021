
load("data/ISSP_NI_M23_comp_miss_ranger.RData")
load("data/ISSP_NI_M23_comp_redu.RData")

colnames(d)

library(tidyverse)

d_temp <- d %>% select(cntr:POLR)
dat <- bind_cols(d_temp,reduced_data)

gdata::keep(dat, sure = T)
save(dat, file = "data/ISSP_NI_M23_comp_redu_comb.RData")

library(bootnet)
ni_nw_ggm <- estimateNetwork(dat[,12:23], default = "EBICglasso", tuning = 0.5, missing = "pairwise", threshold = T, lambda.min.ratio=0.01)
groups = c("national identification",
           "conceptions of nationhood", "conceptions of nationhood", "conceptions of nationhood",
           "out-group orientations", "out-group orientations", "out-group orientations",
           "conceptions of nationhood",
           "national identification", "national identification",
           "out-group orientations",
           "national identification")

ni_nw_comm_graph_lo <- plot(ni_nw_ggm, layout = "spring",
                            groups = groups, legend=F, maximum=1, edge.labels=T, edge.label.cex=1,
                            details = T, title = "Force-directed plotting with Fruchterman-Reingold")

library(qgraph)
ni_nw_wmat <- getWmat(ni_nw_ggm)
ni_nw_eigen <- ni_nw_wmat
diag(ni_nw_eigen) <- NA
p <- 2
library(eigenmodel)
fitEM <- eigenmodel_mcmc(Y = ni_nw_eigen, R = p, S = 10000, burn = 200, seed = 12345)
EVD <- eigen(fitEM$ULU_postmean) 
evecs <- EVD$vec[, 1:p]

ni_nw_comm_graph_lo_for_int_eigen <- plot(ni_nw_ggm, layout = evecs, palette = "ggplot2", theme = "colorblind",
                                          groups = groups, legend=F, maximum=1, edge.labels=T, edge.label.cex=1,
                                          vTrans = 200,
                                          details = T, title = "Eigenmodel configuration | After item-reduction")
