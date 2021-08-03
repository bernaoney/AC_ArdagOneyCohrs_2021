
load("data/ISSP_NI_M23_comp_redu_comb.RData")

library(tidyverse)
richer <- dat %>% filter(GDPPPP == ">med")
richer <- as.data.frame(richer)

groups = c("national identification",
           "conceptions of nationhood", "conceptions of nationhood", "conceptions of nationhood",
           "out-group orientations", "out-group orientations", "out-group orientations",
           "conceptions of nationhood",
           "national identification", "national identification",
           "out-group orientations",
           "national identification")
richer_ggm <- bootnet::estimateNetwork(richer[,12:23], default = "EBICglasso", tuning = 0.5, missing = "pairwise", threshold = T, lambda.min.ratio=0.01)
richer_lo <- plot(richer_ggm, layout = "spring",
                  groups = groups, legend = F, maximum = 1, edge.labels = T, edge.label.cex = 1,
                  palette = "ggplot2", theme = "colorblind",
                  details = T, title = "Richer Countries | Bidirectional Relationships")

richer_ggm[["graph"]]
blacklist_richer <- readxl::read_excel("data/from_to_blacklist_richer.xlsx")

library(parallel)
cl <- makeCluster(7)

library(bnlearn)
set.seed(123)
bootnet_rich <- boot.strength(data = richer[,12:23], R = 1000, algorithm = "tabu", algorithm.args = c(blacklist = blacklist_richer), cluster = cl)
avgnet_rich_threshold <- averaged.network(bootnet_rich, threshold = 0.95)

boottab_rich <- bootnet_rich[bootnet_rich$strength > 0.95 & bootnet_rich$direction > 0.50, ]
astr_rich <- boottab_rich
astr_rich$strength <- astr_rich$direction
strength.plot(avgnet_rich_threshold, astr_rich, shape = "ellipse", main = "Bayesian Network of the richer countries")

rich_igraph <- bnviewer::bn.to.igraph(avgnet_rich_threshold)
library(igraph)
E(rich_igraph)$weight <- astr_rich$strength

sort(degree(rich_igraph, mode = "in"), decreasing = T)
# outcomes
# Pbs clC CoO fel Pbi lng rsp ImA sNP ntv ShC dNP 
#  7   6   6   5   5   3   3   3   2   1   0   0
sort(degree(rich_igraph, mode = "out"), decreasing = T)
# causes
# ShC ntv dNP sNP ImA lng CoO rsp fel Pbs clC Pbi 
#  7   7   7   5   4   3   3   2   2   1   0   0

par(mfrow=c(1,3))
plot(richer_ggm, layout = richer_lo[["layout.orig"]], label.cex = 2,
     groups = groups, legend = F, maximum = 1, edge.labels = F,
     palette = "ggplot2", theme = "colorblind",
     details = F, title = "(A) Richer Countries | Bidirectional Relationships")
qgraph::qgraph(avgnet_rich_threshold, vTrans = 200, layout = richer_lo[["layout.orig"]],
               vsize = (degree(rich_igraph, mode = "out")*2),
               esize = (E(rich_igraph)$weight)*3, edge.width = 2,
               groups = groups, palette = "ggplot2", theme = "colorblind", details = F, legend = F,
               label.cex = 2, title = "(B) Richer Countries | Outgoing Centrality | Central Causes")
qgraph::qgraph(avgnet_rich_threshold, vTrans = 200, layout = richer_lo[["layout.orig"]],
               vsize = (degree(rich_igraph, mode = "in")*2),
               esize = (E(rich_igraph)$weight)*3, edge.width = 2,
               groups = groups, palette = "ggplot2", theme = "colorblind", details = F, legend = F,
               label.cex = 2, title = "(C) Richer Countries | Incoming Centrality | Central Outcomes")
dev.off()

gdata::keep(list = c("dat", "groups", "richer_lo", "astr_rich", "cl"), sure = T)

poorer <- dat %>% filter(GDPPPP == "<med")
poorer <- as.data.frame(poorer)

poorer_ggm <- bootnet::estimateNetwork(poorer[,12:23], default = "EBICglasso", tuning = 0.5, missing = "pairwise", threshold = T, lambda.min.ratio=0.01)
poorer_lo <- plot(poorer_ggm, layout = richer_lo[["layout.orig"]],
                  groups = groups, legend = F, maximum = 1, edge.labels = T, edge.label.cex = 1,
                  palette = "ggplot2", theme = "colorblind",
                  details = T, title = "Poorer Countries | Bidirectional Relationships")

poorer_ggm[["graph"]]
blacklist_poorer <- readxl::read_excel("data/from_to_blacklist_poorer.xlsx")

set.seed(321)
bootnet_poor <- boot.strength(data = poorer[,12:23], R = 1000, algorithm = "tabu", algorithm.args = c(blacklist = blacklist_poorer), cluster = cl)
avgnet_poor_threshold <- averaged.network(bootnet_poor, threshold = 0.95)

boottab_poor <- bootnet_poor[bootnet_poor$strength > 0.95 & bootnet_poor$direction > 0.50, ]
astr_poor <- boottab_poor
astr_poor$strength <- astr_poor$direction
strength.plot(avgnet_poor_threshold, astr_poor, shape = "ellipse", main = "Bayesian Network of the Poorer Countries")

rio::export(list(astr_poor = astr_poor[,1:3],
                 astr_rich = astr_rich[,1:3]), file = "data/BayesianN_archs.xlsx")

poor_igraph <- bnviewer::bn.to.igraph(avgnet_poor_threshold)
E(poor_igraph)$weight <- astr_poor$strength
sort(degree(poor_igraph, mode = "in"), decreasing = T)
# dNP clC sNP ImA lng ShC rsp Pbi Pbs fel CoO ntv 
#  8   7   7   5   4   4   3   3   2   1   1   0
sort(degree(poor_igraph, mode = "out"), decreasing = T)
# CoO fel ntv rsp Pbs lng ShC Pbi dNP ImA sNP clC 
#  8   7   7   5   5   3   3   2   2   2   1   0 

par(mfrow=c(1,3))
plot(poorer_ggm, layout = richer_lo[["layout.orig"]],
     groups = groups, legend = F, maximum = 1, edge.labels = F,
     palette = "ggplot2", theme = "colorblind",
     details = F, title = "(A) Poorer Countries | Bidirectional Relationships")
qgraph::qgraph(avgnet_poor_threshold, vTrans = 200, layout = richer_lo[["layout.orig"]],
               vsize = (degree(poor_igraph, mode = "out")*2),
               esize = (E(poor_igraph)$weight)*3, edge.width = 2,
               groups = groups, palette = "ggplot2", theme = "colorblind", details = F, legend = F,
               label.cex = 2, title = "(B) Poorer Countries | Outgoing Centrality | Central Causes")
qgraph::qgraph(avgnet_poor_threshold, vTrans = 200, layout = richer_lo[["layout.orig"]],
               vsize = (degree(poor_igraph, mode = "in")*2),
               esize = (E(poor_igraph)$weight)*3, edge.width = 2,
               groups = groups, palette = "ggplot2", theme = "colorblind", details = F, legend = F,
               label.cex = 2, title = "(C) Poorer Countries | Incoming Centrality | Central Outcomes")
dev.off()

stopCluster(cl)
