
library(future)
plan(multisession)

load("data/ISSP_NI_M23_comp_miss_ranger.RData")
colnames(d)
library(tidyverse)
#unique(d$cntr)
#sample_df_long <- d %>% group_by(cntr, time) %>% summarise(COUNT = n())
#sample_df_wide <- sample_df_long %>% spread(time, COUNT)
#rio::export(sample_df_wide, file = "sample_df_wide.xlsx")

library(bootnet)
ni_nw_ggm <- estimateNetwork(d[,12:41], default = "EBICglasso", tuning = 0.5, missing = "pairwise", threshold = T, lambda.min.ratio = 0.01)
library(qgraph)
ni_nw_centralityPlot <- centralityPlot(ni_nw_ggm, scale = "z-scores", include = c("Strength","ExpectedInfluence"),
                                       orderBy = "ExpectedInfluence")
plot(ni_nw_ggm, layout = "spring", maximum=1, edge.labels=T, edge.label.cex=1, vTrans = 200,
     title = "Force-directed plotting with Fruchterman-Reingold")

ni_nw_wmat <- getWmat(ni_nw_ggm)
library(igraph)
ni_nw_gam <- graph_from_adjacency_matrix(abs(ni_nw_wmat), "undirected", weighted = T, add.colnames = F)

#################################################################
# community detection with louvain
ni_nw_comm_lo <- cluster_louvain(ni_nw_gam)
ni_nw_comm_lo_int <- ni_nw_comm_lo$membership; ni_nw_comm_lo_int
# 3 1 1 1 1 1 1 1 1 3 3 3 4 2 2 2 2 2 2 2 2 2 2 4 4 4 4 4 4 4
ni_nw_comm_lo_int <- c("3", "1", "1", "1", "1", "1", "1", "1", "1",
                       "3", "3", "3", "4", "2", "2", "2", "2", "2",
                       "2", "2", "2", "2", "2", "4", "4", "4", "4",
                       "4", "4", "4")
ni_nw_comm_graph_lo <- plot(ni_nw_ggm, layout = "spring", palette = "colorblind",
                            groups = ni_nw_comm_lo_int, legend=T, maximum=1, edge.labels=T, edge.label.cex=1,
                            details = T, title = "Force-directed plotting with Fruchterman-Reingold")

ni_nw_comm_lo_chr <- c("comparative",
                       "CoN", "CoN", "CoN", "CoN", "CoN", "CoN", "CoN", "CoN",
                       "comparative", "comparative", "comparative", "out",
                       "NatPri", "NatPri", "NatPri", "NatPri", "NatPri", "NatPri", "NatPri", "NatPri", "NatPri", "NatPri", 
                       "out", "out", "out", "out", "out", "out", "out")

#par(mfrow=c(1,2))
ni_nw_comm_graph_lo <- plot(ni_nw_ggm, layout = "spring",
                            groups = ni_nw_comm_lo_chr, legend = T, maximum = 1, edge.labels = T, edge.label.cex = 1,
                            details = T, title = "Force-directed plotting with Fruchterman-Reingold")

Names <- scan("data/ISSP_NI_M23.RData_items_leg.txt", what = "character", sep = "\n")
nw_graph <- plot(ni_nw_ggm, layout = ni_nw_comm_graph_lo$layout,
                 legend=T, maximum=1, edge.labels=F,
                 nodeNames = Names, legend.cex = 0.3, vsize = 5, details = F, title = "National Identity Attitude Network // Node Labels in Legend")
#dev.off()

ni_nw_eigen <- ni_nw_wmat
diag(ni_nw_eigen) <- NA
p <- 2
library(eigenmodel)
fitEM <- eigenmodel_mcmc(Y = ni_nw_eigen, R = p, S = 10000, burn = 200, seed = 123)
EVD <- eigen(fitEM$ULU_postmean)
evecs <- EVD$vec[, 1:p]
round(colMeans(evecs),3)

ni_nw_comm_graph_lo_for_int_eigen <- plot(ni_nw_ggm, layout = evecs,
                                          groups = ni_nw_comm_lo_chr, legend = F, maximum = 1, edge.labels = T, edge.label.cex = 1,
                                          vTrans = 200,
                                          details = T,
                                          title = "Eigenmodel configuration | same network plotted on a 2-D latent space so that the x- & y- axis are interpretable")

round(smallworldIndex(ni_nw_comm_graph_lo)$index,2) # 1.15
qgraph::smallworldness(ni_nw_gam, B = 1001, up = 0.995, lo = 0.005) # 1.0487516

nw_igo <- as.igraph(ni_nw_comm_graph_lo, attributes=T)
round(graph.density(nw_igo, loops=F),2) # 0.5
round(igraph::transitivity(nw_igo),2) # 0.53
round(average.path.length(nw_igo),2) # 1.5
round(centralization.degree(nw_igo)$centralization,2) # 0.16
round(centralization.closeness(nw_igo)$centralization,2) # 0.16
round(centralization.betweenness(nw_igo)$centralization,2) # 0.03

simulationnw <- lapply(1:1001, erdos.renyi.game,
                       n = length(V(nw_igo)),
                       p.or.m = length(E(nw_igo)), type="gnm")
simulationnw.c <- sapply(simulationnw, igraph::transitivity)
simulationnw.l <- sapply(simulationnw, average.path.length)
hist(simulationnw.c, breaks=20)
hist(simulationnw.l, breaks=20)
plot(density(simulationnw.c))
plot(density(simulationnw.l))
simulationnw.c.alpha <- quantile(simulationnw.c, probs=c(.025, .975))
simulationnw.c.alpha
simulationnw.l.alpha <- quantile(simulationnw.l, probs=c(.025, .975))
simulationnw.l.alpha

par(mfrow=c(1,2))
plot(density(simulationnw.c), main="Clustering", xlim=c(.35,.6))
abline(v=simulationnw.c.alpha, col="green")
abline(v=igraph::transitivity(nw_igo), col="red", lty=2)
plot(density(simulationnw.l), main="Mean geodesic", xlim = c(1.4,1.94))
abline(v=simulationnw.l.alpha, col="green")
abline(v=average.path.length(nw_igo), col="red", lty=2)
# judgind by the simulation of a 1001 random networks
# the clustering coeffient & average short lenght values of the network
# is significantly different from what we would expect from random networks.
dev.off()

###Testing Connectivity
#### SW Index von Dalege 2015: https://surfdrive.surf.nl/files/index.php/s/w2Zfq0ArRCNQIOt
#### More about ASPL: https://www.researchgate.net/publication/316617763_A_Network_Perspective_on_Political_Attitudes_Testing_the_Connectivity_Hypothesis
#### More on Shortest path length: https://cran.r-project.org/web/packages/qgraph/qgraph.pdf


SW_Index <- function (Graph, ci = c (.1, .05, .01, .001))
{
  randomC <- vector (, 1001)
  randomL <- vector (, 1001)
  for (i in 1:1001)
  {
    Rgraph <- erdos.renyi.game (vcount (Graph), ecount (Graph), 'gnm')
    randomC [i] <- igraph::transitivity (Rgraph, 'average')
    randomL [i] <- average.path.length(Rgraph)
  }
  MrandomC <- mean (randomC)
  MrandomL <- mean (randomL)
  Clustering.Graph = igraph::transitivity (Graph, 'average')
  ASPL.Graph = average.path.length (Graph)
  Index <- (Clustering.Graph / MrandomC) / (ASPL.Graph / MrandomL)
  
  sm_sample <- vector (, 1001)
  for (i in 1:1001)
  {
    Rgraph <- erdos.renyi.game (vcount (Graph), ecount (Graph), 'gnm')
    sm_sample [i] <- (igraph::transitivity (Rgraph, 'average') / MrandomC) /(average.path.length(Rgraph) / MrandomL)
  }
  CI <- as.vector (((quantile (sm_sample, 1 - (ci / 2)) - quantile (sm_sample, ci / 2)) / 2) + 1)
  return (list (SW.Index = Index, Upper.CI = data.frame (CI = ci, Value.CI = CI), 
                Clustering.Graph = Clustering.Graph, Clustering.Random.Graph = MrandomC,
                ASPL.Graph = ASPL.Graph, ASPL.Random.Graph = MrandomL))
}


SW_Index (ni_nw_gam)

# values are going to be different slightly every time; changes in the 3rd decimal

#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2323569/

#$SW.Index
#[1] 1.096617

#$Upper.CI
#CI Value.CI
#1 0.100 1.040656
#2 0.050 1.050077
#3 0.010 1.067866
#4 0.001 1.076460

#$Clustering.Graph
#[1] 0.5475522

#$Clustering.Random.Graph
#[1] 0.4993438

#$ASPL.Graph
#[1] 1.501149

#$ASPL.Random.Graph
#[1] 1.50125
