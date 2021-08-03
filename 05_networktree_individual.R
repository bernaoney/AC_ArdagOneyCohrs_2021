
load("data/ISSP_NI_M23_comp_redu_comb.RData")
colnames(dat)
library(networktree)

nodevar_names <- paste(names(dat[,12:23]), collapse = "+")
splitvars_names_ind <- paste(names(dat[,c(7:9)]), collapse = "+")
nw_f_sd <- as.formula(paste(c(nodevar_names, splitvars_names_ind), collapse= "~"))

ni_nwt_sode <- networktree(nw_f_sd, data = dat, method = "ctree", model = "correlation", transform = "glasso")
plot(ni_nwt_sode, type = "glasso", layout="circle", maximum=1, edge.labels=T, edge.label.cex=1, theme="colorblind")

splitvars_names_pol <- paste(names(dat[,10:11]), collapse = "+")
nw_f_pol <- as.formula(paste(c(nodevar_names, splitvars_names_pol), collapse= "~"))

ni_nwt_pol <- networktree(nw_f_pol, data = dat, method = "ctree", model = "correlation", transform = "glasso")
plot(ni_nwt_pol, type = "glasso", layout="circle", maximum=1, edge.labels=T, edge.label.cex=1, theme="colorblind")
