
items_dic <- readxl::read_excel("data/ISSP_NI_M23.RData_items_dic.xlsx")
load("data/ISSP_NI_M23_comp_miss_ranger.RData")

library(EGAnet)
key.ind <- match(colnames(d[,12:41]), items_dic$item_id)
key <- items_dic$item[key.ind]

nw_re_la <- UVA(data = d[,12:41], model = "glasso", method = "wTO",
                type = "adapt", key = key,
                reduce = T, reduce.method = "latent",
                adhoc = T)

write.csv(nw_re_la$reduced$merged, "data/merged_items.csv")

library(tidyverse)
reduced_data <- data.frame(nw_re_la$reduced$data) %>% rename(clC = Feeling.close.to.country,
                                                             lng = Important.to.speak.the.language,
                                                             rsp = Important.to.respect.political.institutions...laws,
                                                             fel = Important.to.feel.nationality,
                                                             Pbs = Support.their.country.even.if.the.country.is.in.the.wrong,
                                                             Pbi = Country.should.follow.its.own.interests,
                                                             ShC = Those.who.do.not.share.customs...traditions.cannot.become...,
                                                             ntv = LV_8, dNP = LV_9, sNP = LV_10, CoO = LV_4, ImA = LV_11)

ega <- EGA(reduced_data, algorithm = "louvain", plot.EGA = F)
ega$wc

qgraph::qgraph(ega[["network"]], layout = "spring",
               legend = T, maximum = 1, edge.labels = T, edge.label.cex = 1,
               details = T, theme = "colorblind",
               groups = c("national identification",
                          "conceptions of nationhood", "conceptions of nationhood", "conceptions of nationhood",
                          "out-group orientations", "out-group orientations", "out-group orientations",
                          "conceptions of nationhood",
                          "national identification", "national identification",
                          "out-group orientations",
                          "national identification"),
               GLratio = .9, layoutScale = c(1,1))

save(reduced_data, file = "data/ISSP_NI_M23_comp_redu.RData")

# Compute standardized node strength
net.loads(ega)$std

# Compute bootstrap
boot <- bootEGA(reduced_data, uni.method = "LE", iter = 1000, type = "parametric",
                model = "glasso",
                algorithm = "louvain",
                plot.typicalStructure = F,
                ncores = 7)
# Compute structural consistency
sc <- dimStability(boot, orig.wc = ega$wc)
# Print structural consistency
sc$dimensions ##
#    1     2     3 
#0.992 1.000 0.928
# Item stability statistics plot
sc$items$plot.itemStability
# View item stability across dimensions
#View(sc$items$item.dim.rep)
