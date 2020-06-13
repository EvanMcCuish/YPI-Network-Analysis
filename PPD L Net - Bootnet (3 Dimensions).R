
library(foreign)
db = file.choose()
YPIMIS = read.spss(db, to.data.frame = TRUE)

library("missForest")


YPIMIS.imp <- missForest(YPIMIS, verbose = TRUE)

YPIMIS.imp$OOBerror

YPIMIS.imp$ximp


# Load packages: 
library("mlVAR")
library("graphicalVAR")
library("qgraph")
library("dplyr")
library("qgraph")
library("EstimateGroupNetwork")
library("stringr")
library("ggplot2")
library("ggthemes")
library("networktools")


library("qgraph")
library("bootnet")
library("networktools")




#creates categories to show up on the network figure

groups <- factor(c(
  rep("Criminal Versatility",1),
  rep("Grandiose-Manipulative",1),
  rep("Callous-Unemotional",1),
  rep("Impulsive-Irresponsible",1)))

Network <- estimateNetwork(YPIMIS.imp$ximp, default = "EBICglasso")
plot(Network, layout = "spring", groups = groups)


Results1 <- bootnet(Network, nBoots = 2500, statistics = c("strength"), type = "case", nCores = 6)
corStability(Results1)


Results1 <- bootnet(Network, nBoots = 2500, statistics = c("betweenness"), type = "case", nCores = 6)
corStability(Results1)


Results1 <- bootnet(Network, nBoots = 2500, statistics = c("closeness"), type = "case", nCores = 6)
corStability(Results1)

