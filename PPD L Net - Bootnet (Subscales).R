
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

groups <- factor(c(
  rep("Unemotionality",1),
  rep("Lying",1),
  rep("Manipulation",1),
  rep("Thrill Seeking",1),
  rep("Impulsiveness",1),
  rep("Irresponsibility",1),
  rep("Versatility",1),
  rep("Callousness",1),
  rep("Dishonest Charm",1),
  rep("Grandiosity",1),
  rep("Remorselessness",1)))

Network <- estimateNetwork(YPIMIS.imp$ximp, default = "EBICglasso")
plot(Network, layout = "spring", groups = groups)


Results1 <- bootnet(Network, nBoots = 2500, statistics = c("all"), type = "case", nCores = 6)
corStability(Results1)

