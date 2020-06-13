
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




#creates categories to show up on the network figure

groups <- factor(c(
  rep("Criminal Versatility",1),
  rep("Grandiose-Manipulative",1),
  rep("Callous-Unemotional",1),
  rep("Impulsive-Irresponsible",1)))
  



# Input:
vars <- c("Versatility", "GM", "CU", "II")
idvar <- "CASEID"



# Via graphicalVAR:
library("graphicalVAR")
res_GVAR <- mlGraphicalVAR(YPIMIS.imp$ximp, vars, idvar = idvar, centerWithin = TRUE, gamma = 0.25, lags = 1,
                           subjectNetworks = FALSE, nLambda = 50)

###Note about the above model - default lambda = 50 anyway



layout(t(1:1))
qgraph(res_GVAR$betweenNet, layout = "spring", nonsig = "hide", rule = "and", theme = "colorblind", title = "Between-subjects",
       labels = vars, vsize = 10,  asize = 10, mar = c(5,5,5,5))
box("figure")

layout(t(1:2))
qgraph(res_GVAR$fixedPDC,  layout = "spring", nonsig = "hide", rule = "and", theme = "colorblind", title = "Temporal",
       labels = vars, vsize = 10,  asize = 10, mar = c(5,5,5,5))
box("figure")
qgraph(res_GVAR$fixedPCC,  layout = "spring", nonsig = "hide", rule = "and", theme = "colorblind", title = "Contemporaneous",
       labels = vars, vsize = 10, asize = 10, mar = c(5,5,5,5))
box("figure")


ew <- res_GVAR$betweenNet [upper.tri(res_GVAR$betweenNet)]
sum(ew != 0) # the number of edges
sum(ew > 0) # the number of positive edges
sum(ew < 0) # the number of negative edges


ew <- res_GVAR$fixedPDC [upper.tri(res_GVAR$fixedPDC)]
sum(ew != 0) # the number of edges
sum(ew > 0) # the number of positive edges
sum(ew < 0) # the number of negative edges


ew <- res_GVAR$fixedPCC [upper.tri(res_GVAR$fixedPCC)]
sum(ew != 0) # the number of edges
sum(ew > 0) # the number of positive edges
sum(ew < 0) # the number of negative edges



#########################FOR THE CENTRALITY INDICES FIGURE#############################

cnt <- bind_rows(data.frame(centralityTable(res_GVAR$betweenNet),
                            "group" = "Between-Subjects"),
                 data.frame(centralityTable(res_GVAR$fixedPDC),
                            group = "Temporal"),
                 data.frame(centralityTable(res_GVAR$fixedPCC),
                            group = "Contemporaneous"))


cnt <- rbind(cnt) %>% arrange(group, node)
cnt$node <- str_sub(cnt$node, 1, 6)

library("ggplot2")
library("ggthemes")


gl <- ggplot(filter(cnt),
             aes(x = value,
                 y = node,
                 group = group,
                 color = group,
                 shape = group))+
  geom_path(alpha = 1, size = 1)+
  geom_point(size = 2) +
  xlab("") + ylab("") +
  facet_grid(. ~ measure, scales = "free") +
  scale_x_continuous(breaks = c(-1, 0, 1)) +
  theme_few() %+replace%
  theme(panel.border = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.y = element_line(colour = "white", size = 0),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(angle = 45, colour = "black"))
gl




