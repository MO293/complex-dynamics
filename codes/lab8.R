# Title     : Laboratorium8
# Objective : Zadanie8
# Created by: Maksymilian Odziemczyk
# Created on: 03.12.2020

rm(list=ls())
library(ggplot2)
library(igraph)

pp <- c(100,200,300,400,500,600,700,800,900,1000,1500,2000,3000,4000,5000,6000,7000,8000,9000,10000)
ba <- data.frame(data = pp, l = sapply(pp, function(x) average.path.length( barabasi.game(x, m=3, directed = F))))
er <- data.frame(data = pp, l = sapply(pp, function(x) average.path.length( erdos.renyi.game(x, 6/(x-1), "gnp" ,directed = F))))
g <- ggplot()
points1 <- geom_point(data = ba, aes(x = data, y = l, colour = "#F8766D"), size = 3)
points2 <- geom_point(data = er, aes(x = data, y = l, colour = "#00BFC4"), size = 3)
g + points1 + points2 + scale_color_discrete(name = "Legenda", labels = c("BA", "ER"))
ggsave("293654_apl_vs_size.png")

newSeq <- seq(4,40,2)
ba <- data.frame(data = newSeq, l = sapply(newSeq, function(x) average.path.length( barabasi.game(1000, m = x/2, directed = F))))
er <- data.frame(data = newSeq, l = sapply(newSeq, function(x) average.path.length( erdos.renyi.game(1000, x/(1000-1), "gnp" ,directed = F))))
g <- ggplot()
points1 <- geom_point(data = ba, aes(x = data, y = l, colour = "#F8766D"), size = 3)
points2 <- geom_point(data = er, aes(x = data, y = l, colour = "#00BFC4"), size = 3)
g + points1 + points2 + scale_color_discrete(name = "Legenda", labels = c("BA", "ER"))
ggsave("293654_apl_vs_degree.png")
rm(list=ls())
