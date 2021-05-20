# Title     : Laboratorium9
# Objective : Zadanie9
# Created by: Maksymilian Odziemczyk
# Created on: 10.12.2020

rm(list=ls())
library(ggplot2)
library(igraph)
library(igraphdata)
data("karate")

ploc <- function(dane) {
 plot(dane, vertex.size = 10, vertex.label.cex = 0.7, vertex.label.font = 2, vertex.label.color="black", edge.color="black", edge.arrow.width = 0.4)
}

original <- function(dane){
  png("293654_original.png")
  ploc(dane)
  dev.off()
}
new <- function(dane){
  colors <- c("#F8766D", "#00BFC4", "#7CAE00")
  V(dane)$color <- colors[fastgreedy.community(dane)$membership]
  png("293654_new.png")
  ploc(dane)
  dev.off()
}
histogram <- function(dane){
  df <- data.frame(s = graph.strength(dane), k = degree(dane))
  ggplot(df) +
  geom_histogram(aes(x = k), fill = "red", color = "red", alpha=0.2) +
  geom_histogram(aes(x = s), fill = "blue", color = "blue", alpha=0.2)
  ggsave("293654_histogram.png")
}
original(karate)
new(karate)
histogram(karate)