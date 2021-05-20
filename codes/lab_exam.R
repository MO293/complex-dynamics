# Title     : Kolokwium
# Objective : Główne
# Created by: Maksymilian Odziemczyk
# Created on: 21.01.2021

library(ggplot2)
library(latex2exp)
library(igraph)
rm(list = ls())


df1 <- read.table("bilans_przeplywow_tab4.txt")
new_df <- read.table("bilans_przeplywow_nazwy_kolumn.txt", sep = "\t", encoding = "UTF-8")

A <- data.matrix(df1)

rownames(A) <- new_df$V2
colnames(A) <- new_df$V2

d1 <- apply(A, 1, sum)
d2 <- apply(A, 2, sum)
which.max(d1)
which.max(d2)

0 -> A[A < 0]
hist(A[A > 0], breaks = max(A) / 0.001, xlim = c(0, round(max(A), digits = 2))) -> H

data.frame(mids = H$mids, counts = H$counts) -> df
ggplot(df) + geom_point(data = df, aes(x = mids, y = counts), shape = 1) +
  scale_x_log10(limits = c(min(df$mids), max(df$mids))) +
  scale_y_log10() +
  xlab(TeX("$\\a_{ij}$"))+
  ylab("counts") -> gg1
ggsave("293654_wykres1.pdf")
graph.adjacency(A, mode="directed", weighted=TRUE, diag=FALSE) -> G
plot(G, vertex.label=NA, vertex.size = 8, edge.arrow.width = 0.5)

seq(0, 0.5, 0.001) -> p
lapply(p, function(x) delete.edges(G, which(E(G)$weight < x))) -> L
lapply(L, function(x) mean(degree(x))) -> k
lapply(L, function(x) transitivity(x,"localaverage")) -> C

new_df<-data.frame(p=p, y_k=unlist(lapply(L, function(x) mean(degree(x)))), y_c=unlist(lapply(L, function(x) transitivity(x, "localaverage"))))
ggplot(new_df, aes(x=p)) +
  geom_point(aes(y = y_k/max(new_df$y_k), fill="C"), color="red", shape=21, size=3) +
  geom_point(aes(y = y_c/max(new_df$y_c), fill="<l>"), color="blue", shape=21, size=3) +
  scale_fill_manual("Legend", breaks = c("<l>", "C"), values = c("blue", "red"))+xlab("p")+ylab("<k>") -> gg2
ggsave("293654_wykres2.pdf")