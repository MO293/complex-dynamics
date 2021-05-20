# Title     : Laboratorium10
# Objective : Zadanie10
# Created by: Maksymilian Odziemczyk
# Created on: 17.12.2020

rm(list = ls())
library(klaR)
library(MASS)
library(scatterplot3d)
library(ggplot2)

gaus_data <- rbind(mvrnorm(60, c(2, 2), matrix(c(3, 0, 0, 3), 2, 2)), mvrnorm(20, c(-1, -1), matrix(c(3, 0, 0, 3), 2, 2)))
df <- data.frame(x = gaus_data[, 1], y = gaus_data[, 2], z = gaus_data[, 1] + gaus_data[, 2])
png('293654_3dplot.png')
scatterplot3d(x = df$x, y = df$y, z = df$z, main = "Dane", xlab = "x", ylab = "y", zlab = "z")
dev.off()

PCA <- princomp(~., cor = T, data = df)
png('293654_pca.png')
plot(PCA)
dev.off()

klasy <- as.factor(c(rep("1", 60), rep("2", 20)))
df2 <- data.frame(x = klasy, y = PCA$scores[, 1], z = PCA$scores[, 2])
theme_set(theme_bw())
data.lda <- lda(klasy ~ PCA$scores[, 1] + PCA$scores[, 2], data = df2)
ggplot(df2) +
  geom_point(aes(x = PCA$scores[, 2], y = PCA$scores[, 1], color = klasy), shape = 19, size = 3) +
  geom_function(aes(x=PCA$scores[, 1]),fun=function(xx) data.lda$scaling[2,]*xx + data.lda$scaling[1,]) +
  labs(x = "Comp1", y = "Comp2")
ggsave('293654_lca.png')

print('Done.')