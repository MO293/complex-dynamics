# Title     : Laboratorium3
# Objective : Zadanie3
# Created by: Maksymilian Odziemczyk
# Created on: 29.10.2020

library("fields")
library("Hmisc")
data("ChickWeight")

dane <- data.frame(ChickWeight[ChickWeight$Diet == 1,])
x <- dane$Time
y <- dane$weight
xy.sb <- stats.bin(dane$Time, dane$weight)
x.sb <- xy.sb$centers
N.sb <- xy.sb$stats[1,]
y.sb <- xy.sb$stats[2,]
e.sb <- xy.sb$stats[3,]
xy.lm <- lm(y.sb ~ x.sb)
a <- xy.lm$coefficients[2]
b <- xy.lm$coefficients[1]
plot(x, y, xlab = "Dzien zycia", ylab = "Waga [g]")
lines(x, a * x + b, col = "blue", lwd = 3)
errbar(x.sb, y.sb, y.sb + e.sb / sqrt(N.sb), y.sb - e.sb / sqrt(N.sb), add = TRUE, col = "red", errbar.col = "red")
saveFrame <- data.frame(x = x.sb, y = y.sb, std = e.sb)
write.table(format(saveFrame, digits = 3), "293654_ramka_danych.txt", quote = FALSE)
rm(list = ls())