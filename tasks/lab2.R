# Title     : Laboratorium2
# Objective : Zadanie2
# Created by: Maksymilian Odziemczyk
# Created on: 22.10.2020

numbs <- rnorm(10000, 1.5, 0.75)
h <- hist(x = numbs, plot = FALSE)
args <- seq(-3, 6, 0.1)
plot(h$mids, h$density, main = "Punkty z dopasowaniem", pch = 16, xlab = "x", ylab = "f(x)", ylim = c(0, 0.7))
lines(args, dnorm(args, 1.5, 0.75), col = "red", lty = 1)
legend("topleft", legend = c("data", "fit"), col = c("black", "red"), pch = c(16, NA), lty = c(NA, 1))