# Title     : Laboratorium11
# Objective : Zadanie11
# Created by: Maksymilian Odziemczyk
# Created on: 07.01.2021

rm(list=ls())
library("GA")

rastrigin <- function(x){
  return(-(10*length(x) + sum(x^2 - 10*cos(2*pi*x))))
}
gen <- function(pm){
  GA <- ga(type = 'real-valued', fitness = rastrigin, lower = c(-5.12, 5.12), upper = c(5.12, 5.12), pmutation = pm)
  return(which.max(GA@summary[,"max"]))
}
q <- 2/100 : 50/100
plot(q, sapply(q, gen), xlab = "Mutacja prawdopodobieństwa", ylab = "Najlepsza iteracja")