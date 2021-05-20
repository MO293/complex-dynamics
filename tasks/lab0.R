# Title     : Laboratorium0
# Objective : Zadanie0
# Created by: Maksymilian Odziemczyk
# Created on: 15.10.2020

f <- function(x,div){
  x[x %% div == 0]
}
x <- -120:176 #dowolny ciąg liczb
div <- 7 #przez co ma być dzielona
f(x,div)