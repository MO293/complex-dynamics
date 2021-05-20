# Title     : Laboratorium5
# Objective : Zadanie5
# Created by: Maksymilian Odziemczyk
# Created on: 12.11.2020

rm(list=ls())
library(ggplot2)
data("mtcars")
automat <- data.frame(mtcars[mtcars$am == 0,])
manual <- data.frame(mtcars[mtcars$am == 1,])
df <- data.frame(KM = c(automat$hp, manual$hp), czas = c(automat$qsec,manual$qsec), mpg = c(automat$mpg,manual$mpg), biegi = c(rep("automat",length(automat$am)), rep("manual", length(manual$am))))
g <- ggplot(df)
points <- geom_point(aes(x = KM, y = czas, fill=biegi, size=mpg), shape=21)
labels <- ggtitle("Dane z magazynu Motor Trend (1974)")
xlab <- xlab("moc [KM]")
ylab <- ylab("czas na 1/4 mili [s]")
plot <- g + points + labels + xlab + ylab; plot
rm(list=ls())