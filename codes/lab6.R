# Title     : Laboratorium6
# Objective : Zadanie6
# Created by: Maksymilian Odziemczyk
# Created on: 19.11.2020

library(MASS)
library(dplyr)
library(ggplot2)
rm(list=ls())

partOne <- function(data){
  comm <- sample(data$comments, 100)
  emoo <- sample(data$emo, 100)
  acf.comments <- acf(comm, plot=FALSE)
  acf.emo <- acf(emoo, plot = FALSE)
  df.acf <- data.frame(lag = acf.emo$lag, comments = acf.comments$acf, emo = acf.emo$acf)
  g <- ggplot(df.acf, aes(x = lag))
  points.comments <- geom_point(aes(y = comments, fill="Comments"), shape=21, size=4)
  points.emo <- geom_point(aes(y = emo, fill="Emo"), shape=21, size=4)
  scale <- scale_fill_manual("Autocorrelation", breaks = c("Comments", "Emo"), values = c("#F8766D", "#00BFC4"))
  plt <- g + points.emo + points.comments + scale; plt
  ggsave("293654_lab6_plot1.png")
  rm(list=ls())
}

partTwo <- function(data){
  data$V2 %>%
    ts(freq=4) %>%
    decompose() -> newDf

  myRandoms<-acf(newDf$random[!is.na(newDf$random)], lag.max = 100, plot=FALSE)
  myIndex<-acf(newDf$x[!is.na(newDf$x)], lag.max = 100, plot=FALSE)
  df.acf <- data.frame(lag = myIndex$lag, Random = myRandoms$acf, Normal = myIndex$acf)

  g <- ggplot(df.acf, aes(x = lag))
  points.random <- geom_point(aes(y = Random, fill="Random"), shape=21, size=3)
  points.normal <- geom_point(aes(y = Normal, fill="All"), shape=21, size=3)
  scale <- scale_fill_manual("Autocorrelation", breaks = c("Random", "All"), values = c("#7CAE00", "#C77CFF"))
  plt <- g + points.random + points.normal + scale; plt
  ggsave("293654_lab6_plot2.png")
}
df1 <- read.table("cyber.dat")
df2 <- read.table("wig20.dat")
partOne(df1)
partTwo(df2)
rm(list=ls())
