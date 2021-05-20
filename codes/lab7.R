# Title     : Laboratorium7
# Objective : Zadanie7
# Created by: Maksymilian Odziemczyk
# Created on: 26.11.2020

library(ggplot2)
library(entropy)
library(flexmix)
rm(list = ls())

random.walker.p1 <- function(M_steps){
  left <- 0
  right <-0
  S <- 0
  A <- c(left,right)
  trace<-0
  for (i in 1:M_steps){
     if(runif(1)>=0.5){
       trace <- c(trace, trace[i]+1)
       left <- left+1
     }
     else{
       trace<-c(trace, trace[i]-1)
       right<-right+1
     }
    S<-c(S, apply(cbind(left, right), 1, entropy))
    A<-rbind(A,c(left, right))
  }
  walker.p1.df <- data.frame(n = 0:M_steps, trace = trace, entrop = S)
  return (walker.p1.df)
}
random.walker.p5 <- function(M_steps){
  left <- 0
  right <-0
  S <- 0
  A <- c(left,right)
  trace5 <-0
  for (i in 1:M_steps){
    if(runif(1)>=1/6){
       trace5 <- c(trace5, trace5[i]+1)
       left <- left+1
     }
     else{
       trace5 <-c(trace5, trace5[i]-1)
       right <-right+1
     }
    S <- c(S, apply(cbind(left, right), 1, entropy))
    A <- rbind(A,c(left, right))
  }
  walker.p5.df <- data.frame(n = 0:M_steps, trace = trace5, entrop = S)
  return (walker.p5.df)
}

myEntropy <- function (p_minus, p_plus){
  return(-(p_minus * log(p_minus) + p_plus *log(p_plus)))
}

M_steps <- 500
t1 <- random.walker.p1(M_steps)
t5 <- random.walker.p5(M_steps)
S <- data.frame(dane = rep(myEntropy(1/6, 5/6), 500))
S2 <- data.frame(dane2 = rep(myEntropy(1/2, 1/2), 500))

g <- ggplot()
trace1 <- geom_line(data = t1, aes(x = 0:M_steps, y = trace, colour="1/1"), size=1)
trace5 <- geom_line(data = t5, aes(x = 0:M_steps, y = trace, colour="5/6"), size=1)
scale <- scale_fill_manual("Legenda", breaks = c("p 1/2", "p 5/6"), values = c("#F8766D", "#00BFC4"))
plt <- g + trace1 + trace5 + scale
ggsave("293654_trace.png")

g2 <- ggplot()
ent1 <- geom_line(data = t1, aes(x = 0:M_steps, y = entrop, colour="1/2"), size=1)
ent5 <- geom_line(data = t5, aes(x = 0:M_steps, y = entrop, colour="5/6"), size=1)
ent_eqn <- geom_line(data = S, aes(x = 1:M_steps, y = dane, colour = "blue"), linetype = "dashed")
ent_eqn2 <- geom_line(data = S2, aes(x = 1:M_steps, y = dane2, colour = "#00BFC4"), linetype = "dashed")
scale2 <- scale_fill_manual("Legenda", breaks = c("p 1/2", "p 5/6"), values = c("#F8766D", "#00BFC4"))
plt2 <- g2 + ent1 + ent5 + ent_eqn + ent_eqn2 + scale2
ggsave("293654_entropy.png")
rm(list = ls())