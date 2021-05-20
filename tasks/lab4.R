# Title     : Laboratorium4
# Objective : Zadanie4
# Created by: Maksymilian Odziemczyk
# Created on: 05.11.2020

test.normalny <- function(data) {
  test <- shapiro.test(data)
  if (test$p.value < 0.05) {
    cat("Należy odrzucić hipotezę zerową o pochodzeniu danych z rozkładu Normalnego,\n")
    cat("gdyż wartość parametru p =", test$p.value, "jest mniejsza od 0.05.\n")
  }
  else {
    cat("Nie należy odrzucać hipotezy zerowej o pochodzeniu danych z rozkładu Normalnego.\n")
    cat("gdyż wartość parametru p =", test$p.value, "jest większa od 0.05.\n")
  }
}

test.poissona <- function(data) {
  h <- hist(data, plot = FALSE)
  test <- chisq.test(h$counts, p = dpois(h$mids, lambda = 4), rescale.p = TRUE)
  if (test$p.value < 0.19) {
    cat("\nNależy odrzucić hipotezę zerową o pochodzeniu danych z rozkładu Poissona,\n")
    cat("gdyż wartość parametru p =", test$p.value, "jest mniejsza od 0.19.\n")
  }
  else {
    cat("\nNie należy odrzucać hipotezy zerowej o pochodzeniu danych z rozkładu Poissona,\n")
    cat("gdyż wartość parametru p =", test$p.value, "jest większa od 0.19.\n")
  }
}

test.wykladniczy <- function(data) {
  x <- sort(data, decreasing = TRUE)
  p <- sort(rexp(data, 0.25), decreasing = TRUE)
  test <- ks.test(x, p)
  cat("\nWynik Testu KS")
  print(test)
}

myPlot <- function(data) {
  h <- hist(data, plot = FALSE)
  args <- seq(min(round(data)), max(round(data)+1), 0.1)
  plot(h$mids, h$density, xlab = "x", ylab = "p(x)", pch = 19)
  lines(args, dexp(args, 0.25), col = "blue", lwd = 3)
  legend("topright", legend = c("data", "fit"), col = c("black", "blue"), pch = c(19, NA), lty = c(NA, 1))
}

options(warn=-1)
data <- read.table("zad4.txt", col.names = "numbers", stringsAsFactors = FALSE)
data <- sort(data$numbers,decreasing = TRUE)
test.normalny(data)
test.poissona(data)
test.wykladniczy(data)
myPlot(data)