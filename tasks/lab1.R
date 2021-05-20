# Title     : Laboratorium0
# Objective : Zadanie1
# Created by: Maksymilian Odziemczyk
# Created on: 15.10.2020
cross.prod <- function(vectorX, vectorY){
  mat = rbind(vectorX, vectorY)
  ex = det(mat[c(1, 2), c(2, 3)])
  ey = det(mat[c(1, 2), c(1, 3)])
  ez = det(mat[c(1, 2), c(1, 2)])
  if (ey<0){
    if (ez<0){
      cat("Iloczyn wektorowy wektorow U i V wynosi:",ex,"ex +",abs(ey),"ey -",abs(ez),"ez","\n")
    }
    else
      cat("Iloczyn wektorowy wektorow U i V wynosi:",ex,"ex +",abs(ey),"ey -",abs(ez),"ez","\n")
  }
  else if (ey>0){
    if (ez<0){
      cat("Iloczyn wektorowy wektorow U i V wynosi:",ex,"ex -",abs(ey),"ey -",abs(ez),"ez","\n")
    }
    else
      cat("Iloczyn wektorowy wektorow U i V wynosi:",ex,"ex -",abs(ey),"ey +",abs(ez),"ez","\n")
  }
  else {
    cat("Iloczyn wektorowy wektorow U i V wynosi:",ex,"ex -",abs(ey),"ey +",abs(ez),"ez","\n")
  }
}
x = c(-3,4,1)
y = c(5,-2,0)
cross.prod(x,y)