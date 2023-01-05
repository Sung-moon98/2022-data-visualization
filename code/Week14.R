install.packages("plot3D")
install.packages("rgl")
library(rgl)
library(plot3D)
library(tidyverse)
library(readxl) 


x1 <- c(1,1,1,1,1, 2,3,4, 5,5,5,5,5, 5,5,5,5,5, 4,3,2, 4,3,2, 1,2,3,4,5, 1,1, 2,3,4,5, 5,5, 4,3,2,1)
y1 <- c(1,1,1,1,1, 1,1,1, 1,1,1,1,1, 5,5,5,5,5, 5,5,5, 5,5,5, 3,3,3,3,3, 3,3, 3,3,3,3, 3,3, 3,3,3,3)
z1 <- c(1,2,3,4,5, 2,3,2, 1,2,3,4,5, 1,2,3,4,5, 3,4,5, 3,2,1, 1,1,1,1,1, 2,3, 3,3,3,3, 4,5, 5,5,5,5)
df <- data.frame(x1,y1,z1) 
plot3d(x = df$x1, y = df$y1, z = df$z1, col = rainbow(length(x1)),
       size = 3, type = "s", main = "KSM", xlab = "x", ylab = "y", zlab = "z")




par(mar = c(1, 1, 1, 1))
scatter3D(x = mtcars$drat, y = mtcars$mpg, z = mtcars$disp, pch = 15,
          main = "mtcars", xlab = "drat", ylab = "mile per galon", zlab = "disp",
          bty = "g", ticktype = "detailed", d = 3, theta = 50, phi = 30)




par(mar = c(1, 1, 1, 1))
scatter3D(x = mtcars$mpg, y = mtcars$hp, z = mtcars$gear, pch = 10,
          main = "mtcars", xlab = "mile per galon", ylab = "hp", zlab = "gear",
          bty = "b", ticktype = "detailed", d = 1, theta = 60, phi = 20)




