library('tidyverse')


z1 <- volcano        
x1 <- 1:nrow(z1)  
y1 <- 1:ncol(z1)  

persp(x1, y1, z1, theta = 50, phi = 120, expand = 0.5, col = "green",
      ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "Sinc( r )") -> res
title(main = "Volcano")
round(res, 3)




x <- seq(-10, 10, by=0.1)
y <- x
f <- function(x,y){ 1/(2*pi)*exp(-(x^2+y^2)/2) }
z <- outer(x,y,f)

persp(x,y,z)

persp(x,y,z, theta = 30, phi = 200, expand = 0.5, col = 'yellow', shade = 0.5, ltheta = 90)
title(main="정규분포 함수")
round(res, 3)




x2 <- seq(-40, 40, length = 30)
y2 <- x2
f <- function(x, y) { r <- sqrt(x ^ 2 + y ^ 2); 10 * tan(r) }
z2 <- outer(x2, y2, f)
z2[is.na(z2)] <- 1


persp(x2, y2, z2, theta =30, phi = 30, expand = 0.5, col = "lightblue",
      ltheta = 90, shade = 0.5, ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "Sinc( r )") -> res
title(main = 'Tan(r) 함수')
round(res, 3)



trans3d <- function(x, y, z, pmat) {
  tr <- cbind(x, y, z, 1) %*% pmat
  list(x = tr[,1] / tr[,4], y = tr[,2] / tr[,4])
}

phi <- seq(0, 2 * pi, len = 201)
r1 <- 8
xr <- r1 * cos(phi)
yr <- r1 * sin(phi)
lines(trans3d(xr, yr, f(xr, yr), res), col = "pink", lwd = 2)




x <- seq(-4, 4, by=0.1)
y <- x
f <- function(x,y){ 1/(2*pi)*exp(-(x^2+y^2)/2) }
z <- outer(x,y,f)

persp(x,y,z)

persp(x,y,z, theta = 30, phi = 30, expand = 0.5, col = 'lightblue', shade = 0.5, ltheta = 30)
title(main="정규분포 함수")
round(res, 3)   


trans3d <- function(x, y, z, pmat) {
  tr <- cbind(x, y, z, 1) %*% pmat
  list(x = tr[,1] / tr[,4], y = tr[,2] / tr[,4])
}

phi <- seq(0, 2 * pi, len = 201)
r1 <- 3
xr <- r1 * cos(phi)
yr <- r1 * sin(phi)
lines(trans3d(xr, yr, f(xr, yr), res), col = "pink", lwd = 2)


x <- seq(-4, 4, by=0.1)
y <- x
f <- function(x,y){ 1/(2*pi)*exp(-(x^2+y^2)/2) }
z <- outer(x,y,f)

persp(x,y,z)

persp(x,y,z, theta = 50, phi = 200, expand = 0.5, col = 'pink', shade = 0.5, ltheta = 30)
title(main="정규분포 함수")
round(res, 3)   






x2 <- seq(-5, 5, length = 30)
y2 <- x2
f <- function(x, y) { x^2 + y^2 }
z2 <- outer(x2, y2, f)
z2[is.na(z2)] <- 1


persp(x2, y2, z2, theta =90, phi = 30, expand = 0.5, col = "red",
      ltheta = 60, shade = 0.2, ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "Sinc( r )") -> res
my_exp=expression(x^2+y^2)
title(main = my_exp)
round(res, 3)



























      