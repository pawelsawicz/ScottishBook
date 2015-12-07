library(rgl);

zetaFunction <- function(n,s){
  z <- (1/(n^(s)));
}

zeta <- function(x,y)
{
  s<-complex(real=x, imaginary=y);
  result <- 0+0i;
  for(i in 1:100)
  {
    resultOfzeta <- zetaFunction(i, s);
    result <- result + resultOfzeta;
  }
  result;
}


plotZetaFunction <- function(){
  x <- seq(1, 30, 1);
  #x <- seq(-15, 15, 0.1);
  y <- seq(1, 30, 1);
  #s <- complex(real=x, imaginary=y);
  z <- outer(x,y,zeta);
  #z <- zeta(x,y); 
  #plot(y, abs(z), type='l');
  #plot(x,Re(z));
  #plot(y,Im(z));
  #par(bg = "white");
  #persp(x, y, Re(z), theta = 90, phi=10, expand = 0.3, col = "lightblue",
   #     ltheta = 120, shade = 0.75, ticktype = "detailed", border = NA, box = TRUE)
  #persp(x, y, Im(z), theta = 90, phi=10, expand = 0.3, col = "lightblue",
   #     ltheta = 120, shade = 0.75, ticktype = "detailed", border = NA, box = TRUE)
  #persp(x, y, Mod(z), theta = 90, phi=10, expand = 0.3, col = "lightblue",
    #    ltheta = 120, shade = 0.75, ticktype = "detailed", border = NA, box = TRUE)
  plot(y, Re(zeta(x,y)));
  plot(y, Im(zeta(x,y)));
  plot(y, Mod(zeta(x,y)));
  plot(x, Re(zeta(x,y)));
  plot(x, Im(zeta(x,y)));
  plot(x, Mod(zeta(x,y)));
  plot(zeta(x,y));
  persp3d(x, y , Re(z) , col = "lightblue", scale = FALSE,
          shade = 0.5, expand = 0.25);
  #plot3d(x, y, Im(z), col="red", size=3);
  #plot3d(x, y, Mod(z), col="red", size=3);
}
