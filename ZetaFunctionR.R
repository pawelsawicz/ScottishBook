zetaFunction <- function(n,s){
  z <- (1/n^s);
}

plotZetaFunction <- function(endSeries, s){
  n<-seq(1, endSeries, 1);
  z<-zetaFunction(n,s);
  plot(n,z, type='l');
}

sumOfSeries <- function(endSeries, s)
{
  result <- 0;
  for(i in 1:endSeries)
  {
    resultOfzeta <- zetaFunction(i, s);
    result <- result + resultOfzeta;
  }
  result
}

result <- sumOfSeries(100,2);

print(result)
