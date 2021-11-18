library(dplyr)

df1 <- data.frame(c(0,50,100,150),
                  c(0.3, 0.25, 0.35, 0.1))
df2 <- data.frame(c(0,20,50,80, 100),
                  c(0.2, 0.1, 0.15, 0.3, 0.25))
df3 <- data.frame(c(0, 200),
                  c(0.75, 0.25))

c <- convolucion(convolucion(df1, df2), df3)

tinv <- function(c){
  c <- c[order(-c[,2]),]
  c <- cbind(c, cumsum(c[,2]))
  
  L = length(c[,1])

  u = runif(1)
  for (i in 1:L){
    if (u < c[i,3]){return(c[i,1])}
  }
  
}


v <- replicate(10000, tinv(c))

summary(v)
table(v)