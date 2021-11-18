library(dplyr)

convolucion <- function(df1, df2, porder = FALSE){
  L1 = length(df1[,1])
  L2 = length(df2[,1])
  c1 = c()
  c2 = c()
  
  for (i in 1:L1){
    for (j in 1:L2){
      c1 <- append(c1, df1[i,1] + df2[j,1])
      c2 <- append(c2, df1[i,2] * df2[j,2])
    }
  }
  dfc <- data.frame(c1, c2)
  
  return(aggregate(c2 ~ c1, dfc, sum))
}

df1 <- data.frame(c(0,50,100,150),
                  c(0.3, 0.25, 0.35, 0.1))
df2 <- data.frame(c(0,20,50,80, 100),
                  c(0.2, 0.1, 0.15, 0.3, 0.25))
df3 <- data.frame(c(0, 200),
                  c(0.75, 0.25))

c <- convolucion(convolucion(df1, df2), df3)
c