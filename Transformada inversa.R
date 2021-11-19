library(dplyr)

tinv <- function(c){
  c <- c[order(-c[,2]),]
  c <- cbind(c, cumsum(c[,2]))
  
  L = length(c[,1])

  u = runif(1)
  for (i in 1:L){
    if (u < c[i,3]){return(c[i,1])}
  }
  
}


df1 <- data.frame(c(0,1),
                  c(0.8, 0.2))
c <- conv(df1, df1)
# El dataframe c contiene las posibles combinaciones de siniestros dadas las dos carteras, junto a sus probabilidades.


set.seed(200)
n = 10000
v <- replicate(n, tinv(c))
# El vector binario v indica la presencia o no de un siniestro, simulado n veces.


df2 <- data.frame(c(50, 150, 250, 300),
                  c(0.03, 0.06, 0.09, 0.82))
df2 <- df2[order(-df2[,2]),]
df2 <- cbind(df2, cumsum(df2[,2]))
# df2 contiene los posibles costos de los siniestros junto a sus probabilidades.}
# Se ordena el dataframe de manera de presentar primero los eventos de mayor probabilidad puntual.


w = c()
# Declaro el vector w, que simulará el costo de un siniestro para cada valor no nulo del vector v.
# Nótese que el vector debe simular más de un costo en caso de haber más de un siniestro por período simulado.
for (i in 1:length(v)){
  if (v[i] == 0){w = append(w, 0)} else {
    w = append(w,
               sum(replicate(v[i], tinv(df2))))
  }
}

mean(w)   # Para 100:   97.5
          # Para 1000:  107.45
          # Para 10000: 110.495

sd(w)     # Para 100:   157.3735
          # Para 1000:  159.601
          # Para 10000: 160.7519

table(w)  # Frecuencia relativa en w
          #    0      50      150     200    250     300     350     400     450     500     550     600 
          # 0.6427  0.0094  0.0195  0.0001  0.0278  0.2627  0.0019  0.0006  0.0028  0.0004  0.0061  0.0260 


