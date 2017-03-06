
# from analysis/02-dtfilter.Rmd -------------------------------------------------------------------------------------------------------------------------------------------

# "2017-03-06 12:42:41 PET"

### UNBIASED selection
#
#Unbiased selection criteria of Ab.units estimate among replicated reads of per sample:
#
#+ **NOTE:** This assumes that no dilution to the original evaluated sample has been performed.
#    + For triplicates, the closest value to the mean among replicates was selected
#    + Reads with mean.OD with a %CV higher than 20% were rejected
#    + For duplicates, the replicate with lower %CV was preferred.
#    + If a selection was not acomplished, the last read performed was preferred.
#
#This criteria have been implemented in a **function** called `Unbiased()`:

#### FUNCION PARA LA APLICACION DEL CRITERIO
Unbiased <- function(repliMAT) {
  # LOOP: closest value to the mean
  z <- NULL
  # for each ID with replicates
  for(i in 1:length(levels(repliMAT$ID))){
    # for each Specie
    for (j in 1:length(levels(repliMAT$Specie))) {
      # extrae Ab.unit y %CV para cada especie
      x <- repliMAT[repliMAT$ID==levels(repliMAT$ID)[i] & 
                      repliMAT$Specie==levels(repliMAT$Specie)[j], c(4,8)]
      if (any(x$cv.OD>=20, na.rm = TRUE)) {
        # lecturas con %CV>=20% seran descartadas (independiente de OD>0.25)
        x <- x[-which(x$cv.OD>=20),]
      }
      y <- mean(x$Ab.unit, na.rm = TRUE)
      # elije cual de los valores es igual a la menor diferencia con la media aritmetica
      a <- x[which(min(abs(x$Ab.unit - y), na.rm = TRUE)==abs(x$Ab.unit - y)),]
      
      if (dim(a)[1]>1) {
        # para 2 lecturas, elegir la que tenga menor %CV
        a <- a[which(a$cv.OD==min(a$cv.OD, na.rm = TRUE)),]$Ab.unit
        if (length(a)>1) {
          # si no hay diferencias por %CV, entonces optar por la ultima lectura
          a <- a[2] 
        }
      } else {
        # si la dimension es igual a 1, entonces extraer las Ab.units
        a <- a$Ab.unit
      }
      
      z <- c(z,a)
    }
  }
  # LOOP para asignar eleccion a una tabla
  w <- NULL
  for(i in 1:length(z)){
    # evita IDENTIDAD entre Ab.Units de distintas muestras
    # seleccion por bloques
    v <- repliMAT[repliMAT$ID==levels(repliMAT$ID)[ceiling(i/2)],] 
    w <- rbind(w,subset(v, Ab.unit==z[i])) 
  }
  # output
  return(w)
}
####