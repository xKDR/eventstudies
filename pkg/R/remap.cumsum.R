
# If is.pc then a value like "1" means 0.01
remap.cumsum <- function(z, is.pc=FALSE, base=0) {
  z <- firstValueZero(z)
  for (i in 1:NCOL(z)) {
    tmp <- z[,i]
    if (is.pc) {
      tmp <- tmp/100
    }
    if(NCOL(z)==1){
      z <- tmp
    } else {
      z[,i] <- base+cumsum(tmp)
    }
  }
  z
}

firstValueZero <- function(x){
  if(NCOL(x)==1){
    x[1] <- 0
  } else {
    x[1,] <- 0
  }
  return(x)
}
