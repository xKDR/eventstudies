
# is.pc and is.returns are TRUE
#    values are like "1" for 1%
# is.returns is true but not is.pc
#    values are like "0.01" for 1%
# is.returns is false                         in this case is.pc is ignored!
#    values are like 1.01 for 1%
remap.cumprod <- function(z, is.pc=TRUE, is.returns=TRUE, base=100) {
  z <- firstValueZero(z)
  for (i in 1:NCOL(z)) {
    tmp <- z[,i]
    if (is.returns) {
      if (is.pc) {
        tmp <- tmp/100
      }
      tmp <- 1+tmp
    }
    tmp[1] <- base
    if(NCOL(z)==1){
      z <- cumprod(tmp)
    } else {
      z[,i] <- cumprod(tmp)
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
