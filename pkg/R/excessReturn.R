###############
# Excess return
###############
# Argument:
# 1. data.object: This is a time series object with firm return and market return
# 2. market.name: It is the market (index) column name in the data object
# Output:
# Value: Excess market return

excessReturn <- function(data.object, market.name=NULL){
  if(is.null(market.name)==TRUE){
    stop("Column name for market index not provided")
  }
  cn.names <- colnames(data.object)
  cn.names <- cn.names[-which(cn.names%in%market.name)]
  ma.ret <- data.object[,cn.names]-data.object[,market.name]
  return(ma.ret)
}
