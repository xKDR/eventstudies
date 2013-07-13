###############
# Excess return
###############
# Argument:
# 1. data.object: This is a time series object with firm return and market return
# 2. firm.name: It is the firm column name in the data object
# 3. market.name: It is the market (index) column name in the data object
# Output:
# Value: Excess market return
excessReturn <- function(firm.name,market.name,data.object){
  ma.ret <- data.object[,firm.name]-data.object[,market.name]
  return(ma.ret)
}
