###############
# Excess return
###############
# Argument:
## FIXME: data.object??
# 1. data.object: This is a time series object with firm return and market return
# 2. market.name: It is the market (index) column name in the data object
# Output:
# Value: Excess market return

excessReturn <- function(firm.returns, market.returns){
  ## Getting market return
  ma.ret <- firm.returns - market.returns
  return(ma.ret)
}
