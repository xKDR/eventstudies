excessReturn <- function(firm.returns, market.returns) {
  stopifnot(NROW(firm.returns) == NROW(market.returns))

  stopifnot(class(firm.returns)=="zoo" || class(firm.returns)=="xts")

  stopifnot(class(market.returns)=="zoo" || class(market.returns)=="xts")
  
   return(firm.returns - market.returns)
}
