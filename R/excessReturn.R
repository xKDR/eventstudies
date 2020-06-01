excessReturn <- function(firm.returns, market.returns) {
  stopifnot(NROW(firm.returns) == NROW(market.returns))

  stopifnot("zoo" %in% class(firm.returns) || "xts" %in% class(firm.returns))

  stopifnot("zoo" %in% class(market.returns) || "xts" %in% class(market.returns))
  
   return(firm.returns - market.returns)
}
