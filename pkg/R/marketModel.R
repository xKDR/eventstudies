marketModel <- function(firm.returns, market.returns) {
    returns <- merge(firm.returns, market.returns, all = FALSE, fill = NA)
    market.returns <- returns$market.returns
    returns <- returns[, -match("market.returns", colnames(returns))]
    reg <- lm(returns ~ market.returns, na.action = na.exclude)
    return(reg)
}
