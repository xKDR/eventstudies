marketModel <- function(firm.returns, market.returns, residuals = TRUE) {
    returns <- merge(firm.returns, market.returns, all = FALSE, fill = NA)

    if (NCOL(market.returns) == 1) {
        market.returns.name <- "market.returns"
    } else {
        market.returns.name <- colnames(market.returns)
    }
    firms.name <- colnames(returns)[-match(market.returns.name, colnames(returns))]

                                        # Single firm
    if (NCOL(returns[, firms.name]) == 1) {
        reg <- lm(returns[, firms.name] ~ returns[, market.returns.name],
                  na.action = na.exclude) # :DOC: na.exclude: NAs can
                                          # be seen in prediction

        if (residuals == TRUE) {
            resid <- returns[, firms.name] - predict(reg)
            result <- resid
        } else {
            result <- reg
        }
                                        # Multiple firms
    } else 
        reg <- list()
        resids <- list()

    ## :DOC: we don't push the whole data.frame into lm() because it
    ## does na.omit, thereby removing rows from some firms even if
    ## they don't have NAs in them.
    for (i in 1:length(firms.name)) {
        reg[[i]] <- lm(returns[, firms.name[i]] ~ returns[, market.returns.name],
                       na.action = na.exclude)

        if (residuals == TRUE) {
            resids[[i]] <- returns[, firms.name[i]] - predict(reg[[i]])
        }
    }
    names(reg) <- firms.name

    if (residuals == TRUE) {
        names(resids) <- firms.name
        resids <- do.call("merge", resids)
        result <- resids
    } else {
        result <- reg
    }

    return(result)
}
