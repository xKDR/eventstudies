marketModel <- function(firm.returns, market.returns, residuals = TRUE) {
    stopifnot(NROW(firm.returns) == NROW(market.returns)) #:DOC

                                        # Single firm
    if (NCOL(firm.returns) == 1) {
        merged.object <- merge.zoo(firm.returns, market.returns, all = TRUE) #:DOC
        reg <- lm(firm.returns ~ market.returns, data = merged.object,
                  na.action = na.exclude) # :DOC: na.exclude: NAs can
                                          # be seen in prediction

        if (residuals == TRUE) {
            resid <- firm.returns - predict(reg)
            result <- resid
        } else {
            result <- reg
        }
                                        # Multiple firms
    } else {
        reg <- list()
        resids <- list()

        ## :DOC: we don't push the whole data.frame into lm() because it
        ## does na.omit, thereby removing rows from some firms even if
        ## they don't have NAs in them.
        for (i in 1:NCOL(firm.returns)) {
            merged.object <- merge.zoo(firm.returns[, i], market.returns, all = TRUE)
            colnames(merged.object)[i] <- "firm.returns"
            reg[[i]] <- lm(firm.returns ~ market.returns, data = merged.object,
                           na.action = na.exclude)

            if (residuals == TRUE) {
                resids[[i]] <- firm.returns[, i] - predict(reg[[i]])
            }
        }
        names(reg) <- colnames(firm.returns)

        if (residuals == TRUE) {
            names(resids) <- colnames(firm.returns)
            resids <- do.call("merge", resids)
            result <- resids
        } else {
            result <- reg
        }
    }                                   # END multiple firms

    return(result)
}
