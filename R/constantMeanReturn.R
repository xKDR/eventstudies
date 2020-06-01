constantMeanReturn <- function(firm.returns, residuals = TRUE) {
    
    stopifnot("zoo" %in% class(firm.returns) || "xts" %in% class(firm.returns))
                                         # Single firm
    if (NCOL(firm.returns) == 1) {
                                        # Compute constant mean over
                                        # estimation period
        constantMean <- mean(firm.returns, na.rm = T)
        if (residuals == TRUE) {
            resid <- firm.returns - constantMean
            result <- resid
        } else {
            result <- constantMean
        }
        
    } else {
        constantMean <- list()
        resids <- list()
        for (i in 1:NCOL(firm.returns)) {
            constantMean[[i]] <- mean(firm.returns[ , i],
                                      na.rm = TRUE)
            if (residuals == TRUE) {
                resids[[i]] <- firm.returns[, i] - constantMean[[i]]
            }
        }
        if (residuals == TRUE) {
            names(resids) <- colnames(firm.returns)
            resids <- do.call("merge", resids)
            result <- resids
        } else {
            constantMean <- do.call(rbind, constantMean)
            result <- constantMean
        }
    }                                   # END multiple firms
    return(result)
}


