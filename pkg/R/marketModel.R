marketModel <- function(firm.returns, market.returns,resid = TRUE) {
    returns <- merge(firm.returns, market.returns, all = FALSE, fill = NA)
    market.returns <- returns$market.returns
    returns <- returns[, -match("market.returns", colnames(returns))]
    if (NCOL(returns) == 1) {             # Output for a single firm
        reg <- lm(returns ~ market.returns, na.action = na.exclude) #:DOC: na.exclude
        if (resid == TRUE) { ## MM-residuals for a single firm
            resid <- returns - predict(reg)
            return(resid)
        } else { ## Model estimates for a single firm
            return(reg)
        }
    } else { ## Multi-firm case
        reg <- list()
        resids <- list()
        if (resid == TRUE) { ## Residuals for the multi-firm case
            for (i in 1:ncol(returns)) {
                reg[[i]] <- lm(returns[, i] ~ market.returns, na.action = na.exclude)
                resids[[i]] <- returns[, i] - predict(reg[[i]])
            }
            names(resids) <- colnames(returns)
            resid <- do.call("merge", resids)
            return(resid)
        } else { ## Model estimates for the multi-firm case
            for (i in 1:ncol(returns)) {
                reg[[i]] <- lm(returns[,i] ~ market.returns, na.action = na.exclude)    
            }
            names(reg) <- colnames(returns)
            return(reg)
        }
    }
}
