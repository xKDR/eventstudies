#########################
# Market model adjustment
#########################
## Argument:
## 1. firm.returns: Firm returns of which market residual is to computed
## 2. market.returns: Market Index returns 
## Output:
## Value: Market residual after extracting market returns from the firm return

marketResidual <- function(firm.returns, market.returns){
  ## Checking
  if(NCOL(firm.returns)>1){
    result <- mm.residual(firm.returns[,1], market.returns)
    for(i in 2:NCOL(firm.returns)){
      res <- mm.residual(firm.returns[,i], market.returns)
      result <- cbind(result,res)
    }
    colnames(result) <- colnames(firm.returns)
  } else {
    result <- mm.residual(firm.returns, market.returns)
  }
  result <- zoo(result)
  return(result)
}

mm.residual <- function(firm.returns, market.returns){
  ## Storing NA observations
    na.date <- firm.returns[which(complete.cases(firm.returns)==FALSE)]
    firm <- firm.returns
    market <- market.returns
    mm.data <- merge(firm,market,all=TRUE)
    colnames(mm.data) <- c("firm","market")
    reg <- lm(firm ~ market, data = mm.data)
    resid <- xts(reg$residuals,as.Date(attr(reg$residuals,"names")))
    suppressWarnings(tot.resid <- rbind(resid,
                                        xts(rep(NA,NROW(na.date)),
                                            index(na.date))))
    colnames(tot.resid) <- "firm.residual"
  return(tot.resid)

}
