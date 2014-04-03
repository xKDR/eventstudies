#########################
# Market model adjustment
#########################
## Argument:
## 1. firm.returns: Firm returns of which market residual is to computed
## 2. market.returns: Market Index returns 
## Output:
## Value: Market residual after extracting market returns from the firm return

marketResidual <- function(firm.returns, market.returns){
  mm.residual <- function(y,x){
    ## Identify start and end date
    startdate <- start(x)
    enddate <- end(x)
    
    fulldata <- merge(x,y,all=TRUE)
    fulldata <- window(fulldata,start=startdate,end=enddate)
    
    ## Storing NA observations
    non.na.loc <- complete.cases(fulldata)
    fulldata <- fulldata[complete.cases(fulldata),]
    colnames(fulldata) <- c("x","y")
    reg <- lm(y ~ x, data = fulldata)
    
    result <- rep(NA,length(non.na.loc))
    result[non.na.loc] <- reg$residuals
    result <- zoo(result,order.by=index(x))
    result
  }
  
  ## Checking
  if(NCOL(firm.returns)>1){
    result <- NULL
    for(i in 1:NCOL(firm.returns)){
      res <- mm.residual(y=firm.returns[,i],x=market.returns)
      result <- merge(result,res,all=TRUE)
    }
    colnames(result) <- colnames(firm.returns)
  } else {
    result <- mm.residual(y=firm.returns,x=market.returns)
  }
  return(result)
}

