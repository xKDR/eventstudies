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
    if (length(fulldata) == 0) {
      warning("no common window found");
      return(NULL)
    }
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
    result <- lapply(firm.returns, function(i)
           {
             mm.residual(y=i,x=market.returns)
           })
    names(result) <- colnames(firm.returns)
    chk <- which(do.call("c",lapply(result,is.null))==TRUE)
    if(length(chk)!=0){
      result <- result[-chk]
    }
    result <- do.call("merge.zoo", result)
  } else {
    result <- mm.residual(y=firm.returns,x=market.returns)
  }
  return(result)
}

