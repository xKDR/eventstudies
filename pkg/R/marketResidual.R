#########################
# Market model adjustment
#########################
# Argument:
# 1. mm.formula: Here the input is the linear model (lm) formula for eg: a ~ b + c
#                If formula is not given then first column will be dependent and                   rest will be independent
# 2. data.object: Single time series object with all the variables
# Output:
# Value: Market residual after extracting market returns from the firm return
marketResidual <- function(mm.formula=NULL,data.object){
  # Storing NA observations
  na.date <- data.object[which(complete.cases(data.object)==FALSE)]
  # Extracting market residuals
  if(is.null(mm.formula)==TRUE){
    formula <- paste(colnames(data.object)[1],"~",
                     colnames(data.object)[2:NCOL(data.object)],sep="")
    reg <- lm(as.formula(formula),data=data.object)
  }else{
    
    reg <- lm(as.formula(mm.formula),data=data.object)
  }
  resid <- xts(reg$residuals,as.Date(attr(reg$residuals,"names")))
  suppressWarnings(tot.resid <- rbind(resid,
                                      xts(rep(NA,nrow(na.date)),
                                          index(na.date))))
  return(tot.resid)
}
