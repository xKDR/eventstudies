#########################
# Market model adjustment
#########################
## Argument:
## 1. data.object: Single time series object with all the variables
## 2. market.name: Column name of market index in the data
## Output:
## Value: Market residual after extracting market returns from the firm return

marketResidual <- function(data.object, market.name=NULL){
### Checking arguments
  if(is.null(market.name)==TRUE){
    stop("Column name of market index not provided")
  }
  cn.names <- colnames(data.object)
  cn.names <- cn.names[-which(cn.names%in%market.name)]
  ## Market residual
  formula <- paste(cn.names[1],"~",market.name,sep=" ")
  tmp <- marketResidual.onefirm(mm.formula = formula,
                                data.object = data.object,
                                firm.name = cn.names[1])
  ## tmp <- tmp[complete.cases(tmp),]
  if(length(cn.names)>1){
    for(i in 2:length(cn.names)){
      ## Getting formula
      formula <- paste(cn.names[i],"~",market.name,sep=" ")
      ## Market residual
      tmp.resid <- marketResidual.onefirm(mm.formula = formula,
                                          data.object = data.object,
                                          firm.name = cn.names[i])
      ## tmp.resid <- tmp.resid[complete.cases(tmp.resid),]
      tmp <- merge(tmp,tmp.resid,all=TRUE)
    }
  }
  colnames(tmp) <- cn.names
  return(tmp)
}

marketResidual.onefirm <- function(mm.formula=NULL,data.object,firm.name){
### Market residual one firm
  ## Storing NA observations
  na.date <- data.object[which(complete.cases(data.object[,firm.name])==FALSE),
                         firm.name]
### Checking arguments
  if(is.null(mm.formula)==TRUE){
    print("Formula for market residual model not provided")
  }
  ## Extracting market residuals
  reg <- lm(as.formula(mm.formula),data=data.object)
  resid <- xts(reg$residuals,as.Date(attr(reg$residuals,"names")))
  suppressWarnings(tot.resid <- rbind(resid,
                                      xts(rep(NA,NROW(na.date)),
                                          index(na.date))))
  return(tot.resid)
}

