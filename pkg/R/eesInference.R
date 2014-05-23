
# Total 16 functions
############################
# Identifying extreme events
############################

#----------------------------------------------------------------
# INPUT:
# 'input'     : Output of get.clusters.formatted
#-----------------------------------------------------------------
# OUTPUT:
# Result will be in a list of 3 with following tables:
# 1. Summary statistics
#    a. Summary of whole data-set
# 2. Lower tail: Extreme event tables
#    a. Distribution of extreme events
#    b. Run length distribution
#    c. Quantile values
#    d. Yearly distribution
#    e. Extreme event data
#     - Clustered, Un-clustered and Both
# 3. Upper tail: Extreme event tables
#    a. Distribution of extreme events
#    b. Run length distribution
#    c. Quantile values
#    d. Yearly distribution
#    e. Extreme event data
#     - Clustered, Un-clustered and Both
#------------------------------------------------------------------
# NOTE:
## Functions: get.clusters.formatted, eesSummary, eesDates, eesInference, plot.ees
eesSummary <- function(input){
  no.var <- NCOL(input)

  #---------------------
  # Extreme event output
  #---------------------
  # Summary statistics
  summ.st <- attr(input,"sumstat")
  colnames(summ.st) <- NULL
  summ.st <- t(summ.st)

  # Distribtution of events
  event.dist <- attr(input,"extreme.events.distribution")

  # Run length distribution
  runlength <- runlength.dist(input)

  # Quantile extreme values 
  qnt.values <- quantile.extreme.values(input)
  
  # Yearly distribution of extreme event dates
  yearly.exevent <- yearly.exevent.dist(input)

  #---------------------
  # Compiling the output
  #---------------------
  output <- lower.tail <- upper.tail <- list()
  # Compiling lower tail and upper tail separately
  # Lower tail
  lower.tail$extreme.event.distribution <- event.dist$lower.tail
  lower.tail$runlength <- runlength$lower.tail
  lower.tail$quantile.values <- qnt.values$lower.tail
  lower.tail$yearly.extreme.event <- round(t(yearly.exevent$lower.tail),2)
  # Upper tail
  upper.tail$extreme.event.distribution <- event.dist$upper.tail
  upper.tail$runlength <- runlength$upper.tail
  upper.tail$quantile.values <- qnt.values$upper.tail
  upper.tail$yearly.extreme.event <- round(t(yearly.exevent$upper.tail),2)
  # Output
  output$data.summary <- summ.st
  output$lower.tail <- lower.tail
  output$upper.tail <- upper.tail
  return(output)
}

########################################
# Functions used for formatting clusters
########################################
#------------------------
# Categorzing tail events
# for ES analysis
#------------------------
# Generates returns for the series
# Mark left tail, right tail events
gen.data <- function(d,probvalue,value="nonreturns"){
  res <- data.frame(dates=index(d),value=coredata(d))
  if(value=="returns"){
    res$returns <- c(NA,coredata(diff(log(d))*100))
  }else{
    res$returns <- d
  }
  pval <- c(probvalue/100,(1-(probvalue/100)))
  pval <- quantile(res$returns,prob=pval,na.rm=TRUE)
  res$left.tail <- as.numeric(res$returns < pval[1])
  res$right.tail <- as.numeric(res$returns > pval[2])
  res$both.tails <- res$left.tail + res$right.tail
  res <- res[complete.cases(res),]
  if(value=="returns"){
    return(res[-1,])
  }else{
    return(res)
  }
}


#-------------------
# Summarise patterns
summarise.rle <- function(oneseries){
  tp <- rle(oneseries)
  tp1 <- data.frame(tp$lengths,tp$values)
  tp1 <- subset(tp1,tp1[,2]==1)
  summary(tp1[,1])
}

# Summarise the pattern of cluster
summarise.cluster <- function(obj){
  rle.both <- summarise.rle(obj$both.tail)
  rle.left <- summarise.rle(obj$left.tail)
  rle.right <- summarise.rle(obj$right.tail)
  rbind(both=rle.both,left=rle.left,right=rle.right)
}
  
# Getting location for the length
exact.pattern.location <- function(us,pt,pt.len){
  st <- rle(us)
  len <- st$length
  loc.cs <- cumsum(st$length)
  loc <- loc.cs[which(st$values==pt & st$length==pt.len)]-pt.len+1
  return(loc)
}

# Identify and mark mixed clusters
identify.mixedclusters <- function(m,j){
  m$remove.mixed <- 0
  rownum <- which(m$pattern==TRUE)
  for(i in 1:length(rownum)){
    nextnum <- rownum[i]+j-1
    twonums <- m$returns[c(rownum[i]:nextnum)] > 0
    if(sum(twonums)==j || sum(twonums)==0){
        next
      }else{
        m$remove.mixed[c(rownum[i]:nextnum)] <- 5
      }
  }
  m
}

#--------------------
# Formatting clusters
#--------------------
# This function takes does the following transformation:
#----------------------------------------------------
# What the function does?
# i.   Get extreme events from event.series
# ii.  Remove all the mixed clusters
# iii. Get different types cluster
# iv.  Further club the clusters for event series and
#      corresponding response series to get
#      clustered returns
# v.   Throw the output in timeseries format
#----------------------------------------------------
# Input for the function
#    event.series = Series in levels or returns on events
#                   is to be defined
# response.series = Series in levels or returns on which
#                   response is to be generated
#      prob.value = Tail value for defining an event
#    event.value  = What value is to be studied
#                   returns or levels
# Similarly for response.value
#----------------------------------------------------
# Output = Formatted clusters in time series format
#----------------------------------------------------
get.clusters.formatted <- function(event.series,
                                   response.series,
                                   probvalue=5,
                                   event.value="nonreturns",
                                   response.value="nonreturns"){
   # Getting levels in event format
  tmp <- gen.data(event.series,
                  probvalue=probvalue,
                  value=event.value)
  res.ser <- gen.data(response.series,
                      probvalue=probvalue,
                      value=response.value)
  # Storing old data points
  tmp.old <- tmp

  # Get pattern with maximum length
  res <- summarise.cluster(tmp)
  max.len <- max(res[,"Max."])

  #------------------------
  # Removing mixed clusters
  #------------------------
  for(i in max.len:2){
    which.pattern <- rep(1,i)
    patrn <- exact.pattern.location(tmp$both.tails,1,i)
    # If pattern does not exist move to next pattern
    if(length(patrn)==0){next}
    tmp$pattern <- FALSE
    tmp$pattern[patrn] <- TRUE
    tmp <- identify.mixedclusters(m=tmp,i)
    me <- length(which(tmp$remove.mixed==5))
    
    if(me!=0){
      tmp <- tmp[-which(tmp$remove.mixed==5),]
      cat("Pattern of:",i,";",
          "Discarded event:",me/i,"\n")
    }
  }
  tmp.nc <- tmp

  # Merging event and response series
  tmp.es <- xts(tmp[,-1],as.Date(tmp$dates))
  tmp.rs <- xts(res.ser[,-1],as.Date(res.ser$dates))
  tmp.m <- merge(tmp.es,res.ser=tmp.rs[,c("value","returns")],
                 all=F)
  
  # Formatting 
  if(event.value=="returns"){
    which.value <- event.value
  }else{
    which.value <- "value"
  }
  # Converting to data.frame
  temp <- as.data.frame(tmp.m)
  temp$dates <- rownames(temp)
  n <- temp
  # Get pattern with maximum length
  res <- summarise.cluster(temp)
  max.len <- max(res[,"Max."])
  cat("Maximum length after removing mixed clusters is",
      max.len,"\n")
  # Marking clusters
  n$cluster.pattern <- n$both.tails
  for(pt.len in max.len:1){
    mark <- exact.pattern.location(n$both.tails,1,pt.len)
    if(length(mark)==0){next}
    n$cluster.pattern[mark] <- pt.len
  }
  
  #-------------------
  # Clustering returns
  #-------------------
  print("Clustering events.")
  for(pt.len in max.len:2){
    rownum <- exact.pattern.location(n$both.tails,1,pt.len)
    # If pattern does not exist
    if(length(rownum)==0){
      cat("Pattern",pt.len,"does not exist.","\n");next
    }
    # Clustering
    while(length(rownum)>0){
      prevnum <- rownum[1]-1
      lastnum <- rownum[1]+pt.len-1
    # Clustering event series
      if(event.value=="returns"){
        newreturns <- (n$value[lastnum]-n$value[prevnum])*100/n$value[prevnum]
        n[rownum[1],c("value","returns")] <-  c(n$value[lastnum],newreturns)
      }else{
        newreturns <- sum(n$value[rownum[1]:lastnum],na.rm=T)
        n[rownum[1],c("value","returns")] <-  c(n$value[lastnum],newreturns)
      }
    # Clustering response series
      if(response.value=="returns"){
        newreturns.rs <- (n$value.1[lastnum]-n$value.1[prevnum])*100/n$value.1[prevnum]
        n[rownum[1],c("value.1","returns.1")] <-  c(n$value.1[lastnum],newreturns.rs)
      }else{
        newreturns <- sum(n$value.1[rownum[1]:lastnum],na.rm=T)
        n[rownum[1],c("value.1","returns.1")] <-  c(n$value.1[lastnum],newreturns)
      }
      n <- n[-c((rownum[1]+1):lastnum),]
      rownum <- exact.pattern.location(n$both.tails,1,pt.len)
    }
  }
  # Columns to keep
  cn <- c(which.value,"left.tail","right.tail",
          "returns.1","cluster.pattern")
  tmp.ts <- zoo(n[,cn],order.by=as.Date(n$dates))
  colnames(tmp.ts) <- c("event.series","left.tail","right.tail",
                        "response.series","cluster.pattern")

  # Results
  attr(tmp.ts, which = "sumstat") <- sumstat(input = event.series)
  attr(tmp.ts, which = "extreme.events.distribution") <- extreme.events.distribution(input = event.series, gcf.output = tmp.ts, prob.value = probvalue)
  attr(tmp.ts, which = "probvalue") <- probvalue
  class(tmp.ts) <- c("ees","zoo")
  return(tmp.ts)
}

##############################
# Summary statistics functions
##############################
#---------------------------------------------
# Table 1: Summary statistics
# INPUT: Time series data-set for which
#        summary statistics is to be estimated
# OUTPUT: A data frame with:
# - Values: "Minimum", 5%,"25%","Median",
#           "Mean","75%","95%","Maximum",
#           "Standard deviation","IQR",
#           "Observations"
#----------------------------------------------
sumstat <- function(input){
  no.var <- NCOL(input)
  if(no.var==1){input <- xts(input)}
  # Creating empty frame: chassis
  tmp <- data.frame(matrix(NA,nrow=11,ncol=NCOL(input)))
  colnames(tmp) <-  "summary" 
  rownames(tmp) <- c("Min","5%","25%","Median","Mean","75%","95%",
                         "Max","sd","IQR","Obs.")
  # Estimating summary statistics
  tmp[1,] <- apply(input,2,function(x){min(x,na.rm=TRUE)})
  tmp[2,] <- apply(input,2,function(x){quantile(x,0.05,na.rm=TRUE)})
  tmp[3,] <- apply(input,2,function(x){quantile(x,0.25,na.rm=TRUE)})
  tmp[4,] <- apply(input,2,function(x){median(x,na.rm=TRUE)})
  tmp[5,] <- apply(input,2,function(x){mean(x,na.rm=TRUE)})
  tmp[6,] <- apply(input,2,function(x){quantile(x,0.75,na.rm=TRUE)})
  tmp[7,] <- apply(input,2,function(x){quantile(x,0.95,na.rm=TRUE)})
  tmp[8,] <- apply(input,2,function(x){max(x,na.rm=TRUE)})
  tmp[9,] <- apply(input,2,function(x){sd(x,na.rm=TRUE)})
  tmp[10,] <- apply(input,2,function(x){IQR(x,na.rm=TRUE)})
  tmp[11,] <- apply(input,2,function(x){NROW(x)})
  tmp <- round(tmp,2)

  return(tmp)
}

#############################
# Getting event segregation
# - clustered and unclustered
#############################
#----------------------------
# INPUT:
# 'input': Data series for which event cluster distribution
#        is to be calculated;
# Note: The input series expects the input to be in levels not in returns,
#       if the some the inputs are already in return formats one has to
#       use the other variable 'already.return.series'
# 'already.return.series': column name is to be given which already has
#       return series in the data-set
# 'prob.value': Probility value for which tail is to be constructed this
#       value is equivalent to one side tail for eg. if prob.value=5
#       then we have values of 5% tail on both sides
# Functions used: get.event.count()
# OUTPUT:
# Distribution of extreme events
#----------------------------

extreme.events.distribution <- function(input, gcf.output, prob.value){
  # Creating an empty frame
  no.var <- NCOL(input)
  lower.tail.dist <- data.frame(matrix(NA,nrow=no.var,ncol=6))
  upper.tail.dist <- data.frame(matrix(NA,nrow=no.var,ncol=6))
  colnames(lower.tail.dist) <- c("Unclustered","Used clusters",
                                 "Removed clusters","Total clusters",
                                 "Total","Total used clusters")
  rownames(lower.tail.dist) <- colnames(input)
  colnames(upper.tail.dist) <- c("Unclustered","Used clusters",
                                 "Removed clusters","Total clusters",
                                 "Total","Total used clusters")
  rownames(upper.tail.dist) <- colnames(input)
  # Estimating cluster count
  #--------------
  # Cluster count
  #--------------
  # Non-returns (if it is already in return format)
  tmp <- get.event.count(input, gcf.output, probvalue=prob.value,
                         value="nonreturns")
  lower.tail.dist  <- tmp[1,]
  upper.tail.dist  <- tmp[2,]

  #-----------------------------
  # Naming the tail distribution
  #-----------------------------
  mylist <- list(lower.tail.dist,upper.tail.dist)
  names(mylist) <- c("lower.tail", "upper.tail")
  return(mylist)
}

# Functions used in event count calculation
get.event.count <- function(series,
                            probvalue=5,
                            gcf.output,
                            value="returns"){
  # Extracting dataset
  tmp.old <- gen.data(series,probvalue,value)
  cp <- gcf.output[,"cluster.pattern"]
  lvl <- as.numeric(levels(as.factor(cp)))
  lvl.use <- lvl[which(lvl>1)]
  # Calculating Total events
  tot.ev.l <- length(which(tmp.old[,"left.tail"]==1))
  tot.ev.r <- length(which(tmp.old[,"right.tail"]==1))
  # Calculating Unclustered events
  un.clstr.l <- length(which(gcf.output[,"left.tail"]==1 &
                             gcf.output[,"cluster.pattern"]==1))
  un.clstr.r <- length(which(gcf.output[,"right.tail"]==1 &
                             gcf.output[,"cluster.pattern"]==1))
  # Calculating Used clusters
  us.cl.l <- us.cl.r <- NULL
  for(i in 1:length(lvl.use)){
    tmp1 <- length(which(gcf.output[,"cluster.pattern"]==lvl.use[i] &
                         gcf.output[,"left.tail"]==1))*lvl.use[i]
    tmp2 <- length(which(gcf.output[,"cluster.pattern"]==lvl.use[i] &
                         gcf.output[,"right.tail"]==1))*lvl.use[i]
    us.cl.l <- sum(us.cl.l,tmp1,na.rm=TRUE)
    us.cl.r <- sum(us.cl.r,tmp2,na.rm=TRUE)
  }

  # Making a table
  tb <- data.frame(matrix(NA,2,6))
  colnames(tb) <- c("unclustered.events","used.clustered.events","removed.clustered.events","total.clustered.events","total.events","total.used.events")
  rownames(tb) <- c("lower","upper")
  tb[,"total.events"] <- c(tot.ev.l,tot.ev.r)
  tb[,"unclustered.events"] <- c(un.clstr.l,un.clstr.r)
  tb[,"used.clustered.events"] <- c(us.cl.l,us.cl.r)
  tb[,"total.used.events"] <- tb$unclustered.events+tb$used.clustered.events
  tb[,"total.clustered.events"] <- tb$total.events-tb$unclustered.events
  tb[,"removed.clustered.events"] <- tb$total.clustered.events-tb$used.clustered.events

  return(tb)
}

######################
# Yearly summary stats
######################
#----------------------------
# INPUT:
# 'input': Output from get.clusters.formatted function
# 'prob.value': Probility value for which tail is to be constructed this
#       value is equivalent to one side tail for eg. if prob.value=5
#       then we have values of 5% tail on both sides
# Functions used: yearly.exevent.summary()
# OUTPUT:
# Yearly distribution of extreme events
#----------------------------
yearly.exevent.dist <- function(input){
  mylist <- list()
  ## Estimating cluster count  
  tmp.res <- yearly.exevent.summary(input)
  tmp.res[is.na(tmp.res)] <- 0
  ## Left and right tail
  lower.tail.yearly.exevent <- tmp.res[,1:2]
  upper.tail.yearly.exevent <- tmp.res[,3:4]
  output <- list()
  output$lower.tail <- lower.tail.yearly.exevent
  output$upper.tail <- upper.tail.yearly.exevent
  mylist <- output
  return(mylist)
}

#------------------------------------------------
# Get yearly no. and median for good and bad days
#------------------------------------------------
yearly.exevent.summary <- function(tmp){
  tmp.bad <- tmp[which(tmp[,"left.tail"]==1),]
  tmp.good <- tmp[which(tmp[,"right.tail"]==1),]
  # Bad days
  tmp.bad.y <- apply.yearly(xts(tmp.bad),function(x)nrow(x))
  tmp.bad.y <- merge(tmp.bad.y,apply.yearly(xts(tmp.bad[,1]),function(x)median(x,na.rm=T)))
  index(tmp.bad.y) <- as.yearmon(as.Date(substr(index(tmp.bad.y),1,4),"%Y"))
  # Good days
  tmp.good.y <- apply.yearly(xts(tmp.good),function(x)nrow(x))
  tmp.good.y <- merge(tmp.good.y,apply.yearly(xts(tmp.good[,1]),function(x)median(x,na.rm=T)))
    index(tmp.good.y) <- as.yearmon(as.Date(substr(index(tmp.good.y),1,4),"%Y"))
  tmp.res <- merge(tmp.bad.y,tmp.good.y)
  colnames(tmp.res) <- c("total.events.l","median.value.l",
                         "total.events.u","median.value.u")
  output <- as.data.frame(tmp.res)
  cn <- rownames(output)
  rownames(output) <- sapply(rownames(output),
                             function(x)substr(x,nchar(x)-3,nchar(x)))
  return(output)
}

####################################
# Quantile values for extreme events
####################################
#-----------------------------------
# INPUT:
# 'input': Output of get.clusters.formatted
# Note: The input series expects the input to be in levels not in returns,
#       if the some the inputs are already in return formats one has to
#       use the other variable 'already.return.series'
# 'already.return.series': column name is to be given which already has
#       return series in the data-set
# Functions used: get.clusters.formatted()
# OUTPUT:
# Lower tail and Upper tail quantile values
#-----------------------------------
quantile.extreme.values <- function(input){
  # Creating an empty frame
  lower.tail.qnt.value <- data.frame(matrix(NA,nrow=1,ncol=6))
  upper.tail.qnt.value <- data.frame(matrix(NA,nrow=1,ncol=6))
  colnames(lower.tail.qnt.value) <- c("Min","25%","Median","75%","Max",
                                      "Mean")
  rownames(lower.tail.qnt.value) <- "extreme.events"
  colnames(upper.tail.qnt.value) <- c("Min","25%","Median","75%","Max",
                                      "Mean")
  rownames(upper.tail.qnt.value) <- "extreme.events"
  # Estimating cluster count
  # Left tail
  tmp.left.tail <- input[which(input$left.tail==1),
                       "event.series"]
  df.left <- t(data.frame(quantile(tmp.left.tail,c(0,0.25,0.5,0.75,1))))
  tmp.left <- round(cbind(df.left,mean(tmp.left.tail)),2)
  rownames(tmp.left) <- "extreme.events"
  colnames(tmp.left) <- c("0%","25%","Median","75%","100%","Mean")
  # Right tail
  tmp.right.tail <- input[which(input$right.tail==1),
                        "event.series"]
  df.right <- t(data.frame(quantile(tmp.right.tail,c(0,0.25,0.5,0.75,1))))
  tmp.right <- round(cbind(df.right,
                           mean(tmp.right.tail)),2)
  rownames(tmp.right) <- "extreme.events"
  colnames(tmp.right) <- c("0%","25%","Median","75%","100%","Mean")
  
  lower.tail.qnt.value  <- tmp.left 
  upper.tail.qnt.value  <- tmp.right

  mylist <- list(lower.tail.qnt.value,upper.tail.qnt.value)
  names(mylist) <- c("lower.tail", "upper.tail")
  return(mylist)
}

##########################
# Run length distribution
##########################
#-----------------------------------
# INPUT:
# 'input': Data series in time series format
# Note: The input series expects the input to be in levels not in returns,
#       if the some the inputs are already in return formats one has to
#       use the other variable 'already.return.series'
# 'already.return.series': column name is to be given which already has
#       return series in the data-set
# Functions used: get.clusters.formatted()
#                 get.cluster.distribution()
#                 numbers2words()
# OUTPUT:
# Lower tail and Upper tail Run length distribution
#-----------------------------------
runlength.dist <- function(input){

  # Finding maximum Run length
  # Seed value
  max.runlength <- 0 
  #---------------------------
  # Estimating max. Run length
  #---------------------------
  tmp.runlength <- get.cluster.distribution(input,"event.series")
  max.runlength <- max(max.runlength,as.numeric(colnames(tmp.runlength)[NCOL(tmp.runlength)]))
  
  # Generating empty frame
  col.names <- seq(2:max.runlength)+1
  lower.tail.runlength <- data.frame(matrix(NA,nrow=1,
                                            ncol=length(col.names)))
  upper.tail.runlength <- data.frame(matrix(NA,nrow=1,
                                            ncol=length(col.names)))
  colnames(lower.tail.runlength) <- col.names
  rownames(lower.tail.runlength) <- "clustered.events"
  colnames(upper.tail.runlength) <- col.names
  rownames(upper.tail.runlength) <- "clustered.events"

  #----------------------
  # Run length estimation
  #----------------------
  tmp.res <- get.cluster.distribution(input,"event.series")
  for(j in 1:length(colnames(tmp.res))){
    col.number <- colnames(tmp.res)[j]
    lower.tail.runlength[1,col.number] <- tmp.res[1,col.number]
    upper.tail.runlength[1,col.number] <- tmp.res[2,col.number]
  }
  
  # Replacing NA's with zeroes
  lower.tail.runlength[is.na(lower.tail.runlength)] <- 0
  upper.tail.runlength[is.na(upper.tail.runlength)] <- 0

  # creating column names
  word.cn <- NULL
  for(i in 1:length(col.names)){
    word.cn[i] <- numbers2words(col.names[i])
  }
  colnames(lower.tail.runlength) <- word.cn
  colnames(upper.tail.runlength) <- word.cn
  mylist <- list(lower.tail.runlength,upper.tail.runlength)
  names(mylist) <- c("lower.tail", "upper.tail")
  return(mylist) 
}

#-------------------------
# Get cluster distribution
#-------------------------
# Input for this function is the output of get.cluster.formatted
get.cluster.distribution <- function(tmp,variable){
  # Extract cluster category 
  cp <- tmp[,"cluster.pattern"]
  lvl <- as.numeric(levels(as.factor(cp)))
  lvl.use <- lvl[which(lvl>1)]
  # Get numbers for each category
  tb <- data.frame(matrix(NA,2,length(lvl.use)))
  colnames(tb) <- as.character(lvl.use)
  rownames(tb) <- c(paste(variable,":lower tail"),
                    paste(variable,":upper tail"))
  for(i in 1:length(lvl.use)){
    tb[1,i] <- length(which(tmp[,"cluster.pattern"]==lvl.use[i]
                            & tmp[,"left.tail"]==1))
    tb[2,i] <- length(which(tmp[,"cluster.pattern"]==lvl.use[i]
                            & tmp[,"right.tail"]==1))
    
  }
  return(tb)
}

#----------------------------
# Converting numbers to words
#----------------------------
numbers2words <- function(x){
  helper <- function(x){
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
      else trim(paste(tens[digits[2]],
                      Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred",
               Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
                                          nDigits:(3*nSuffix + 1)])),
                 suffixes[nSuffix],
                 Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    gsub("^\ ", "", gsub("\ *$", "", text))
  }
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))
  opts <- options(scipen=100)
  on.exit(options(opts))
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine")
  names(ones) <- 0:9
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy",
            "eighty",
            "ninety")
  names(tens) <- 2:9
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")
  if (length(x) > 1) return(sapply(x, helper))
  helper(x)
}

##########################
# Extreme event study plot
##########################
# This function generates event study plot for clustered and un-clustered data
#-------------------------
# Input for the function
#                  z   = Data object with both the series response.series and event.series
# response.series.name = Column name of the series for which response is observed
#    event.series.name = Column name of the series on which event is observed
#          titlestring = Title string for the event study plot
#                 ylab = Y - axis label
#                width = width of event window for event study plot
#           prob.value = Probability value for which extreme events is determined
#-------------------------

######################
## Extreme event dates
######################
## Input:  get.clusters.formatted (GCF) output
## Output: Extreme Event dates for normal and purged data
eesDates <- function(input){
  ##-----------------
  ## Get event dates
  ##-----------------
  ## Get only unclustered data
  data.only.cluster <- input[which(input$cluster.pattern==1),]
  data.no.cluster <- input[which(input$cluster.pattern!=0),]

  ## get dates for bigdays and baddays
  days.bad.normal <- index(data.only.cluster[which(data.only.cluster[,"left.tail"]==1)])
  days.good.normal <- index(data.only.cluster[which(data.only.cluster[,"right.tail"]==1)])
  days.bad.purged <- index(data.no.cluster[which(data.no.cluster[,"left.tail"]==1)])
  days.good.purged <- index(data.no.cluster[which(data.no.cluster[,"right.tail"]==1)])
  ## Event list
  events.good.normal <- data.frame(name=rep("response.series",
                                     length(days.good.normal)),
                                   when=days.good.normal)
  events.bad.normal <- data.frame(name=rep("response.series",
                                     length(days.bad.normal)),
                                   when=days.bad.normal)
  events.good.purged <- data.frame(name=rep("response.series",
                                     length(days.good.purged)),
                                   when=days.good.purged)
  events.bad.purged <- data.frame(name=rep("response.series",
                                     length(days.bad.purged)),
                                   when=days.bad.purged)
  dates <- list(events.good.normal=events.good.normal,
                events.bad.normal=events.bad.normal,
                events.good.purged=events.good.purged,
                events.bad.purged=events.bad.purged)
  for(i in 1:length(dates)){dates[[i]][,1] <- as.character(dates[[i]][,1])}
  return(dates)
}

##----------------------
## Getting ees inference
##----------------------
## Event study plot for EES (extreme event studies)
## Input: Output of GCF
## event.lists: Output of eesDates
eesInference <- function(input, event.lists, event.window, to.remap=TRUE, 
                         remap="cumsum", inference = TRUE,
                         inference.strategy = "bootstrap"){
                         
  inf <- list()
  ## Computing inference
  ## Normal
                                        # Good days
  inf$good.normal <- eventstudy(input, event.list=event.lists$events.good.normal,
                                type="None", to.remap=to.remap,
                                remap=remap, event.window=event.window, inference=inference,
                                inference.strategy=inference.strategy)
                                        # Bad days
  inf$bad.normal <- eventstudy(input, event.list=event.lists$events.bad.normal,
                                type="None", to.remap=to.remap,
                                remap=remap, event.window=event.window, inference=inference,
                                inference.strategy=inference.strategy)
  ## Purged
                                          # Good days
  inf$good.purged <- eventstudy(input, event.list=event.lists$events.good.purged,
                                type="None", to.remap=to.remap,
                                remap=remap, event.window=event.window, inference=inference,
                                inference.strategy=inference.strategy)
                                            # Bad days
  inf$bad.purged <- eventstudy(input, event.list=event.lists$events.bad.purged,
                                type="None", to.remap=to.remap,
                                remap=remap, event.window=event.window, inference=inference,
                                inference.strategy=inference.strategy)

  class(inf) <- "ees"
  return(inf)
}

plot.ees <- function(x, xlab = NULL, ...){
  ## assign own labels if they're missing
  if (is.null(xlab)) {
      xlab <- "Event time"
  }
  ## Inference
  es.good.normal <- x$good.normal$eventstudy.output
  es.bad.normal <- x$bad.normal$eventstudy.output
  es.good.purged <- x$good.purged$eventstudy.output
  es.bad.purged <- x$bad.purged$eventstudy.output
                                        # Width
  width <- (NROW(x[[1]]$eventstudy.output)-1)/2
  ##---------------
  ## Plotting graph
  ##---------------
  big.normal <- max(abs(cbind(es.good.normal,es.bad.normal)))
  big.purged <- max(abs(cbind(es.good.purged,es.bad.purged)))
  big <- max(big.normal,big.purged)
  ylim.max <- c(-big,big)

  # Plotting graph
  par(mfrow=c(1,2))
  
  # Plot very good days
  plot(-width:width, es.good.normal[,2], type="l", lwd=2,
       ylim=ylim.max, col="red", xlab=xlab,
       main="Very good days", ...)       
  lines(-width:width, es.good.purged[,2], lwd=2, lty=1,type="l", col="orange")
  points(-width:width, es.good.normal[,2], pch=19,col="red")
  points(-width:width, es.good.purged[,2], pch=25,col="orange")
  lines(-width:width, es.good.normal[,1], lwd=0.8, lty=2, col="red")
  lines(-width:width, es.good.normal[,3], lwd=0.8, lty=2, col="red")
  lines(-width:width, es.good.purged[,1], lwd=0.8, lty=4, col="orange")
  lines(-width:width, es.good.purged[,3], lwd=0.8, lty=4, col="orange")
  abline(h=0,v=0)
  points(-width:width, es.good.normal[,1], pch=19,col="red")
  points(-width:width, es.good.purged[,1],pch=25,col="orange")
  points(-width:width, es.good.normal[,3], pch=19,col="red")
  points(-width:width, es.good.purged[,3],pch=25,col="orange")
    
  legend("topleft",legend=c("Un-clustered","Clustered"),
         cex=0.7,pch=c(19,25),
         col=c("red","orange"),lty=c(1,1),bty="n")
    
  # Plot very bad days
  plot(-width:width, es.bad.normal[,2], type="l", lwd=2,
       ylim=ylim.max, col="red", xlab=xlab,
       main = "Very bad days",...)
  lines(-width:width, es.bad.purged[,2], lwd=2, lty=1,type="l", col="orange")
  points(-width:width, es.bad.normal[,2], pch=19,col="red")
  points(-width:width, es.bad.purged[,2], pch=25,col="orange")
  lines(-width:width, es.bad.normal[,1], lwd=0.8, lty=2, col="red")
  lines(-width:width, es.bad.normal[,3], lwd=0.8, lty=2, col="red")
  lines(-width:width, es.bad.purged[,1], lwd=0.8, lty=2, col="orange")
  lines(-width:width, es.bad.purged[,3], lwd=0.8, lty=2, col="orange")
  points(-width:width, es.bad.normal[,1], pch=19,col="red")
  points(-width:width, es.bad.purged[,1], pch=25,col="orange")
  points(-width:width, es.bad.normal[,3], pch=19,col="red")
  points(-width:width, es.bad.purged[,3], pch=25,col="orange")
    
  abline(h=0,v=0)
  legend("topleft",legend=c("Un-clustered","Clustered"),
         cex=0.7,pch=c(19,25),
         col=c("red","orange"),lty=c(1,1),bty="n")
}

#--------------------------
# Suppress the messages
deprintize<-function(f){
 return(function(...) {capture.output(w<-f(...));return(w);});
}
