library(testthat)
library(eventstudies)
context("example")

## Testing the functionality of the package, "eventstudies", using
## the example from the book "Event studies for financial research"
## by Doron Kliger and Gregory Gurevich (Chapter 7).

## Reading data for stock prices
stock.data <- read.csv("test_data_exampleStock.csv",
                       header = TRUE, skip = 0,
                       stringsAsFactors = FALSE)[, c(2:3, 12:14,
                         18:139)]
colnames(stock.data) <- c("EventDate", "Company", "Good", "Met",
                          "Bad", paste0("P(", -31:90, ")"))
stock.data$EventDate = as.Date(as.character(stock.data$EventDate),
                            "%B %d, %Y")
## Reading data for S&P index levels
index.data <- read.csv("test_data_exampleIndex.csv",
                       header = TRUE, skip = 0,
                       stringsAsFactors = FALSE)[, c(2:3, 12:14,
                         18:139)]
colnames(index.data) <- c("EventDate", "Company", "Good", "Met",
                          "Bad", paste0("M(", -31:90, ")"))
index.data$EventDate <- as.Date(as.character(index.data$EventDate),
                            "%B %d, %Y")

## Converting into zoo object
stock.prices <- t(stock.data[,-c(1,3:5)])
colnames(stock.prices) <- stock.prices[1,]
rownames(stock.prices) <- NULL
stock.prices <- stock.prices[-1,]
mode(stock.prices) <- "numeric"
# price to returns conversion
stock.prices <- apply(stock.prices, 2, function(x) diff(log(x)))

index.prices <- t(index.data[,-c(1,3:5)])
colnames(index.prices) <- index.prices[1,]
rownames(index.prices) <- NULL
index.prices <- index.prices[-1,]
mode(index.prices) <- "numeric"
# price to returns conversion
index.prices <- apply(index.prices, 2, function(x) diff(log(x)))

# zoo objects
test.firm.returns <- zoo(stock.prices, -30:90)
test.market.returns <- zoo(index.prices, -30:90)


##################### Testing the models in the package###############

### Stage I: AR estimation
#excessReturn model: defined as naive benchmark model in test example 
test.er.result <- excessReturn(firm.returns = test.firm.returns,
                               market.returns = test.market.returns)

### Stage II: Data sorting
naive.result <- t(test.er.result)

#Good
arGood <- stock.data[which(stock.data$Good == 1),]
arGood <- t(naive.result[which(rownames(naive.result) %in% arGood$Company),])

#Met
arMet <- stock.data[which(stock.data$Met == 1),]
arMet <- t(naive.result[which(rownames(naive.result) %in% arMet$Company),])

#Bad
arBad <- stock.data[which(stock.data$Bad == 1),]
arBad <- t(naive.result[which(rownames(naive.result) %in% arBad$Company),])


### Stage III: CAR Estimation

#Good
carGood <- remap.cumsum(arGood, is.pc = FALSE, base = 0)

#Met
carMet <- remap.cumsum(arMet, is.pc = FALSE, base = 0)

#Bad
carBad <- remap.cumsum(arBad, is.pc = FALSE, base = 0)


### Stage IV: Averaging CARs to obtain CAARs

#Good
caarGood <- rowMeans(carGood)

#Met
caarMet <- rowMeans(carMet)

#Bad
caarBad <- rowMeans(carBad)

### Stage V: Calculating descriptive statistics
