library(tseries)
library(zoo)
data <- read.csv("ww4.csv")
data$X <- index(data)
ts1 <- ts(data$x)

plot(ts1)
acf(ts1, lag.max = 50)
pacf(ts1)

ts2 <- HoltWinters(ts1, seasonal = c("additive"), start.periods = 2)
ts1<- zoo(x=data$x, order.by = strptime(data$X,format = c("%Y-%m-%d")))
