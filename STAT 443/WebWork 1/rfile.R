# Question 1
install.packages("zoo")
library(zoo)
x <- read.csv("file2396e0bc64e.csv")
y <- ts(x$Seattle, start= c(2004,1) , frequency = 4)
plot(y)
z <-rollmean(y, 4)
z

a <- read.csv("file23921dea654.csv")
b <- ts(a$Seattle, start = c(2008,1), frequency = 4)
plot(b)
u <- ts.union(y,b)
plot(u)

# Question 2
q2dat <- read.csv("2.csv")
head(q2dat, 33)
tail(q2dat)
q2ser <- ts(q2dat$Value)
q2ser
acf(q2ser, lag.max = 30, plot = F)
q2ser
plot(q2dat$Value, type="l")

q2b <- diff(q2ser)
acf(q2b, lag.max = 30, plot = F)

q2c <- diff(q2b)
acf(q2c, lag.max = 30, plot = F)

0.161  