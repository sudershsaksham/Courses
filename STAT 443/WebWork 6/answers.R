# q1
data <- read.csv("realize.csv")
spc<-spec.pgram(data$x, log = "no")
spc1<-spec.pgram(data$x, log = "no", spans = c(16))
abs(spc$spec[198]/pi - (3.01235*cos(spc$freq[198]*2*pi)+3.53306)/pi)
spc1$spec[198]/pi - (3.01235*cos(spc1$freq[198]*2*pi)+3.53306)/pi

# q2
data2 <- read.csv("lynx.csv")
plot(data2$X, log(data2$x), type = "l")

spc2 <- spec.pgram(log(data2$x), log = "no")
spc2$freq[which.max(spc2$spec)]

spc3 <- spec.pgram(log(data2$x), log = "no", spans = c(7,9))
spc3$freq[which.max(spc3$spec)]
