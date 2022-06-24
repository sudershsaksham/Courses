#loading in data for estimation
nkedataest <- data.frame(read.csv("nkeestwind.csv"))
gsdataest <- data.frame(read.csv("gsestwind.csv"))
vdataest <- data.frame(read.csv("vestwind.csv"))
ffdataest <- data.frame(read.csv("ffestwind.csv"))

#creating a dataframe together
masterdat <- cbind(nkedataest,vdataest,gsdataest,ffdataest)

#making regression models
nkelm <- lm(formula = masterdat[,4] ~ masterdat$mktrf + masterdat$smb + masterdat$hml)
vlm <- lm(formula = masterdat[,8] ~ masterdat$mktrf + masterdat$smb + masterdat$hml)
gslm <- lm(formula = masterdat[,12] ~ masterdat$mktrf + masterdat$smb + masterdat$hml)

#making a dataframe of coefficients 
summnke <- summary(nkelm)
summv <- summary(vlm)
summgs <- summary(gslm)
coeff <- data.frame(summnke$coefficients[,1])
coeff[,2] <- summv$coefficients[,1]
coeff[,3] <- summgs$coefficients[,1]

#making coeff easy to read
rownames(coeff) <- c("Intercept","MKTRF","SMB","HML")
colnames(coeff) <- c("NKE","V","GS")
coeff <- t(coeff)

#predicting for the event window
ffdataevent <- data.frame(read.csv("ffeventwind.csv"))
pred <- data.frame()
for (i in 1:11) {
  pred[i,1] <- coeff[1,1]+(ffdataevent[i,2]*coeff[1,2])+(ffdataevent[i,3]*coeff[1,3])+(ffdataevent[i,4]*coeff[1,4])
  pred[i,2] <- coeff[2,1]+(ffdataevent[i,2]*coeff[2,2])+(ffdataevent[i,3]*coeff[2,3])+(ffdataevent[i,4]*coeff[2,4])
  pred[i,3] <- coeff[3,1]+(ffdataevent[i,2]*coeff[3,2])+(ffdataevent[i,3]*coeff[3,3])+(ffdataevent[i,4]*coeff[3,4])
}

#label pred dataframe
pred <- cbind(pred,ffdataevent$date)
colnames(pred) <- c("NKE", "V", "GS", "date")

#making actual value dataframe for comparison
nkedataevt <- data.frame(read.csv("nkeeventwind.csv"))
gsdataevt <- data.frame(read.csv("gseventwind.csv"))
vdataevt <- data.frame(read.csv("veventwind.csv"))

#making a dataframe for comparison
comp <- data.frame(pred)
for(i in 1:11){
  comp[i,1] <- nkedataevt[i,4]-pred[i,1]
  comp[i,2] <- vdataevt[i,4]-pred[i,2]
  comp[i,3] <- gsdataevt[i,4]-pred[i,3]
}

#making cumulative columns
for(i in 1:11){
  comp[i,5] <- 
  comp[i,6] <- vdataevt[i,4]-pred[i,2]
  comp[i,7] <- gsdataevt[i,4]-pred[i,3]
}
