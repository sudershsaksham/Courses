df1 <- data.frame(read.csv("490lab2.csv"))
df2 <- data.frame(read.csv("490lab2b.csv"))
summary(df2)
SummaryTable <- data.frame(row.names = )
SummaryTable <- data.frame(Mean = c(mean(df2$mktrf), mean(df2$smb), mean(df2$hml), mean(df2$rf)), StDev = c(sd(df2$mktrf), sd(df2$smb), sd(df2$hml), sd(df2$rf)))
rownames(SummaryTable) <- c("MKTRF", "SMB", "HML", "RF")
MeanOfPort <- data.frame(Lowest = c(mean(df1$s1b1_vwret), mean(df1$s2b1_vwret), mean(df1$s3b1_vwret), mean(df1$s4b1_vwret), mean(df1$s5b1_vwret)), 
                  Second = c(mean(df1$s1b2_vwret), mean(df1$s2b2_vwret), mean(df1$s3b2_vwret), mean(df1$s4b2_vwret), mean(df1$s5b2_vwret)),
                  Third = c(mean(df1$s1b3_vwret), mean(df1$s2b3_vwret), mean(df1$s3b3_vwret), mean(df1$s4b3_vwret), mean(df1$s5b3_vwret)),
                  Fourth = c(mean(df1$s1b4_vwret), mean(df1$s2b4_vwret), mean(df1$s3b4_vwret), mean(df1$s4b4_vwret), mean(df1$s5b4_vwret)),
                  Highest = c(mean(df1$s1b5_vwret), mean(df1$s2b5_vwret), mean(df1$s3b5_vwret), mean(df1$s4b5_vwret), mean(df1$s5b5_vwret)))
rownames(MeanOfPort) <- c("Smallest", "Second", "Third", "Fourth", "Largest")
STDOfPort <- data.frame(Lowest = c(sd(df1$s1b1_vwret), sd(df1$s2b1_vwret), sd(df1$s3b1_vwret), sd(df1$s4b1_vwret), sd(df1$s5b1_vwret)), 
                        Second = c(sd(df1$s1b2_vwret), sd(df1$s2b2_vwret), sd(df1$s3b2_vwret), sd(df1$s4b2_vwret), sd(df1$s5b2_vwret)),
                        Third = c(sd(df1$s1b3_vwret), sd(df1$s2b3_vwret), sd(df1$s3b3_vwret), sd(df1$s4b3_vwret), sd(df1$s5b3_vwret)),
                        Fourth = c(sd(df1$s1b4_vwret), sd(df1$s2b4_vwret), sd(df1$s3b4_vwret), sd(df1$s4b4_vwret), sd(df1$s5b4_vwret)),
                        Highest = c(sd(df1$s1b5_vwret), sd(df1$s2b5_vwret), sd(df1$s3b5_vwret), sd(df1$s4b5_vwret), sd(df1$s5b5_vwret)))
rownames(STDOfPort) <- c("Smallest", "Second", "Third", "Fourth", "Largest")
df3 <- data.frame(df1)
for (i in 2:26) {
  for (j in 1:684) {
    df3[j,i]=df1[j,i]-df2[j,5]
  }
}
df3[,27] = df2$mktrf
data.frame(row= c(1,2)) -> df4
for (k in 2:26) {
  capm.lm <- lm(formula = df3[,k] ~ df3 [,27], data = df3)
  x = k-1
  df4[,x] <- capm.lm$coefficients
  modsummary <- summary(capm.lm)
  modcoeff <- modsummary$coefficients
  df4[1,x] <- modcoeff[2,1]
  df4[2,x] <- modcoeff[2,3]
}
rownames(df4) <- c("beta", "t statistic")
df4 <- t(df4)

data.frame(row= c(1,2)) -> RegResultsEst
data.frame(row= c(1,2)) -> RegResultsT
x=0
for (l in 1:5) {
  for (m in 1:5) {
    x=x+1
    RegResultsEst[l,m] <- df4[x,1]
    RegResultsT[l,m] <- df4[x,2]
  }  
}
colnames(RegResultsEst) <- c("lowest", "2", "3", "4", "highest")
rownames(RegResultsEst) <- c("smallest", "2", "3", "4", "largest")
colnames(RegResultsT) <- c("lowest", "2", "3", "4", "highest")
rownames(RegResultsT) <- c("smallest", "2", "3", "4", "largest")

data.frame(row= c(1,2)) -> df5
for (n in 2:26) {
  x=n-1
  mean(df3[,n]) -> df5[,x]
}

for (o in 1:26) {
  df5[1,o] <- df4[o,1]
}

rownames(df5) <- c("beta", "MeanReturn")
df5 <- t(df5)

df5 <- data.frame(df5)
smlreg <- lm(formula = df5[,2] ~ df5[,1], data = df5)
plot(df5$beta, df5$MeanReturn)
abline(lm(df5$MeanReturn ~ df5$beta))

df3 <- cbind(df3, df2$smb, df2$hml)

df6 <- data.frame(row= c(1,2,3,4,5))
for (p in 2:26) {
  x=p-1
  ffmod <- lm(formula = df3[,p] ~ df3$`df2$smb` + df3$`df2$hml`)
  modsummary <- summary(ffmod)
  modcoeff <- modsummary$coefficients
  df6[1,x] <- modcoeff[2,1]
  df6[2,x] <- modcoeff[2,3]
  df6[3,x] <- modcoeff[3,1]
  df6[4,x] <- modcoeff[3,3]
  df6[5,x] <- modsummary$r.squared
}

df6 <- t(df6)
colnames(df6) <- c("S","TforS","H","TforH","R2")
rownames(df6) <- c("r1","r2","r3","r4","r5","r6","r7","r8","r9","r10","r11","r12","r13","r14","r15","r16","r17","r18","r19","r20","r21","r22","r23","r24","r25")

df7 <- data.frame(row= c(1,2,3,4,5,6,7))
for (p in 2:26) {
  x=p-1
  ffmod <- lm(formula = df3[,p] ~ df3$`df2$smb` + df3$`df2$hml` + df3$V27)
  modsummary <- summary(ffmod)
  modcoeff <- modsummary$coefficients
  df7[1,x] <- modcoeff[2,1]
  df7[2,x] <- modcoeff[2,3]
  df7[3,x] <- modcoeff[3,1]
  df7[4,x] <- modcoeff[3,3]
  df7[5,x] <- modcoeff[4,1]
  df7[6,x] <- modcoeff[4,3]
  df7[7,x] <- modsummary$r.squared
}

df7 <- t(df7)
colnames(df7) <- c("S","TforS","H","TforH","Beta","TforBeta","R2")
rownames(df7) <- c("r1","r2","r3","r4","r5","r6","r7","r8","r9","r10","r11","r12","r13","r14","r15","r16","r17","r18","r19","r20","r21","r22","r23","r24","r25")
