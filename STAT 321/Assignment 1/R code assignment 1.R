sim_dice <- function(i,j){
  number_sim <- 10000
  condition_1 = condition_2 <- c(0)
  for(k in 1:number_sim){
    sample_rolls <- sample(1:8, size = 20, replace = TRUE)
    i_check <- sum(sample_rolls[]>=6)
    j_check <- sum(sample_rolls[]>=4)
    if(i_check >= i){
      condition_1 = condition_1 + 1
    }
    if(j_check >= j){
      condition_2 = condition_2 + 1
    }
  }
  prob <- count1/count2
  
  return(prob)
}

output <- data.frame(row.names = c("j=8","j=9","j=10","j=11","j=12"))
for(m in 1:3){
  for(o in 1:5){
    output[o,m] <- die_roll(m+11,o+7)
  }
}
colnames(output) <- c("i=12","i=13","i=14")

output
