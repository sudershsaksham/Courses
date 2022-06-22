## Galton Board Simulation
galton_sim <- function(no_row_of_pegs = 5, no_runs = 100){
  cells <- as.data.frame(matrix(0, nrow = 2, ncol = (no_row_of_pegs+1)))
  row.names(cells) <- c("Cell Number","No. of marbles in the cell")
  for(i in 1:(no_runs)){
      marble_run <- sample((0:1), size = no_row_of_pegs, replace = TRUE)
      cells[1, (sum(marble_run)+1)] <- sum(marble_run)
      cells[2, (sum(marble_run)+1)] <- cells[2, (sum(marble_run)+1)] + 1
  }
  cells
}  

