rps <- function(k=1,p_a=c(1/3,1/3,1/3),p_b=c(1/3,1/3,1/3)){
  # arguments: - N is the number of trials (default N = 1)
  #            - p_a is the probability vector of player A for (R,P,S)
  #                 (default p_a = c(1/3,1/3,1/3))
  #            - p_b is the probability vector of player B fro (R,P,S)
  #                 (default p_b = c(1/3,1/3,1/3))
  
  # possible hands for each player and their probabilities
  player_A <- c("r","p","s")
  player_A_probs <- p_a
  player_B <- c("r","p","s")
  player_B_probs <- p_b
  
  # simulate the game
  results <- tibble(winner = character())
  for(i in 1:k){
    player_A_hand <- sample(player_A,1,p_a,replace=TRUE)
    player_B_hand <- sample(player_B,1,p_b,replace=TRUE)
    outcome <- switch(paste(player_A_hand,player_B_hand),
                      "r r" = "Tie",
                      "r p" = "B",
                      "r s" = "A",
                      "p r" = "A",
                      "p p" = "Tie",
                      "p s" = "B",
                      "s r" = "B",
                      "s p" = "A",
                      "s s" = "Tie")
    results <- results %>% add_row(winner=outcome)
  }
  
  # return outcomes
  return(results)
}