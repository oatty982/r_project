# -*- coding: utf-8 -*-

# -- Sheet --

pyc <- function() {
  cat("Game Start! \n")
  win <- 0
  lose <- 0
  draw <- 0
  turn <- 0
  hands <- c("rock", "paper", "scissors") 
  ##################################################
  while(T){
    bot_hands <-sample(1:3, size = 1)
    user_hands <- as.numeric(readline("Please select hand [1:rock, 2:paper, 3:scissors, 4:end]: "))
    
      if (user_hands == 4) {
        cat("!---- END ----!\n")
        cat("WIN:   " ,win, "\n")
        cat("LOSE:  " ,lose, "\n")
        cat("DRAW:  " ,draw, "\n")
        cat("Total: " ,turn, "\n")
        break
      } else if (user_hands == bot_hands) {
        turn <- turn + 1
        draw <- draw + 1
        cat("!---- Draw ----!\n")
      } else if (hands[user_hands] == "rock" & hands[bot_hands] == "scissors") {
        turn <- turn + 1
        win <- win + 1
        cat("Game: ", turn,"\n")
        cat("!---- WIN ----! \n")
      } else if (hands[user_hands] == "paper" & hands[bot_hands] == "rock") {
        turn <- turn + 1
        win <- win + 1
        cat("Game: ", turn,"\n")
        cat("!---- WIN ----! \n")
      } else if (hands[user_hands] == "scissors" & hands[bot_hands] == "paper") {
        turn <- turn + 1
        win <- win + 1
        cat("Game: ", turn,"\n")
        cat("!---- WIN ----! \n")
      } else {
        turn <- turn + 1
        lose <- lose + 1
        cat("!---- LOSER ----!\n")
      }
      cat("user_hands:  ", hands[user_hands],"\n")
      cat("bot_hand:    ", hands[bot_hands], "\n")
  }
};

pyc ()
