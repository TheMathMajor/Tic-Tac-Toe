
triples <- list(
  c(1,2,3),
  c(4,5,6),
  c(7,8,9),
  c(1,4,7),
  c(2,5,8),
  c(3,6,9),
  c(1,5,9),
  c(3,5,7)
)

check_winner <- function(board){
  for (p in c("x", "o")){
    for (i in 1:8){
      if(sum(board[triples[[i]]] == p, na.rm = TRUE) == 3){
        return(p)
      }
    }
  }
  if (sum(is.na(board)) == 0){
    return("draw")
  }
  return("none")
}

display <- function(board){
  for (i in 1:9){
    if(is.na(board[i])){
      board[i] <- i
    }
  }
  cat(" ", board[1], "|", board[2], "|", board[3], "\n", "---+---+---", "\n", "", board[4], "|", board[5], "|", board[6], "\n", "---+---+---", "\n", "", board[7], "|", board[8], "|", board[9], "\n")
}

update <- function(board, who, pos){
  if (!is.na(board[pos])){
    return(board)
  }
  board[pos] <- who
  return(board)
}

computer_turn <- function(board){
  if(sum(board == "x", na.rm = TRUE) > sum(board == "o", na.rm = TRUE)){
    if(is.na(board)[5]){
      return(5)
    } else{
      for (i in 1:8){
        if(sum(board[triples[[i]]] == "o", na.rm = TRUE) == 2){
          for (j in 1:3){
            if(is.na(board[triples[[i]]][j])){
              return(triples[[i]][j])
            }
          }
        }
        if(sum(board[triples[[i]]] == "x", na.rm = TRUE) == 2){
          for (j in 1:3){
            if(is.na(board[triples[[i]]][j])){
              return(triples[[i]][j])
            }
          }
        }
      }
    }
  } else {
    if(is.na(board)[5]){
      return(5)
    } else{
      for (i in 1:8){
        if(sum(board[triples[[i]]] == "x", na.rm = TRUE) == 2){
          for (j in 1:3){
            if(is.na(board[triples[[i]]][j])){
              return(triples[[i]][j])
            }
          }
        }
        if(sum(board[triples[[i]]] == "o", na.rm = TRUE) == 2){
          for (j in 1:3){
            if(is.na(board[triples[[i]]][j])){
              return(triples[[i]][j])
            }
          }
        }
      }
    }
  }
  for (i in c(1,3,7,9)){
    if(is.na(board[i])){
      return(i)
    }
  }
  for (i in c(2,4,6,8)){
    if(is.na(board[i])){
      return(i)
    }
  }
  return(NA)
}

play <- function(){
  players <- readline("How many players? Enter 1 or 2:\n")
  while (players != "1" & players != "2"){
    players <- readline("Don't be a dick, enter 1 or 2:\n")
  }
  if (players == "1"){
    board <- rep(NA, 9)
    turn <- readline("Do you want to go first or second?\n")
    while (turn != "1" & turn != "2"){
      turn <- readline("Invalid, ENTER 1 OR 2:\n")
    }
    display(board)
    while (check_winner(board) == "none"){
      if (turn == "1"){
        position <- as.numeric(readline("Where do you want to play?\n"))
        while(!position %in% 1:9 | !is.na(board[position])){
          position <- as.numeric(readline("Enter a valid move:\n"))
        }
        board <- update(board, "x", position)
        display(board)
        if (check_winner(board) != "none"){
          break
        }
        cpt <- computer_turn(board)
        board <- update(board, "o", cpt)
        cat("Computer has played. \n")
        display(board)
        if (check_winner(board) != "none"){
          break
        }
      } else {
        cpt <- computer_turn(board)
        board <- update(board, "x", cpt)
        cat("Computer has played. \n")
        display(board)
        if (check_winner(board) != "none"){
          break
        }
        position <- as.numeric(readline("Where do you want to play?\n"))
        while(!position %in% 1:9 | !is.na(board[position])){
          position <- as.numeric(readline("Enter a valid move:\n"))
        }
        board <- update(board, "o", position)
        display(board)
        if (check_winner(board) != "none"){
          break
        }
      }
    }
    if (check_winner(board) == "x"){
      cat("Player 1 (x) wins")
    }
    if (check_winner(board) == "o"){
      cat("Player 2 (o) wins")
    }
    if (check_winner(board) == "draw"){
      cat("Draw")
    }
  } else{
    board <- rep(NA, 9)
    display(board)
    while (check_winner(board) == "none"){
      position <- as.numeric(readline("Where does 'x' want to play?\n"))
      while(!position %in% 1:9 | !is.na(board[position])){
        position <- as.numeric(readline("Enter a valid move:\n"))
      }
      board <- update(board, "x", position)
      display(board)
      if (check_winner(board) != "none"){
        break
      }
      position <- as.numeric(readline("Where does 'o' want to play?\n"))
      while(!position %in% 1:9 | !is.na(board[position])){
        position <- as.numeric(readline("Enter a valid move:\n"))
      }
      board <- update(board, "o", position)
      display(board)
      if (check_winner(board) != "none"){
        break
      }
    }
    if (check_winner(board) == "x"){
      cat("Player 1 (x) wins")
    }
    if (check_winner(board) == "o"){
      cat("Player 2 (o) wins")
    }
    if (check_winner(board) == "draw"){
      cat("Draw")
    }
  }
}
