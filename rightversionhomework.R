#generate the computer vector
generate_computer_vector <- function() {
  computer_choice = sample(1:9,4)
  return(computer_choice)
}
get_guess <- function(computer_choice, total_attempts, attempts){
  print(paste("You left",(total_attempts - attempts),"chance(s)!")) # how many guesses they have remaining
  number_string <- readline("please enter the four numbers with no duplicate digits or 0s: ")
  user_choice = as.integer(unlist(strsplit(number_string,"")[[1]]))
  return(user_choice)
}
#calculate the number of bulls and cows, given a computer vector and user guess
number_bulls_and_cows <- function(computer_choice, user_choice){
  bulls <- number_bulls(computer_choice, user_choice)
  cows <- number_cows(computer_choice, user_choice, bulls)
  return(c(bulls, cows))
}
#number of bulls
number_bulls <- function(computer_choice, user_choice){
  sum(user_choice == computer_choice)
}
#number of cows
number_cows <- function(computer_choice, user_choice, bulls){
  sum(user_choice %in% computer_choice) - bulls
}
#print the computer's response to user
do_response <- function(bulls, attempts){
  if(bulls==4){
    print(paste("You won in",attempts+1,"attempt(s)!"))
    return(TRUE)
  }
  return(FALSE)
}
#prompt the user to input a guess and tell the user how many guesses they remain
bulls_and_cows <- function(){
  computer_choice <- generate_computer_vector()
  bulls <- 0
  cows <- 0
  attempts <- 0
  total_attempts <- 10
  print(paste("Now you should input 4 digits for bulls & cows guess game, you just have", total_attempts,"chances!"))
  while (bulls != 4) 
  {
    error = 0
    while(error<3){
      user_choice <- get_guess(computer_choice, total_attempts, attempts)
      if(length(user_choice)>4){
        print("You have input too long!")
      }else if(length(user_choice)<4){
        print("You have input too short!")
      }else break
      error = error + 1
      attempts = attempts + 1
    }
    if(error==3){
      print("You have consecutively input 3 times incorrect number, game over!")
      break
    }
    bulls_and_cows = number_bulls_and_cows(computer_choice, user_choice)
    bulls = bulls_and_cows[1]
    cows = bulls_and_cows[2]
    cat("\n",bulls," Bull(s) and ",cows, " Cow(s)\n")
    is.correct = do_response(bulls, attempts)
    if(is.correct) break # answer correct
    # after 10 attempts
    if(attempts == total_attempts){
      print(paste("You lost in", attempts, "attempt(s)!"))
      break
    }
    attempts <- attempts + 1
  }
}
#guess the bulls and cows
bulls_and_cows()


