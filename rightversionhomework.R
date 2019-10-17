
#print the computer's response to user
do_response <- function(bulls, attempts) {
  if(bulls == 4) {
    print(paste("You won in",attempts,"attempt(s)!"))
  } else {
    print(paste("You lost in", attempts, "attempt(s)!"))
  }
}

#number of cows
number_cows <- function(user_choice, computer_choice, bulls) {
  cows = sum(user_choice %in% computer_choice) - bulls
  return(cows)
}

#number of bulls
number_bulls <- function(user_choice, computer_choice) {
  bulls = sum(user_choice == computer_choice)
  return(bulls)
}

#calculate the number of bulls and cows, given a computer vector and user guess
number_bulls_cows <- function(user_choice, computer_choice) {
  bulls = number_bulls(user_choice, computer_choice)
  cows = number_cows(user_choice, computer_choice, bulls)
  return(c(bulls, cows))
}

#generate the computer vector
generate_computer_vector <- function() {
  computer_choice = sample(1:9,4)
  return(computer_choice)
}

#prompt the user to input a guess and tell the user how many guesses they remain
get_guess <- function(bulls, cows, attempts, total_attempts, wrong_attempts, computer_choice) {
  print(paste("Now you should input 4 digits for bulls & cows guess game, you just have", total_attempts,"chances!"))
  while (bulls != 4) {
    number_string = readline("please enter the four numbers with no duplicate digits or 0s: >")
    attempts <- attempts + 1
    if(nchar(number_string) == 4) {
      user_choice = as.integer(unlist(strsplit(number_string,"")[[1]]))
      numbers = number_bulls_cows(user_choice, computer_choice)
      bulls <- numbers[1]
      cows <- numbers[2]
      cat("\n",bulls," Bull(s) and ",cows, " Cow(s)\n")
      if(bulls == 4) {
        break
      }
    } else {
      wrongattemp = 0
      while(wrongattemp<3){
        user_choice <- get_guess(computer_choice, total_attempts, attempts)
        if(length(user_choice)>4){
          print("You have input too long!")
        }else if(length(user_choice)<4){
          print("You have input too short!")
        }else break
        wrongattemp = wrongattemp + 1
        attempts = attempts + 1
      }
      if(wrongattemp==3){
        print("You have consecutively input 3 times incorrect number, game over!")
        break
      }
    }
    if(attempts == total_attempts) {
      break
    }
    print(paste("You left",(total_attempts - attempts),"chance(s)!"))
  }
  do_response(bulls, attempts)
}

#guess the bulls and cows
bulls_and_cows <- function() {
  bulls = 0
  cows = 0
  attempts = 0
  total_attempts = 10
  computer_choice = generate_computer_vector()
  wrongattemp = 0
  get_guess(bulls, cows, attempts, total_attempts, wrongattemp, computer_choice)
}

bulls_and_cows()
