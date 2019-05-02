#' @title bin_choose()
#' @description calculates the number of combinations in which k successes can occur in n trials
#' @param k number of trials 
#' @param n number of trials
#' @return an integer vector of the number of combinations
#' @seealso 
#' @export
#' @examples 
#' 
#' 
bin_choose <- function(n, k){
  check_success(k, n)
  check_trials(n)
  
  for(i in 1:length(k)){
  if(k[i] > n){
    stop("k cannot be greater than n")
  }
  }
    combos = factorial(n) / (factorial(k) * factorial(n - k))
    combos
  
}



# private checker function to ensure that probability, p, is valid i.e. between 1 and 0
check_prob <- function(p){
  if(p < 0 | p > 1){
    stop("Invalid probability value, p has to be a number between 0 and 1")
  }
  else{
    return(TRUE)
  }
}

# private checker function to ensure that the value of trials is a non-negative integer
check_trials <- function(trials){
  if(trials %% 1 != 0 | trials < 0){
    stop("Invalid trials value, trials must be non-negative interger value")  
  }
  else{
    return(TRUE)
  }
}

# private checker function to ensure that successes are non-negative integer value that is less than or equal to trials
check_success <- function(success, trials){
  for(i in 1:length(success)){
    if(success[i] %% 1 != 0 | success[i] > trials){
      stop("Invalid success value, success cannot be greater than trials")
    }
  }
  return(TRUE)
}
