#' @title bin_variable
#' 
#' 
bin_variable <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  
  number = c(0:trials)
  probability = bin_probability(number, trials, prob)
  
  variable <- list(
    trials = number,
    probability = probability,
    prob_success = prob,
    number_trials = trials
  )
  class(variable) = "binvar"
  variable
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


#' @export
print.binvar <- function(x, ...){
  cat('"Binomial variable" \n\n')
  cat("Parameters \n")
  cat("- number of trials:", x$number_trials)
  cat("\n- prob of success:", x$prob_success)
  invisible(x)
}

#' @export
summary.binvar <- function(x, ...){
  summary <- list(
    trials = x$number_trials,
    prob = x$prob_success,
    mean = aux_mean(x$number_trials, x$prob_success),
    variance = aux_variance(x$number_trials, x$prob_success),
    mode = aux_mode(x$number_trials, x$prob_success),
    skewness = aux_skewness(x$number_trials, x$prob_success),
    kurtosis = aux_kurtosis(x$number_trials, x$prob_success)
  )
  class(summary) = "summary.binvar"
  summary
  
}

#' @export
print.summary.binvar <- function(x, ...){
  cat('"Summary Binomial" \n\n')
  cat("Parameters \n")
  cat("- number of trials:", x$number_trials)
  cat("\n- prob of success:", x$prob_success)
  cat("\n\nMeasures")
  cat("\n- mean:", x$mean)
  cat("\n- variance:", x$variance)
  cat("\n- mode:", x$mode)
  cat("\n- skewness:", x$skewness)
  cat("\n- kurtosis:", x$kurtosis)
}

# private auxiliary function to find mean
aux_mean <- function(trials, prob){
  mean = trials*prob
  return(mean)
}

# private auxiliary function to find variance
aux_variance <- function(trials, prob){
  variance = trials * prob * (1 - prob)
  return(variance)
}

# private auxiliary function to find mode
aux_mode <- function(trials, prob){
  if(trials %%2 == 0 & trials >= 1 & prob >= 0 & prob <= 1){
    mode = floor((trials * prob) + prob)
    return(mode)
  }
  else if(trials %%2 != 0 & trials >= 1 & prob >= 0 & prob <= 1){
    m = floor((trials * prob) + prob)
    mode2 = c(m, m - 1)
    return(mode2)
  }
}

# private auxiliary function to find skewness
aux_skewness <- function(trials, prob){
  skewness <- (1 - 2*prob) / sqrt(trials*prob*(1 - prob))
  return(skewness)
}

# private auxiliary function to find kurtosis
aux_kurtosis <- function(trials, prob){
  kurtosis = (1 - 6*prob * (1 - prob)) / (trials * prob * (1 - prob))
  return(kurtosis)
}


