#' @title bin_cumulative
#' 
bin_cumulative <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  
  success = c(0:trials)
  probability = bin_probability(success, trials, prob)
  cumulative = c(rep(0, length(success)))
  
  for(i in 2:length(success)){
    cumulative[1] = probability[1]
    cumulative[i] = probability[i] + cumulative[i - 1]
    cumulative
  }
  
  cumul <- data.frame(
    success = success,
    probability = probability,
    cumulative = cumulative
  )
  class(cumul) <- c("bincum", "data.frame")
  cumul
}


#' @export
plot.bincum <- function(x, ...) {
  lines(x$success, x$cumulative, type = "o", xlab = "successes", ylab = "probability")
}
