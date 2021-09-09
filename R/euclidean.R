#' Euclidean Algorithm
#' @param num1 The first number to find the greatest common divisor from
#' @param num2 The first number to find the greatest common divisor from
#' @description The algorithm finds the greatest common divisor of two given numbers
#' @return returns the greatest common divisor of the two input numbers
#' @references https://en.wikipedia.org/wiki/Euclidean_algorithm

euclidean <-
function(num1, num2){
  if(!(is.numeric(num1) & is.numeric(num2) & length(num1) == 1 & length(num2) == 1)){
    stop('both arguments need to be numbers')
  }
  else{
    if(num1 == num2){
      # print(num1)
      return(num1)
    }
    else{
      numMax <- max(num1, num2)
      numMin <- min(num1, num2)
      newNum <- numMax - numMin
      res <- euclidean(newNum, numMin)
      return(res)
    }
  }
}
