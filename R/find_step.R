#' @title Find Step
#'
#' @description This function tries to use convolution to find steps in data. It returns candidates to remove in a vector.
#' @param x target numeric vector
#' @param tolerance integer, threshold distance for step detection after convolution
#' @param kernel_size integer, kernel size of the convolution window (by default 3).
#' @keywords convolution
#' @export find_step
#' @return vector of positions of `x` where the step is predicted.
#' @examples
#' x <- c(rep(1,20), rep(50, 20), rep(1, 20))
#' candidate_10 <- find_step(x, tolerance = 10)
#' plot(x, type='l')
#' points(candidate_10, x[candidate_10])
#' # If tolerance is increased, step is not detected.
#' candidate_50 <- find_step(x, tolerance = 50)


find_step <- function(x, tolerance, kernel_size = 3){

  # Kernel for convolution
  my_kernel = matrix(1, nrow = kernel_size, ncol = 1) / 3

  cx <- convolve(x, my_kernel, type="open")
  adj_cx <- abs(cx) - max(cx)

  # Let's check if the step is "up" or "down"
  # We remove the extremes
  # convolution does nasty things there
  diff_cx <- diff(cx[10:(length(cx)-10)])

  zones <- which(abs(diff_cx) > tolerance)

  changes <- unique(sign(diff_cx[zones]))



  # If changes is c(1, 1) >> Positive step
  if (identical(changes, c(1,-1))){
    message("Derivate goes positive to negative,\nPrediction is step-up")
    # Look for candidates
    candidates <- which(adj_cx > (median(adj_cx) + tolerance))

  # If changes is c(-1, 1) >> Negative step
  } else if(identical(changes, c(-1, 1))){
    message("Derivate goes negative to positive,\nPrediction is step-down")
    # Look for candidates
    # Mind the change of sign and operation
    candidates <- which(adj_cx < (median(adj_cx) - tolerance))

  } else {
    print(zones)
    message("Can't determine step.\nIf step is real, modify tolerance or don't use convolution.")
    return(NULL)
  }


  # We can make sure the candidates are consecutive
  # We split them if they are not consecutive
  candidate_list <- split(candidates,
                          cumsum(c(1, diff(candidates) != 1)))

  # We will get the longest list (we assume that would be the step)
  # TODO: alternatively, we can look for the closest to an x candidate
  candidates <- candidate_list[[which.max(sapply(candidate_list, length))]]
  return(candidates)
}

