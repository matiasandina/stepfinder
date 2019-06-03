#' @title Remove Gap Interpolate
#'
#' @name remove_gap_interpolate
#' @description This function removes values between two positions and interpolates using zoo::na.interpolate(). You need a vector of at least length 11.
#' @param x numeric vector.
#' @param from numeric, position on `x` from which to start removing values.
#' @param to numeric, position on `x` where the removal will end.
#' @keywords interpolate
#' @export remove_gap_interpolate
#' @return interpolated version of x
#' @examples
#' x <- c(1,1,1,1,1,1,1,3,3,3,3,3,3,1,1)
#' remove_gap_interpolate(x, 7, 13)

# This functions removes the selected gap
# it interpolates the data using zoo::na.spline

remove_gap_interpolate <- function(x=NULL, from, to){

  # Check proper things
  if(is.null(x)) stop("must supply not NULL x")
  if(length(x)<11) stop("For interpolation to make sense, x must be at least length 11")
  if(from < 2) stop("We need a previous baseline of length at least 2. Increase x length.")

  # let's increase the range just a tiny bit
  # this helps with jumps that are not perfectly a step
  # might cause trouble
  from = from - 2

  # calculate median_from in case to == length(x)
  if(from < 10){
    median_from <- 1
  } else{
    median_from <- from - 10
  }
  # We can't subset greater than x length
  to = to + 2
  if(to > length(x)){
    to <- length(x)
  }


  fixed_x <- x
  fixed_x[from:to] <- NA

  if(to == length(x)){
    # interpolate with the median
    fixed_x[from:to]<- median(x[median_from:from])

  } else {
    # do spline
    fit_x <- zoo::na.spline(fixed_x)
    fixed_x[from:to]<- fit_x[from:to]

  }


  return(fixed_x)

}
