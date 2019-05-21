# This functions removes the selected gap
# it interpolates the data using zoo::na.spline

remove_gap_interpolate <- function(x=NULL, from, to){
  
  # let's increase the range just a tiny bit
  # this helps with jumps that are not perfectly a step
  # might cause trouble 
  from = from - 2
  
  # fix to not subset a negaative number
  if(from < 0){
    from <- 0
  }
  # We can't subset greater than x length
  to = to + 2
  if(to > length(x)){
    to <- length(x)
  }
  
  if(is.null(x)) stop("must supply not NULL x")
  
  #TODO: check if things are there
  
  fixed_x <- x
  fixed_x[from:to] <- NA
  
  if(to == length(x)){
    # interpolate with the median
    fixed_x[from:to]<- median(x[(from-10):from])
    
  } else {
    # do spline
    fit_x <- zoo::na.spline(fixed_x)
    fixed_x[from:to]<- fit_x[from:to]

  }
  
  
  return(fixed_x)
  
}