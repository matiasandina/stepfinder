## Fix detection jumps
# Find peaks function comes from this repo
# https://github.com/stas-g/findPeaks
# I added a copy to this repo to source it without internet connection

library(rChoiceDialogs)
library(stringr)


fix_detection_jumps <- function(df,
                                v_thresh = 25,
                                tol=50,
                                use_convolution=TRUE,
                                manual_removal=FALSE){

  x <- df$x
  y <- df$y

  diff_x <- c(0, diff(x))
  diff_y <- c(0, diff(y))

  # Let's look for potentially bad detections

  maybe_bad_x <- which(abs(diff_x) > v_thresh)
  maybe_bad_y <- which(abs(diff_y) > v_thresh)

  # Let's consider both axis as possible sources
  maybe_bad <- unique(c(maybe_bad_x, maybe_bad_y))

  message(sprintf("We found %d possible candidates...",
                  length(maybe_bad)))

  # This is not a step, should be either fixed with smooth
  # Alternatively, it's a point that is a true detection
  # It might just happen to be
  if(length(maybe_bad) < 2) {
    message("No steps found, only one detection above threshold.\nNo data was changed")
    return(df)
  }

  if(manual_removal == FALSE){
    # Cluster detections in pairs
    x_cluster_list <- cluster_candidate_list(maybe_bad_x, diff_x, tol=tol)
    y_cluster_list <- cluster_candidate_list(maybe_bad_y, diff_y, tol=tol)

    # Inspect them manually
    if(length(x_cluster_list) > 0){
      message("Inspecting positions in x")
      x <- inspect_wrong_detections(x = x, diff_x = diff_x,
                                    maybe_bad = x_cluster_list,
                                    use_convolution = use_convolution)
    }
    if(length(y_cluster_list) > 0){
      message("Inspecting positions in y")
      y <- inspect_wrong_detections(y,
                                    diff_y,
                                    maybe_bad = y_cluster_list,
                                    use_convolution = use_convolution)

    }

  } else {
    message("Entering manual mode....")
    message("Analyze x")
    for(bad_x in maybe_bad_x){

        # Call manual function here
      }
    message("Analyze y")

    }


  message("Replacing values into original data.frame")

  # Overwrite into input df
  df$x <- x
  df$y <- y


  return(df)


}



