#' @title Fix Detection Jumps
#'
#' @name fix_detection_jumps
#' @description This function calculates derivatives, finds jumps above threshold and calls functions to recognize/fix the step. It can manage 3 ways to detect a step: derivatives + convolution, bare derivatives, and manual.
#' @param df data.frame containing `x` and `y` coordinates of subject `id`.
#' @param v_thresh numeric, threshold velocity above which a detection is considered wrong and marked for step detection.
#' @param tol numeric, to be used to pass to `cluster_candidate_list()`.
#' @param use_convolution logical, whether to use convolution to find step. If TRUE will call `find_step()`, else it will use bare derivatives and thresholding for the candidates.
#' @param manual_removal logical, whether to go to manual inspection and removal of the steps. See `manual_step_removal()`.
#' @keywords diagnostic
#' @export fix_detection_jumps
#' @return data.frame with the same input columns, positions may or may not be fixed depending on user input.
#' @examples
#' library(stepfinder)
#' set.seed(123)
#' df <- data.frame(id = "first_target", frameID=1:1000, x=rnorm(1000, 0, 5), y = rnorm(1000, 0, 5))
#' df$x[50:80] <- 200
#' df$y[c(100:150, 400:420)] <- 200
#' diagnose_detection(df)
#' fix_detection_jumps(df)
#' explore parameter values

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
      x <- manual_step_removal(x, point=bad_x,
                               candidates=maybe_bad_x)
    }
    message("Analyze y")
    for(bad_y in maybe_bad_y){
      # Call manual function here
      y <- manual_step_removal(y, point = bad_y,
                               candidates= maybe_bad_y)
    }
  }



  message("Replacing values into original data.frame")

  # Overwrite into input df
  df$x <- x
  df$y <- y


  return(df)


}



