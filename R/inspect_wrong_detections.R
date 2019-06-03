#' @title Inspect Wrong Detections
#'
#' @name inspect_wrong_detections
#' @description This function is called from `fix_detection_jumps`. It handles correct subsetting, prompts user and calls of `remove_gap_interpolate`. This function does a lot of printing an plotting. Printing will be improved/reduced. Plotting might be moved to ggplot2 instead of base.
#' @param x numeric vector, passed through `fix_detection_jumps`
#' @param diff_x result of c(0, diff(x)), passed through `fix_detection_jumps`.
#' @param maybe_bad numeric vector, possible points to inspect, passed through `fix_detection_jumps`.
#' @param use_convolution logical, whether to use convolution to find step. If TRUE will call `find_step()`, else it will use bare derivatives and thresholding for the candidates.
#' @keywords diagnostic
#' @export inspect_wrong_detections
#' @return data.frame with the same input columns, positions may or may not be fixed depending on user input.
#' @examples


inspect_wrong_detections <- function(x, diff_x, maybe_bad,
                                     use_convolution=TRUE){

  for (candidate in maybe_bad){


    cand_low <- as.numeric(candidate[1,1])
    cand_high <- as.numeric(candidate[2,1])

    low <- max(1, cand_low - 50)
    high <- min(cand_high + 50, length(diff_x))

    if(is.na(low) | is.na(high)){
      print(low)
      print(high)
      message("Something went wrong with clustering candidates.\nSee below")
      print(candidate)
      message("No changes made to the data")
      return(x)
    }

    # Subset in position

    x_pos_region <- x[low:high]

    # Subset in velocity
    x_region <- diff_x[low:high]

    # find them in the new range
    new_x <- which(x_region == diff_x[cand_low])
    new_x2 <- which(x_region == diff_x[cand_high])

    invisible(readline(prompt="Press [enter] to see velocity plots: > "))

    # Open a new graphical window
    open_graphic_window()

    plot(x_region, type="l")
    # if matches exist, plot them in red
    if (length(new_x) > 0){
      points(new_x, x_region[new_x], col="red", pch=19)
    }

    # Give some time for the plot to show
    Sys.sleep(0.5)

    message("Diagnose detection:")
    ask <- readline(prompt=("Good Detection --> Keep or Bad Detection --> Remove?? [(1/2)]: >"))

    while(!ask %in% c(1,2)){
      message("Sorry, option not available. Choose 1 or 2.")
      ask <- readline(prompt=("Good Detection --> Keep or Bad Detection --> Remove?? [(1/2)]: >"))
    }


    #if(ask == "Bad Detection -- Remove"){
    if(ask == 2){

      # find steps around position using convolution
      if(use_convolution){
        message("Using convolution to find step.")
        x_to_remove <- find_step(x_pos_region, tolerance = 20)
      } else {
        message("No convolution.\nFinding steps from derivatives")
        # Try with the candidates directly
        # We need to use the candidates with their new axis
        x_to_remove <- c(new_x:new_x2)
      }

      message("Analyzing position close to bad detections")

      # interpolate
      if(length(x_to_remove) > 1){
        new_data_x <- remove_gap_interpolate(x_pos_region,
                                             min(x_to_remove),
                                             max(x_to_remove))

      } else {
        new_data_x <- x_pos_region
      }

      plot(x_pos_region, type = 'l',
           ylab="Position (X)")
      points(x_to_remove, x_pos_region[x_to_remove],
             col = 'red', pch = 19)
      lines(new_data_x, col="red")

      Sys.sleep(0.5)

      # happy <- ask_again(c("Yes", "No, move to next."),
      #                    title="Are you happy with fix?",
      #                    print_message = FALSE)

      happy <- readline(prompt = "Are you happy with interpolation [Yy/Nn]? : >")

      while(!tolower(happy) %in% c('y', 'n')){
        message("Sorry...option not available, answer with [Yy/Nn]")
        happy <- readline(prompt = "Are you happy with interpolation [Yy/Nn]? : >")
      }

      if(happy == "Yes" | happy == "y"){
          message("Modifying data...")
          x[low:high] <- new_data_x

      } else {
        # Do nothing
        print("Candidates were not modified")
        ## TODO: try to find new parameters

      }

    }

    # stop("debug error")
    # clean graphical devices
    graphics.off()
    #print(Cstack_info())

  }


 return(x)

}
