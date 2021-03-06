#' @title Manual Step Removal
#'
#' @name manual_step_removal
#' @description This function handles manual step removal.
#' @param x numeric vector, passed through `fix_detection_jumps`
#' @param point position in x to be evaluated.
#' @param window integer, number of positions to create a window before and after `point`, default=50.
#' @keywords diagnostic
#' @export manual_step_removal
#' @return modified x depending on user input. Calls `remove_gap_interpolate`.
#' @examples

manual_step_removal <- function(x, point, candidates,
                                window=50){

  df <- data.frame(frameID = 1:length(x),
                   variable = x)

  low_zoom <- max(1, point - window)
  high_zoom <- min(point + window, length(x))

  p1 <- ggplot(df, aes(frameID, variable)) +
    geom_line() +
    geom_point(data=data.frame(frameID=point,
                               variable=x[point]), size=2)+
    ggforce::facet_zoom(xlim=c(low_zoom,high_zoom)) +
    theme_light()+
    theme(strip.background = element_rect(fill="red"))

  print(p1)

  message("Diagnose detection:")
  ask <- readline(prompt=("Good Detection --> Keep or Bad Detection --> Remove?? [(1/2)]: >"))

  while(!ask %in% c(1,2)){
    message("Sorry, option not available. Choose 1 or 2.")
    ask <- readline(prompt=("Good Detection --> Keep or Bad Detection --> Remove?? [(1/2)]: >"))
  }

  if(ask == 2){
    message("Select range from possible candidates.")
    print(candidates)
      to <- as.numeric(readline(prompt = "type the candidate at the end of the step. > "))

    while(!to %in% candidates){
      message("Sorry...candidate not available,\nchoose from available candidates")
      print(candidates)
      to <- as.numeric(readline(prompt = "type the candidate at the end of the step. > "))
    }

    # Try with the candidates directly
    # We need to use the candidates with their new axis
    x_to_remove <- c(point:to)
    low <- min(x_to_remove) - 50
    high <- max(x_to_remove) + 50
    x_pos_region <- x[low:high]

    message("Analyzing position close to bad detections")

    # interpolate
    if(length(x_to_remove) > 1){
      new_data_x <- remove_gap_interpolate(x,
                                           min(x_to_remove),
                                           max(x_to_remove))

      df2 <- data.frame(frameID = low:high,
                        variable = new_data_x[low:high])

      # add interpolated data to the graph
      p2 <- p1 + geom_line(data=df2,
                           aes(frameID, variable),
                           color="red")

      print(p2)

      Sys.sleep(0.5)

  }

  happy <- readline(prompt = "Are you happy with interpolation [Yy/Nn]? : >")

  while(!tolower(happy) %in% c('y', 'n')){
    message("Sorry...option not available, answer with [Yy/Nn]")
    happy <- readline(prompt = "Are you happy with interpolation [Yy/Nn]? : >")
  }

  if(happy == "Yes" | happy == "y"){
    message("Modifying data...")
    x[low:high] <- new_data_x[low:high]

  } else {
    # Do nothing
    print("Candidates were not modified")
    ## TODO: try to find new parameters
  }

  }

  return(x)
}
