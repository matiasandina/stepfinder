#' @title Diagnose Detection
#'
#' @description This function is meant to provide a fast plot of the `xy` position trace, `x` and `y` velocities for diagnosis. It can inform the threshold needed for finding wrong detections. User input is required to decide whether the detections are correct (nothing needs to be done) or incorrect (tagged for step detection).
#' @param df data.frame containing `x` and `y` coordinates of subject `id`.
#' @keywords diagnostic
#' @export
#' @return data.frame with id and user input regarding correct
#' @examples
#' set.seed(123)
#' df <- data.frame(id = "first_target", frameID=1:1000, x=rnorm(1000, 0, 5), y = rnorm(1000, 0, 5))
#' diagnose_detection(df)

library(ggplot2)

diagnose_detection <- function(df){

  if("id" %in% colnames(df)){
    message(sprintf("Running diagnostics for %s",
                    unique(df$id)))
  } else {
    message("No id found, assigning random id.\n This is not reproducible, if IDs matter assign IDs beforehand!")
    rand_id <- floor(runif(1, min = 1000, max = 10000))
    df$id <- rand_id
  }


  # X velocity
p2 <- ggplot(df, aes(frameID, c(0, diff(x))))+
  geom_line()+
  theme_bw()+
  ggtitle("Velocity in x") + xlab("Time (frames)") +
  ylab("Vx (px/frame)")


p3 <- ggplot(df, aes(frameID, c(0, diff(y))))+
  geom_line()+
  theme_bw()+
  ggtitle("Velocity in y") +
  xlab("Time (frames)") +
  ylab("Vy (px/frame)")

final <- cowplot::plot_grid(p2, p3, nrow = 2)

path_plot <- ggplot(df, aes(x,y)) +
  geom_path(alpha=0.5) + theme_bw() +
  ggtitle(sprintf("Path for %s", unique(df$id)))

# Show path plot
invisible(readline(prompt="Press [enter] to see animal path: > "))
print(path_plot)
# Show diagnostic metris and marble plot
invisible(readline(prompt="Press [enter] to see diagnostics: >"))
print(final)

# We return a message

ask = readline(prompt="Detection was correct [Yy/Nn]: >")

while(!tolower(ask) %in% c("y","n")){
  message("Please answer the prompt with [Yy/Nn]")
  ask = readline(prompt="Detection was correct [Yy/Nn]: >")
}

out_df = data.frame(id = unique(df$id),
                    pass_diagnostic = tolower(ask),
                    stringsAsFactors = FALSE)

return(out_df)
}
