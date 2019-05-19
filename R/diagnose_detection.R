# This function is meant to diagnose the detection
# Input the semi-processed df

library(ggplot2)

diagnose_detection <- function(df){
  
  message(sprintf("Running diagnostics for %s", unique(df$animal_id)))
  
  # Let's do 3 plots
  par(mfrow=c(3,1))
  
  # Marble burrying
  
p1<- ggplot(df, aes(frameID, adj_blobs)) +
     geom_line()+
     geom_line(aes(frameID, filtered_blobs), colour="red", lwd=1.2)+
     theme_bw()+
     ggtitle(paste("Marbles burried", unique(df$animal_id), sep=" "))+
    xlab("Time (frames)") + ylab("Number of Marbles")
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

final <- cowplot::plot_grid(p1, p2, p3, nrow = 3)

path_plot <- ggplot(df, aes(x,y)) +
  geom_path(alpha=0.5) + theme_bw() +
  ggtitle(sprintf("Path for %s", unique(df$animal_id)))

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

out_df = data.frame(animal_id = unique(df$animal_id),
                    pass_diagnostic = tolower(ask),
                    stringsAsFactors = FALSE)

return(out_df)
}