# Helper to figure out whether they are similar
# all.equal.numeric() should do this, but it is not intuitive
similar <- function(a, b, tol){
  # we assume a and b to be length 1 integers
  # numeric_distance <- dist(c(a, b))
  abs_difference <- abs(a) - abs(b)
  if(abs_difference < tol){
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

## Main function

cluster_candidate_list <- function(positions,
                                   diff_positions,
                                   tol){
  
  if(length(positions) == 0){
    message("Function was called with no positions, early exit before cluster.")
    return(list(data.frame(positions=positions)))
  }
  
  if (length(positions) == 2){
    message("Only 2 positions given, early exit before cluster.")
    return(list(data.frame(positions=positions)))
  }
  
  ### Cluster
  
  # Determine number of centroids
  # We want to group them in pairs
  is_even <- length(positions) %% 2
  
  if(is_even == 0){
    k_centers <- length(positions) / 2
  } else {
    k_centers <- floor(length(positions)/2) + 1
  }
  
  # Get the clustering
  # We iterate a bit more, sometimes it might still be off by random centers
  x_clust <- kmeans(positions,
                    centers = k_centers,
                    nstart = 4, # do it with 2 sets of random starts
                    iter.max = 50)$cluster
  
  print("These clusters were found...")
  print(x_clust)

  candidates <- data.frame(positions,
                   x_clust = x_clust,
                   diff_val = diff_positions[positions])
  
  print(candidates)

  # TODO: Will we use all these variables?
  # We do math here....how useful is the sign ?
  candidates <- 
  candidates %>% group_by(x_clust) %>%
  mutate(
    clust_length = length(x_clust),
    similar = similar(last(diff_val),
                      first(diff_val),
                      tol = tol),
    sign_step = sign(diff_val),
    change_step = first(sign_step) - last(sign_step)) %>%
    ungroup() %>% arrange(positions)

  # Spit a warning if we have some that are not similar
  not_similar <- candidates %>% filter(similar==FALSE)
  if(nrow(not_similar) > 0){
    message("Some steps were not fixed, try calling the function again.\nMaybe increasing the tolerance? See below:")
    print(not_similar)
    
  }
  
  # Split candidates
  x_candidate_list <- candidates %>%
    mutate(x_clust = factor(x_clust)) %>%
    # find those with clusters of 2 and similar magnitude
    filter(clust_length == 2,
           similar == TRUE) %>%
    # Return only the positions (x_clust for debug...sorting problem)
    dplyr::select(positions, x_clust) %>%
    group_by(x_clust, add = TRUE) %>% group_split()

return(x_candidate_list)

}