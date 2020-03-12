############################
### UTILITIES
#####################

rowMax <- function(trajectories){
  retarr <- apply(trajectories,1,max)
  return(retarr)
}

rowMaxWeek <- function(trajectories){
  retarr <- apply(trajectories,1,which.max)
  return(retarr)
}
