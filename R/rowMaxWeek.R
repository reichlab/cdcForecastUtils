rowMaxWeek <-
function(trajectories){
  retarr <- apply(trajectories,1,which.max)
  return(retarr)
}
