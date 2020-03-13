rowMax <-
function(trajectories){
  retarr <- apply(trajectories,1,max)
  return(retarr)
}
