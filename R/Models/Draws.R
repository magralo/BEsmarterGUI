Draws<-function(Post,burnin,keep){
  k<- dim(Post)[2]
  R<- dim(Post)[1]
  draws<- matrix(double(floor(R/keep)*k), ncol= k)
  for (rep in 1:R) {
    if (rep%%keep == 0) {
      mkeep = rep/keep
      draws[mkeep, ] = Post[rep,]
    }
  }
  draws<- draws[-c(1:ceiling(burnin/keep)),]
  return(draws)
}
