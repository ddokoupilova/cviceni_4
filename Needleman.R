
library(Biostrings)

NW <- function (S1,S2,match,mismatch,gap){
  
  S1 <- DNAString(S1)
  S2 <- DNAString(S2)
  
  m <- 1 + length(S1)
  n <- 1 + length(S2)
  S <- (0:(n-1))*gap
  
  for (i in 2:m) {
    s <- S[1]
    c <- S[1] + gap
    S[1] <- c
  
    for (j in 2:n) {
      if(S1[i-1] == S2[j-1]){
        pom <- match
      }
      else{
        pom <- mismatch
      }
      c <- max(c(S[j]+gap,c+gap,s+pom))
      s <- S[j]
      S[j] <- c
    }
  }
  return(S)
}