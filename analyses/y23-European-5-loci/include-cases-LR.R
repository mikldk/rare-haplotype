case_LRs <- function(case) {
  if (is.null(case) || is.null(case$db)) {
    return(NULL)
  }
  
  spectrum <- count_rows(case$db)$counts
  
  N <- case$dbsize  
  N1 <- sum(spectrum == 1L) # number of singletons 
  N2 <- sum(spectrum == 2L) # number of doubletons

  GT_LR <- N * N1 / (2 * N2)
  Brenner_LR <- N / (1 - ((N1 + 1) / (N + 1)))
  Count_LR <- N + 1
  
  if (N2 == 0L) {
    GT_LR <- NA
  }  
  
  if (N1 == N) {
    GT_LR <- NA
    Brenner_LR <- NA
  }
  
  res <- list(
    spectrum = spectrum,
    LRs = list(
      GT = GT_LR,
      Brenner = Brenner_LR,
      Count = Count_LR
    )
    #,
    #case = case
  )
  
  return(res)
}


