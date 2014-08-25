# case <- Hp_cases[[1L]]
# str(case)
disclap_LRs <- function(case) {
  if (is.null(case) || is.null(case$db)) {
    return(NULL)
  }
  
  Hs <- case$guilty_suspect
  db <- rbind(case$db, Hs, Hs)
  db$pop_freq <- NULL
  db <- apply(as.matrix(db), 2L, as.integer)
  
  clusters_max <- 5L

  res <- NULL
  
  while (TRUE) {
    
    fits <- lapply(1L:clusters_max, function(clusters) {
      dirname <- paste0("cache-disclap/", case$casename)
      
      if (!file.exists(dirname)) {
        dir.create(dirname)
      }

      fitname <- paste0(dirname, "/clusters-", sprintf("%03d", clusters), ".Rdata")
      
      if (file.exists(fitname)) {
        load(fitname)
        return(fit)
      }

      fit <- disclapmix(db, clusters = clusters)
      save(fit, file = fitname)
      return(fit)
    })
    
    BICs <- sapply(fits, function(fit) fit$BIC_marginal)
    AICcs <- sapply(fits, function(fit) fit$AICc_marginal)
    
    # We always start at cluster = 1, s.t. index equals clusters
    BIC_top3 <- order(BICs, decreasing = FALSE)[1L:3L] # increasing
    AICc_top3 <- order(AICcs, decreasing = FALSE)[1L:3L] # increasing        
    best_clusters_top3 <- unique(c(BIC_top3, AICc_top3))
    best_clusters_max <- max(best_clusters_top3)
    
    if ((clusters_max - best_clusters_max) <= 1L) { # At least distance 2 to end
      clusters_max <- clusters_max + 5L
    } else {
      Hs <- matrix(as.integer(Hs[, 1L:(length(Hs) - 1L)]), nrow = 1L)
      
      fits_Hs_probs <- unlist(lapply(fits, function(fit) predict(fit, newdata = Hs)))
      
      LR_best <- 1 / max(fits_Hs_probs)      
      LR_BIC_best <- 1 / fits_Hs_probs[BIC_top3[1L]]
      LR_BIC_top3 <- 1 / mean(fits_Hs_probs[BIC_top3])
      LR_AICc_best <- 1 / fits_Hs_probs[AICc_top3[1L]]
      LR_AICc_top3 <- 1 / mean(fits_Hs_probs[AICc_top3])
      
      clusters_best <- nrow(fits[[which.max(fits_Hs_probs)]]$y)
      clusters_BIC_best <- nrow(fits[[BIC_top3[1L]]]$y)
      clusters_BIC_top3 <- paste0(unlist(lapply(fits[BIC_top3], function(fit) nrow(fit$y))), collapse = ",")
      clusters_AICc_best <- nrow(fits[[AICc_top3[1L]]]$y)
      clusters_AICc_top3 <- paste0(unlist(lapply(fits[AICc_top3], function(fit) nrow(fit$y))), collapse = ",")
      
      res <- list(
        LRBest = LR_best,
        LRBICBest = LR_BIC_best,
        LRBICTop3Mean = LR_BIC_top3,
        LRAICcBest = LR_AICc_best,
        LRAICcTop3Mean = LR_AICc_top3,
        ClustersBest = clusters_best,
        ClustersBICBest = clusters_BIC_best,
        ClustersBICTop3 = clusters_BIC_top3,
        ClustersAICcBest = clusters_AICc_best,
        ClustersAICcTop3 = clusters_AICc_top3
      )
      
      break
    }
  }
  
  return(res)
}

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
  Count_LR <- (N + 1) / 1
  
  if (N2 == 0L) {
    GT_LR <- NA
  }  
  
  if (N1 == N) {
    GT_LR <- NA
    Brenner_LR <- NA
  }
  
  disclap <- disclap_LRs(case)
  
  res <- list(
    spectrum = spectrum,
    LRs = list(
      True = 1/case$guilty_suspect$pop_freq,
      GT = GT_LR,
      Brenner = Brenner_LR,
      Count = Count_LR,
      DiscLapBest = disclap$LRBest,
      DiscLapBICBest = disclap$LRBICBest,
      DiscLapBICTop3Mean = disclap$LRBICTop3Mean,
      DiscLapAICcBest = disclap$LRAICcBest,
      DiscLapAICcTop3Mean = disclap$LRAICcTop3Mean
    ),
    DiscLapInfo = list(
      ClustersBest = disclap$ClustersBest,
      ClustersBICBest = disclap$ClustersBICBest,
      ClustersBICTop3 = disclap$ClustersBICTop3,
      ClustersAICcBest = disclap$ClustersAICcBest,
      ClustersAICcTop3 = disclap$ClustersAICcTop3
    )
    #,
    #case = case
  )
  
  return(res)
}


