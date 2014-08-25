if (!exists("true_pop")) {
  stop("Expected true_pop to exists")
}

if (!file.exists("cache-cases")) {
  dir.create("cache-cases")
}

if (!file.exists("cache-disclap")) {
  dir.create("cache-disclap")
}

# Guilty suspect, H, and a dataset where he is unobserved
sample_case_Hp <- function(dbsize, seed) {  
  casename <- paste0("case-Hp-dbsize-", sprintf("%05d", dbsize), "-seed-", sprintf(paste0("%0", nchar(.Machine$integer.max), "d"), seed))
  filename <- paste0("cache-cases/", casename, ".Rdata")
  
  if (file.exists(filename)) {
    load(filename)
    return(case)
  }

  set.seed(seed)  
  
  unsuccesful_tries <- 0L
  
  while (TRUE) {
    indices_db <- sample(x = 1L:nrow(true_pop), size = dbsize, replace = TRUE, prob = true_pop$pop_freq)
    guilty_suspect_id <- sample(x = 1L:nrow(true_pop), size = 1L, replace = TRUE, prob = true_pop$pop_freq)
  
    if (!(guilty_suspect_id %in% indices_db)) {
      db <- true_pop[indices_db, ]
      db$counts <- NULL
      
      guilty_suspect <- true_pop[guilty_suspect_id, ]
      guilty_suspect$counts <- NULL
      
      case <- list(
        casename = casename,
        unsuccesful_tries = unsuccesful_tries, 
        dbsize = dbsize,
        db = db, 
        guilty_suspect = guilty_suspect)
      
      save(case, file = filename)
      
      return(case)
    }
    
    unsuccesful_tries <- unsuccesful_tries + 1L
  }
}

# Innocent suspect, H, and a dataset where he is unobserved
sample_case_Hd <- function(dbsize, seed) {
  casename <- paste0("case-Hd-dbsize-", sprintf("%05d", dbsize), "-seed-", sprintf(paste0("%0", nchar(.Machine$integer.max), "d"), seed))
  filename <- paste0("cache-cases/", casename, ".Rdata")
  
  if (file.exists(filename)) {
    load(filename)
    return(case)
  }

  set.seed(seed)  
  
  unsuccesful_tries <- 0L
  
  while (TRUE) {
    indices_db <- sample(x = 1L:nrow(true_pop), size = dbsize, replace = TRUE, prob = true_pop$pop_freq)
    guilty_suspect_id <- sample(x = 1L:nrow(true_pop), size = 1L, replace = TRUE, prob = true_pop$pop_freq)
  
    if (!(guilty_suspect_id %in% indices_db)) {
      # Now draw the innocent suspect:
      innocent_suspect_id <- sample(x = 1L:nrow(true_pop), size = 1L, replace = TRUE, prob = true_pop$pop_freq)
      
      innocent_suspect_matched <- (innocent_suspect_id == guilty_suspect_id)
      
      db <- NULL
      guilty_suspect <- NULL
      
      if (innocent_suspect_matched) {
        db <- true_pop[indices_db, ]
        db$counts <- NULL

        guilty_suspect <- true_pop[guilty_suspect_id, ]
        guilty_suspect$counts <- NULL
      }      
      
      case <- list(
        casename = casename,
        unsuccesful_tries = unsuccesful_tries, 
        dbsize = dbsize,
        db = db, 
        guilty_suspect = guilty_suspect, 
        innocent_suspect_matched = innocent_suspect_matched)
      
      save(case, file = filename)
      
      return(case)
    }
    
    unsuccesful_tries <- unsuccesful_tries + 1L
  }
}

