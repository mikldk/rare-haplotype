load("../../data/y23_2014-05-23.RData")

library(disclapmix)
library(parallel)
library(dplyr)
library(tidyr)
library(ggplot2)

# Return data frame with unique rows and their counts
count_rows <- function(x) { 
  order.x <- do.call(order,as.data.frame(x))
  equal.to.previous <- rowSums(x[tail(order.x,-1),] != x[head(order.x,-1),]) == 0
  indices <- split(order.x, cumsum (c (TRUE, !equal.to.previous)))
  x <- x[sapply (indices, function (x) x [[1]]), , drop = FALSE]
  return(data.frame(counts = sapply(indices, length), x))
}

# Consider 5 loci only
if (!exists("true_pop")) { #onde evitare di rinominare una variabile che si chiama pop 
  pop <- data.frame(y23$haplotypes$integer_alleles$db, Population = y23$haplotypes$integer_alleles$population)
  pop <- merge(pop, y23$population_statistics, by = "Population")

  eu_pop <- subset(pop, Ethnos == "European", c(DYS19, DYS389I, DYS389II.I, DYS390, DYS391))  

  #definisce la "vera" popolazione
  true_pop <- count_rows(eu_pop) #database senza ripetizioni e con i count!
  true_pop$pop_freq <- true_pop$counts / sum(true_pop$counts)
}


