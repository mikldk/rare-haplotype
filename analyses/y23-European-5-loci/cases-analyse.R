source("include.R")
source("include-cases-simulate.R")
source("include-cases-LR.R")

N_Hp <- 100L  # Number of Hp cases
N_Hd <- 100000L # Number of Hd cases

DBSIZE <- 100L

################################################################################

set.seed(1L)
Hp_seeds <- sample.int(.Machine$integer.max, N_Hp) 

set.seed(2L)
Hd_seeds <- sample.int(.Machine$integer.max, N_Hd) 

################################################################################

Hp_cases <- mclapply(Hp_seeds, function(seed) sample_case_Hp(DBSIZE, seed), mc.cores = detectCores())
Hd_cases <- mclapply(Hd_seeds, function(seed) sample_case_Hd(DBSIZE, seed), mc.cores = detectCores())

################################################################################

Hp_cases_LRs <- mclapply(Hp_cases, case_LRs, mc.cores = detectCores())
Hd_cases_LRs <- mclapply(Hd_cases, case_LRs, mc.cores = detectCores())
Hd_cases_matches_LRs <- Hd_cases_LRs[!unlist(lapply(Hd_cases_LRs, is.null))]

cat("Percentage non-matching Hd: ", round(100*(1 - (length(Hd_cases_matches_LRs) / length(Hd_cases_LRs))), 3), "%\n", sep = "")

################################################################################

Hp_cases_LRs_matrix <- do.call(rbind, lapply(Hp_cases_LRs, function(l) do.call(cbind, l$LRs)))
Hd_cases_matches_LRs_matrix <- do.call(rbind, lapply(Hd_cases_matches_LRs, function(l) do.call(cbind, l$LRs)))

# tidyr + dplyr
Hp_LR <- data.frame(Hp_cases_LRs_matrix) %>% 
  gather(Estimator, LR) %>%
  mutate(Case = "Hp")

Hd_LR <- data.frame(Hd_cases_matches_LRs_matrix) %>% 
  gather(Estimator, LR) %>%
  mutate(Case = "Hd")

LRs <- rbind(Hp_LR, Hd_LR)

p <- ggplot(LRs, aes(Case, LR)) + geom_boxplot(aes(color = Estimator))

pdf("fig/fig-cases-LR-Hp-Hd.pdf", width = 8, height = 6)
print(p)
dev.off()

################################################################################

aggregate(LR ~ Estimator + Case, LRs, median)

aggregate(LR ~ Estimator + Case, LRs, summary)

################################################################################
library(ROCR)

LR_preds <- split(LRs, LRs$Estimator)
LR_preds$Count <- NULL
LR_preds <- lapply(LR_preds, function(df) with(df, prediction(LR, Case)))

plot(performance(LR_preds[[1L]], "tpr", "fpr"), colorize = TRUE)

for (i in seq_along(LR_preds)[-1L]) {
  plot(performance(LR_preds[[i]], "tpr", "fpr"), add = TRUE, colorize = TRUE)
}

