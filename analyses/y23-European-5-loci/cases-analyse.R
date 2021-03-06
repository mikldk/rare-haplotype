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

cat("-> Sampled Hp                = ", N_Hp, "\n", sep = "")
cat("   Sampled Hd                = ", N_Hd, "\n", sep = "")
cat("->   of which are matching   = ", length(Hd_cases_matches_LRs), "\n", sep = "")
cat("     percentage non-matching = ", round(100*(1 - (length(Hd_cases_matches_LRs) / N_Hd)), 3), "%\n", sep = "")
cat("(Numbers with -> are the number of cases analysed.)\n")

N1_proportion_Hp <- unlist(lapply(Hp_cases_LRs, function(l) sum(l$spectrum == 1L) / sum(l$spectrum)))
N1_proportion_Hd_matches <- unlist(lapply(Hd_cases_matches_LRs, function(l) sum(l$spectrum == 1L) / sum(l$spectrum)))
N1_ttest <- t.test(N1_proportion_Hp, N1_proportion_Hd_matches, paired = FALSE)

N2_proportion_Hp <- unlist(lapply(Hp_cases_LRs, function(l) sum(l$spectrum == 2L) / sum(l$spectrum)))
N2_proportion_Hd_matches <- unlist(lapply(Hd_cases_matches_LRs, function(l) sum(l$spectrum == 2L) / sum(l$spectrum)))
N2_ttest <- t.test(N2_proportion_Hp, N2_proportion_Hd_matches, paired = FALSE)

pdf("fig/fig-cases-spectrum-properties.pdf", width = 6, height = 8)
par(mfrow = c(2, 1))
boxplot(N1_proportion_Hp, N1_proportion_Hd_matches, 
  names = c("Hp", "Hd (match)"), 
  main = paste0("t-test p-value of equality = ", formatC(N1_ttest$p.value)), 
  varwidth = TRUE)
boxplot(N2_proportion_Hp, N2_proportion_Hd_matches, 
  names = c("Hp", "Hd (match)"), 
  main = paste0("t-test p-value of equality = ", formatC(N2_ttest$p.value)), 
  varwidth = TRUE)
dev.off()

################################################################################

Hp_cases_LRs_df <- data.frame(do.call(rbind, lapply(Hp_cases_LRs, function(l) do.call(cbind, l$LRs))))
Hd_cases_matches_LRs_df <- data.frame(do.call(rbind, lapply(Hd_cases_matches_LRs, function(l) do.call(cbind, l$LRs))))

#Hp_cases_LRs_df$CaseId <- paste0("Hp_", 1L:nrow(Hp_cases_LRs_df))
#Hd_cases_matches_LRs_df$CaseId <- paste0("Hd_", 1L:nrow(Hd_cases_matches_LRs_df))

# tidyr + dplyr
Hp_LR <- Hp_cases_LRs_df %>% 
  gather(Estimator, LR) %>%
  mutate(Case = "Hp")

Hd_LR <- Hd_cases_matches_LRs_df %>% 
  gather(Estimator, LR) %>%
  mutate(Case = "Hd")

LRs <- rbind(Hp_LR, Hd_LR)

p <- ggplot(LRs, aes(Case, LR)) + 
  geom_boxplot(aes(color = Estimator)) +
  scale_y_log10()

pdf("fig/fig-cases-LR-Hp-Hd-boxplot.pdf", width = 8, height = 6)
print(p)
dev.off()

##

if (FALSE) {
  Hp_cases_LRs_df_log10 <- log10(Hp_cases_LRs_df[, -which(colnames(Hp_cases_LRs_df) == "Count")])
  p <- ggpairs(Hp_cases_LRs_df_log10, title = "log10(LR)")

  pdf("fig/fig-cases-pairwise-LR-Hp.pdf", width = 16, height = 16)
  print(p)
  dev.off()

  Hd_cases_matches_LRs_df_log10 <- log10(Hd_cases_matches_LRs_df[, -which(colnames(Hd_cases_matches_LRs_df) == "Count")])
  p <- ggpairs(Hd_cases_matches_LRs_df_log10)

  pdf("fig/fig-cases-pairwise-LR-Hd-matching.pdf", width = 16, height = 16)
  print(p)
  dev.off()
}

Hp_cases_LRs_df_log10 <- log10(Hp_cases_LRs_df[, -which(colnames(Hp_cases_LRs_df) == "Count")])
Hd_cases_matches_LRs_df_log10 <- log10(Hd_cases_matches_LRs_df[, -which(colnames(Hd_cases_matches_LRs_df) == "Count")])

draw_comparison <- function(df, sub, ref = "True") {
  par(mfrow = n2mfrow(ncol(df) - 1L))
  
  ref_level <- which(colnames(df) == ref)
  
  for (i in (1L:ncol(df))[-ref_level]) {
    xs <- df[, ref_level]
    ys <- df[, i]
    
    # distance between line Ax + By + C = 0 and point (m, n)
    # d = |Am + Bn + C| / sqrt(A^2 + B^2)
    
    # Here, we want the identity line, y = x
    # A = 1, B = -1, C = 0
    # d = |m - n| / sqrt(2)

    ds <- sum(abs(xs - ys) / sqrt(2))
    
    plot(xs, ys, xlab = ref, ylab = colnames(df)[i], main = paste0("sum(D(points, y = x)) = ", round(ds, 2)), sub = sub)
    abline(a = 0, b = 1)
  }
}

pdf("fig/fig-cases-pairwise-LR-Hp.pdf", width = 10, height = 10)
draw_comparison(Hp_cases_LRs_df_log10, sub = "On log10 scale", ref = "True")
dev.off()

pdf("fig/fig-cases-pairwise-LR-Hd-matching.pdf", width = 10, height = 10)
draw_comparison(Hd_cases_matches_LRs_df_log10, sub = "On log10 scale", ref = "True")
dev.off()

################################################################################

aggregate(LR ~ Estimator + Case, LRs, median)

aggregate(LR ~ Estimator + Case, LRs, summary)

################################################################################
LR_preds <- split(LRs, LRs$Estimator)
LR_preds$Count <- NULL
LR_preds <- lapply(LR_preds, function(df) with(df, prediction(LR, Case, label.ordering = c("Hd", "Hp")))) # Hd = -, Hp = +

cols <- brewer.pal(length(LR_preds), "Set2")

pdf("fig/fig-cases-LR-Hp-Hd-ROC.pdf", width = 8, height = 6)
plot(performance(LR_preds[[1L]], measure = "tpr", x.measure = "fpr"), col = cols[1L])

for (i in seq_along(LR_preds)[-1L]) {
  plot(performance(LR_preds[[i]], measure = "tpr", x.measure = "fpr"), add = TRUE, col = cols[i])
}

legend("bottomright", names(LR_preds), col = cols, lty = 1)
dev.off()

if (FALSE) {
  pdf("fig/fig-cases-LR-Hp-Hd-cutoff-fnr.pdf", width = 8, height = 6)
  plot(performance(LR_preds[[1L]], measure = "fnr", x.measure = "cutoff"), xlim = range(LRs$LR), ylim = c(0, 1), col = cols[1L])

  for (i in seq_along(LR_preds)[-1L]) {
    plot(performance(LR_preds[[i]], measure = "fnr", x.measure = "cutoff"), add = TRUE, col = cols[i])
  }

  legend("bottomright", names(LR_preds), col = cols, lty = 1)
  dev.off()
}

