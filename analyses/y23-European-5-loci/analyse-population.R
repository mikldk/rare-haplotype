source("include.R")

#plot of the frequencies of the true population
pdf("fig/fig-truepop-spectrum.pdf", width = 8, height = 6)
plot(sort(true_pop$counts, decreasing = TRUE), xlab = "h", ylab = expression(n[h]))
dev.off()

#proportion of singletons
sum(true_pop$counts == 1L) / sum(true_pop$counts)


