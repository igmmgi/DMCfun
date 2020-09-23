# Create figures within ./figures directory used in README.md file
library(DMCfun)

# figure 1
dmc <- dmcSim(fullData = TRUE)
# png("figures/figure1.png", width = 800, height = 600)
pdf("figures/figure1.pdf", width = 12, height = 8)
plot(dmc, cex.lab = 1.2, cex.axis = 1.2, mgp=c(2.5,1,0))
dev.off()

# figure 2
dmc <- dmcSim(fullData = TRUE, tau = 150)
# png("figures/figure2.png", width = 800, height = 600)
pdf("figures/figure2.pdf", width = 12, height = 8)
plot(dmc, cex.lab = 1.2, cex.axis = 1.2, mgp=c(2.5,1,0))
dev.off()

# figure 3
params <- list(tau = seq(20, 170, 10))
dmc <- dmcSims(params)
# png("figures/figure3.png", width = 800, height = 600)
pdf("figures/figure3.pdf", width = 12, height = 8)
plot(dmc, ncol = 2, col = c("red", "green"), cex.lab = 1.2, cex.axis = 1.2, mgp = c(2.5,1,0))
dev.off()

# figure 4
params <- list(tau = seq(20, 170, 10))
dmc <- dmcSims(params)
# png("figures/figure4.png", width = 800, height = 600)
pdf("figures/figure4.pdf", width = 12, height = 8)
plot(dmc, ncol = 2, col = c("red", "green"), cex.lab = 1.2, cex.axis = 1.2, mgp = c(2.5,1,0))
dev.off()

# figure 5
fit <- dmcFit(flankerData, fitInitialGridN = 30) # flanker data from Ulrich et al. (2015)
# png("figures/figure5.png", width = 800, height = 600)
pdf("figures/figure5.pdf", width = 12, height = 8)
plot(fit, flankerData, cex.lab = 1.2, cex.axis = 1.2, mgp = c(2.5,1,0))
dev.off()

# figure 6
fit <- dmcFit(simonData, fitInitialGridN = 30) # simon data from Ulrich et al. (2015)
# png("figures/figure6.png", width = 800, height = 600)
pdf("figures/figure6.pdf", width = 12, height = 8)
plot(fit, simonData, cex.lab = 1.2, cex.axis = 1.2, mgp = c(2.5,1,0))
dev.off()
