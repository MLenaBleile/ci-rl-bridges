# R code to create a visualization of partial ignorability concept
# for two-sample estimation

# Set random seed for reproducibility
set.seed(123)
setwd("~/RL book/Figures/relative ignorability")

# Create sample data for two groups with greater separation
n <- 1000
group1 <- rnorm(n, mean = 4, sd = 1)  # Changed mean from 5 to 4
group2 <- rnorm(n, mean = 8, sd = 1)  # Changed mean from 7 to 8

# Calculate true means
true_mean1 <- mean(group1)
true_mean2 <- mean(group2)
true_diff <- true_mean2 - true_mean1

# Create truncated samples (missing the top 20% of values in each group)
cutoff1 <- quantile(group1, 0.8)
cutoff2 <- quantile(group2, 0.8)
truncated_group1 <- group1[group1 <= cutoff1]
truncated_group2 <- group2[group2 <= cutoff2]

# Calculate means of truncated samples
trunc_mean1 <- mean(truncated_group1)
trunc_mean2 <- mean(truncated_group2)
trunc_diff <- trunc_mean2 - trunc_mean1

# Create the plot
#pdf("relative_ignorability_figure.pdf", width = 10, height = 7)

# Set up the plotting area with more margin space
par(mar = c(6, 4, 4, 2) + 0.1)  # Increase bottom margin for equation
xlim <- c(0, 12)
ylim <- c(0, 0.5)

plot(NA, xlim = xlim, ylim = ylim, 
     xlab = "", ylab = "Density",
     main = "Relative Ignorability in Two-Sample Estimation",
     cex.main = 1.2, cex.lab = 1.1)

# Add density curves
d1 <- density(group1)
d2 <- density(group2)

# Normalize densities to have max height of 0.4 for visibility
max_d <- max(c(max(d1$y), max(d2$y)))
scale_factor <- 0.4 / max_d

lines(d1$x, d1$y * scale_factor, col = "blue", lwd = 2)
lines(d2$x, d2$y * scale_factor, col = "red", lwd = 2)

# Add shaded regions for truncated (missing) data
# Make shading a bit more visible
polygon(c(d1$x[d1$x >= cutoff1], cutoff1), 
        c(d1$y[d1$x >= cutoff1] * scale_factor, 0), 
        col = rgb(0, 0, 1, 0.4), border = NA)
polygon(c(d2$x[d2$x >= cutoff2], cutoff2), 
        c(d2$y[d2$x >= cutoff2] * scale_factor, 0), 
        col = rgb(1, 0, 0, 0.4), border = NA)

# Add vertical lines for means
abline(v = true_mean1, col = "blue", lwd = 2, lty = 2)
abline(v = true_mean2, col = "red", lwd = 2, lty = 2)
abline(v = trunc_mean1, col = "blue", lwd = 2, lty = 3)
abline(v = trunc_mean2, col = "red", lwd = 2, lty = 3)

# Add text labels for means with better spacing
text(true_mean1 - 0.2, 0.45, expression(theta[1]), col = "blue", cex = 1.2, pos = 3)
text(true_mean2 + 0.2, 0.45, expression(theta[2]), col = "red", cex = 1.2, pos = 3)
text(trunc_mean1 - 0.2, 0.35, expression(hat(theta)[1](x[0])), col = "blue", cex = 1.2, pos = 1)
text(trunc_mean2 + 0.2, 0.35, expression(hat(theta)[2](x[0])), col = "red", cex = 1.2, pos = 1)

# Add arrows and labels for differences with improved positioning
arrows(true_mean1 + 0.2, 0.48, true_mean2 - 0.2, 0.48, code = 3, length = 0.1, lwd = 2)
text((true_mean1 + true_mean2)/2, 0.5, 
     paste("True difference =", round(true_diff, 2)), cex = 1.1)

arrows(trunc_mean1 + 0.2, 0.32, trunc_mean2 - 0.2, 0.32, code = 3, length = 0.1, lwd = 2)
text((trunc_mean1 + trunc_mean2)/2, 0.29, 
     paste("Estimated difference =", round(trunc_diff, 2)), cex = 1.1)

# Add "missing" labels with better positioning
text(cutoff1 + 0.8, 0.15, "missing", col = "blue", cex = 1.1)
text(cutoff2 + 0.8, 0.15, "missing", col = "red", cex = 1.1)

# Add legend with more space and shorter text
legend("topleft", 
       legend = c("Group 1", "Group 2", "True Means", "Est. Means"),
       col = c("blue", "red", "black", "black"),
       lty = c(1, 1, 2, 3),
       lwd = c(2, 2, 2, 2),
       cex = 0.9,
       bg = "white",  # Add white background
       box.lwd = 0.5)  # Reduce box line width

# Add equation at the bottom with clearer positioning
eq_text <- expression(theta[2] - theta[1] %~~% hat(theta)[2](x[0]) - hat(theta)[1](x[0]))
mtext(eq_text, side = 1, line = 4, cex = 1.2)

mtext("Even with truncated tails (missing data), the difference between group means remains approximately the same", 
      side = 1, line = 5, cex = 1)


# Print the numerical results
cat("True means: Group 1 =", round(true_mean1, 3), "Group 2 =", round(true_mean2, 3), "\n")
cat("True difference =", round(true_diff, 3), "\n\n")
cat("Means after truncation: Group 1 =", round(trunc_mean1, 3), "Group 2 =", round(trunc_mean2, 3), "\n")
cat("Estimated difference =", round(trunc_diff, 3), "\n\n")
cat("Relative error in difference estimation:", round(abs(trunc_diff - true_diff)/true_diff * 100, 2), "%\n")