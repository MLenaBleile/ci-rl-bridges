# Create a diagram with multiple equal-height optima

# Set up a simple plotting area
png("irl_diagram_equal_heights.png", width=700, height=400)
par(mar=c(4, 4, 1, 1))

# Create an empty plot
plot(NULL, xlim=c(0, 10), ylim=c(0, 10), 
     xlab="Policy", ylab="Returns", axes=FALSE)

# Add the x-axis
axis(1, at=c(0,10), labels=FALSE, lwd=1, tck=0)
#segments(0, 0, 10, 0)

# Add the y-axis
axis(2, at=c(0,10), labels=FALSE, lwd=1, tck=0)
#segments(0, 0, 0, 10)

# Add "Returns" label on y-axis
text(-0.8, 5, "Returns", srt=90, cex=1.2)

# Add "Policy" label at bottom
text(5, -1, "Policy", cex=1.2)

# Add vertical separators and Xm labels
segments(1.5, 0, 1.5, 10, lty=2)
segments(4.5, 0, 4.5, 10, lty=2)
segments(7.5, 0, 7.5, 10, lty=2)
text(2.5, 1.5, "Xm=0", cex=0.9)
text(5.5, 1.5, "Xm=2", cex=0.9)
text(8.5, 1.5, "Xm=3", cex=0.9)

# Define three separate sine functions centered at the three optimal policy points
# Each will peak at the SAME height
x <- seq(0, 10, length.out=500)

# Target peak locations
peak_x1 <- 2.5
peak_x2 <- 5.5
peak_x3 <- 8.5

# Target peak height
peak_height <- 7

# Generate a smooth curve with equal-height peaks
y <- rep(0, length(x))

# Create localized bumps at each peak location
for(i in 1:length(x)) {
  # First peak contribution
  dist1 <- abs(x[i] - peak_x1)
  if(dist1 < 2) {
    y[i] <- y[i] + peak_height * (1 - (dist1/2)^2)
  }
  
  # Second peak contribution
  dist2 <- abs(x[i] - peak_x2)
  if(dist2 < 2) {
    y[i] <- y[i] + peak_height * (1 - (dist2/2)^2)
  }
  
  # Third peak contribution
  dist3 <- abs(x[i] - peak_x3)
  if(dist3 < 2) {
    y[i] <- y[i] + peak_height * (1 - (dist3/2)^2)
  }
}

# Connect the valleys between peaks with a smooth curve
# Find the indices of the peaks
peak1_idx <- which.min(abs(x - peak_x1))
peak2_idx <- which.min(abs(x - peak_x2))
peak3_idx <- which.min(abs(x - peak_x3))

# Create a more natural continuous curve
# Adjust the values between peaks to create a smooth, continuous curve
for(i in 1:length(x)) {
  if(x[i] < peak_x1) {
    # Before first peak - gradual rise
    y[i] <- 2 + (peak_height - 2) * (x[i]/peak_x1)^2
  } else if (x[i] > peak_x1 && x[i] < peak_x2) {
    # Between first and second peak
    if(y[i] == 0) { # Only adjust points not already part of a peak
      midpoint <- (peak_x1 + peak_x2)/2
      position <- (x[i] - peak_x1)/(peak_x2 - peak_x1)
      valley_depth <- 3 # How low the curve dips between peaks
      y[i] <- peak_height - (peak_height - valley_depth) * 4 * position * (1 - position)
    }
  } else if (x[i] > peak_x2 && x[i] < peak_x3) {
    # Between second and third peak
    if(y[i] == 0) { # Only adjust points not already part of a peak
      midpoint <- (peak_x2 + peak_x3)/2
      position <- (x[i] - peak_x2)/(peak_x3 - peak_x2)
      valley_depth <- 2 # How low the curve dips between peaks
      y[i] <- peak_height - (peak_height - valley_depth) * 4 * position * (1 - position)
    }
  } else if (x[i] > peak_x3) {
    # After the third peak - gradual decline
    position <- (x[i] - peak_x3)/(10 - peak_x3)
    y[i] <- max(0, peak_height - position * 10)
  }
}

# Smooth the curve
y_smooth <- stats::smooth.spline(x, y, spar=0.35)$y

# Draw the continuous curve
lines(x, y_smooth, lwd=2)

# Add the three labeled peaks (optimal policies)
points(peak_x1, peak_height, pch=19, cex=1)
points(peak_x2, peak_height, pch=19, cex=1)
points(peak_x3, peak_height, pch=19, cex=1)

# Add the π labels exactly as in your sketch
text(peak_x1, peak_height+0.7, expression(pi[1]^"*"), cex=1.2)
text(peak_x2, peak_height+0.7, expression(pi[2]^"*"), cex=1.2)
text(peak_x3, peak_height+0.7, expression(pi[3]^"*"), cex=1.2)

# Close the PDF device
dev.off()

