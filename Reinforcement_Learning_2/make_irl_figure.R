

# Set up a simple plotting area
#png("irl_diagram_exact.png", width=700, height=400)
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

# Add "Returns" label on y-axi
# Add vertical separators and Xm labels
segments(1.5, 0, 1.5, 10, lty=2)
segments(3.5, 0, 3.5, 10, lty=2)
segments(6.5, 0, 6.5, 10, lty=2)
text(2.5, 0.5, "Xm=0", cex=0.9)
text(5.5, 0.5, "Xm=2", cex=0.9)
text(8.5, 0.5, "Xm=3", cex=0.9)

# Define the hand-drawn curve's points - based on visual inspection of your sketch
# Creating a rough approximation of your hand-drawn curve
x <- c(0, 1, 2, 2.5, 3, 4, 5, 5.5, 6, 7, 8, 8.5, 9, 10)
y <- c(2, 2.5, 3, 4.5, 4, 3.5, 5, 7, 6, 5, 6, 6.5, 5.5, 5)

# Draw the line
lines(x, y, lwd=2)

# Add the three labeled peaks
points(2.5, 4.5, pch=19, cex=1)
points(5.5, 7, pch=19, cex=1)
points(8.5, 6.5, pch=19, cex=1)

# Add the π labels exactly as in your sketch
text(2.5, 5.2, expression(pi[1]^"*"), cex=1.2)
text(5.5, 7.7, expression(pi[2]^"*"), cex=1.2)
text(8.5, 7.2, expression(pi[3]^"*"), cex=1.2)

# Close the PDF device
#dev.off()
