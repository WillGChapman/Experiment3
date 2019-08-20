# Crossings of lines
# Logarithmic spiral

xl <- perfectx
yl <- perfecty
plot(xl, yl, type = "l", lwd = 2, col = "blue",
     xlim = c(0, 0.5), ylim = c(0, 1), xlab = "", ylab = "",
     main = "Intersecting Trajectories")
grid()

# 

xa <- track[1,]
ya <- track[2,]
lines(xa, ya, type = "l", lwd = 2, col = "red")
L1 <- rbind(xl, yl)
L2 <- rbind(xa, ya)
P <- poly_crossings(L1, L2)
points(P)
