attach(water.heating)
plot(z ~ block, xaxt = "n", type = "n", xlim = c(0, 5)) # Suppress x-axis, pts
axis(1, at = c(1, 2, 3,4))
points(x = as.numeric(block) + as.numeric(new.code) * 0.05, y = z,
       pch = as.character(as.numeric(new.code)), col = as.numeric(new.code)) 
mtext("block=0,2,4,6", side = 3, adj = 1, line = 1)
abline(h = 0) # Horizontal line at zero
# Margin text, top-rt, line 1 abline(h = 0)

interaction.plot(x.factor = new.code, trace.factor = block, 
                 response= time, xlab="Treatments", ylab="Time", 
                 trace.label="Block", leg.bty="b", leg.bg="white")