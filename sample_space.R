setwd('~/01. Dartmouth/04. Coursework/05. Fall 2022/03. Big Data Bowl/04. Linked Git')

Transform.Values <- function(x) {1.025**(x)-1}

Compute.Isoceles <- function(angle, base) {
  theta <- (180-angle)/2
  h <- tan(theta * (pi/180)) * (base/2)
  side <- sqrt(h^2 + (base/2)^2)
  return(side)
}

x <- c()
y <- c()
values <- c()

for (i in seq(135,180, length.out=100)) {
  
  for (j in seq(0.001, 7, length.out=500)) {
    side <- Compute.Isoceles(angle=i, base=j)
    base <- j
  
    if (Transform.Values(base) >= Transform.Values(side)*2) {
      val <- "lightgreen"
    } else {
      val <- "pink"
    }
    
    x <- c(x, j)   
    y <- c(y, i)
    values <- c(values, val)
    
  }
}

png('images/sample_space.png', units='in', width=10, height=7, res=700)
plot(x, y, col=values, pch=15, xlab='DE-QB Distance (yards)', ylab="OT's Angle", main='Transformed Sample Space')
text(x=2.5, y=145, label='OT not on shortest path')
text(x=4.5, y=170, label='OT on shortest path')
dev.off()