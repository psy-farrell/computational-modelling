n <- 10
stimrange <- c(1,10)
m <- seq(stimrange[1],stimrange[2],length.out = n)
s <- log(m) + rnorm(n,sd=0.5)
s[s < 0] <- 0

lm_3 <- lm(s ~ poly(m,3))
lm_n <- lm(s ~ poly(m,n-1))

#pdf(file="Nihm76.pdf", width=6, height=6)

par(mfrow=c(2,2))
plot(m,s, pch=3, las=1,
     xlab='Physical Intensity',
     ylab='Perceived Intensity')
points(predict(lm_3))

plot(m,s, pch=3, las=1,
     xlab='Physical Intensity',
     ylab='Perceived Intensity')
points(predict(lm_n))

m_fill <- seq(stimrange[1],stimrange[2],length.out = 1000)
plot(m,s, pch=3, las=1,
     xlab='Physical Intensity',
     ylab='Perceived Intensity')
lines(m_fill,
      predict(lm_3,data.frame(m=m_fill)),
      type="l", lty=1, las=1)

plot(m,s, pch=3, las=1,
     xlab='Physical Intensity',
     ylab='Perceived Intensity')
lines(m_fill,
      predict(lm_n,data.frame(m=m_fill)),
      type="l", lty=1, las=1)

#dev.off()