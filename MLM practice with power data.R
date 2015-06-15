##### MLM Practice with Power Thesis Data #####
library(arm)

summary(m.sad.diff)
display(m.sad.diff)

x.new <- data.frame(angerdummy=0)
x.new2 <- data.frame(angerdummy=1)
pred.sad <- predict(m.sad.diff, x.new, interval='prediction', level=.95)
pred.angry <- predict(m.sad.diff, x.new2, interval='prediction', level=.95)
pred.sad-pred.angry
pred.sad/pred.angry


## simulate results
n.sims <- 10000
sim.saddiff <- sim(m.sad.diff, n.sims)
quantile(coef(sim.saddiff)[,1], c(.025, .975)) #95% CI for intercept
quantile(coef(sim.saddiff)[,2], c(.025, .975)) #95% CI for slope

## For MLM
display(m1)
sim.m1 <- sim(m1, n.sims)
quantile(fixef(sim.m1)[,'emot.cent'], c(.025, .975))
quantile(fixef(sim.m1)[,'pow.cent'], c(.025, .975))
quantile(fixef(sim.m1)[,'emot.cent:pow.cent'], c(.025, .975))


## simulate new data with prediction intervals
n.tilde <- length(pow$angerdummy)
X.tilde <- cbind(rep(1,n.tilde), pow$angerdummy)
n.sims <- 1000
sim.saddiff2<- sim(m.sad.diff, n.sims)
y.tilde <- array(NA, c(n.sims, n.tilde))
for(s in 1:n.sims){
    y.tilde[s,] <- rnorm(n.tilde, X.tilde %*% coef(sim.saddiff2)[s,], sigma.hat(sim.saddiff2)[s])
}

## Try a nonlinear model
momed <- momed
momed[momed>4 & !is.na(momed)] <- momed[momed>4 & !is.na(momed)]-2
jit.sad <- with(powopen, o_sad.hp+rnorm(length(o_sad.hp), mean=0, sd=.01))
jit.mom_ed <- with(powopen, momed+rnorm(length(momed), mean=0, sd=.1))
fit <- lm(o_sad.hp ~ momed, data=powopen)
display(fit)
sim.fit <- sim(fit, n.sims)
plot(coef(sim.fit)[,1], coef(sim.fit)[,2], xlab=expression(beta[0]), ylab=expression(beta[1]))
plot(jit.sad ~ jit.mom_ed, data=powopen)
for(s in 1:20){
    curve(invlogit(coef(sim.fit)[s,1] + coef(sim.fit)[s,2]*x), col='gray', add=TRUE)
}
curve(invlogit(coef(fit)[1]+coef(fit)[2]*x), add=TRUE)

