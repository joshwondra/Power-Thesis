##### Power MTurk Summer 2015 Syntax #####



##### 2. Import Data, Load Packages, Load Functions #####

pow <- read.csv('Dropbox/Research/Current Projects/Power Online Study/powR2.csv', header=TRUE)
pow <- pow[is.na(pow$powcond)==FALSE & is.na(pow$emocond)==FALSE,]
pow <- pow[pow$empty!=1 & pow$duplicate!=1 & pow$testruns!=1 & pow$emptyappraisals!=1 & pow$prime_prob!=1 & pow$comments!=1 & pow$suspicion!=1,]

library(ggplot2)
library(car)


##### 3. Recode Data #####

pow$powfac <- factor(pow$powcond, levels=c(-1,1), labels=c('low power','high power'))
pow$emofac <- factor(pow$emocond, levels=c(-1,1), labels=c('sad', 'angry')) 

table(pow$powfac, pow$emofac)

pow$group <- with(pow, ifelse(powcond==-1 & emocond==-1, 1, ifelse(powcond==-1 & emocond==1, 2, ifelse(powcond==1 & emocond==-1, 3, ifelse(powcond==1 & emocond==1, 4, NA)))))

##### 4. Analyze Emotion Data
table(pow$powcond, pow$emocond)

round(cor(pow[14:25], use='complete.obs'), digits=2)

names(pow)
qplot(y=sad, x=emofac, fill=powfac, data=pow, geom='boxplot')
qplot(y=down, x=emofac, fill=powfac, data=pow, geom='boxplot')
qplot(y=(sad+down)/2, x=emofac, fill=powfac, data=pow, geom='boxplot')
with(pow, boxplot((sad+down)/2~powfac*emofac*female))
summary(lm((sad+down)/2 ~ powcond*emocond, data=pow))

# low pow/sad, low pow/angry, high pow/sad, high pow/angry
with(pow, t.contrast((sad)/2, group, c(1,0,-1,0)))
with(pow, t.contrast((sad)/2, group, c(0,1,0,-1)))
with(pow, t.contrast((sad)/2, group, c(1,1,-1,-1)))

with(pow, t.contrast((sad+down)/2, group, c(1,0,-1,0)))
with(pow, t.contrast((sad+down)/2, group, c(0,1,0,-1)))
with(pow, t.contrast((sad+down)/2, group, c(1,1,-1,-1)))

qplot(y=angry, x=emofac, fill=powfac, data=pow, geom='boxplot')
qplot(y=mad, x=emofac, fill=powfac, data=pow, geom='boxplot')
qplot(y=(angry+mad)/2, x=emofac, fill=powfac, data=pow, geom='boxplot')

with(pow, t.contrast((angry+mad)/2, group, c(1,0,-1,0)))
with(pow, t.contrast((angry+mad)/2, group, c(0,1,0,-1)))
with(pow, t.contrast((angry+mad)/2, group, c(-1,-1,1,1)))
with(pow, t.contrast((angry+mad)/2, group, c(0,0,-1,1)))

qplot(y=symp, x=emofac, fill=powfac, data=pow, geom='boxplot')
qplot(y=comp, x=emofac, fill=powfac, data=pow, geom='boxplot')
qplot(y=(symp+comp)/2, x=emofac, fill=powfac, data=pow, geom='boxplot')

with(pow, t.contrast((symp+comp)/2, group, c(1,0,-1,0)))
with(pow, t.contrast((symp+comp)/2, group, c(0,1,0,-1)))
with(pow, t.contrast((symp+comp)/2, group, c(-1,-1,1,1)))

qplot(y=pleas, x=emofac, fill=powfac, data=pow, geom='boxplot')
qplot(y=tresp, x=emofac, fill=powfac, data=pow, geom='boxplot')
qplot(y=oresp, x=emofac, fill=powfac, data=pow, geom='boxplot')
qplot(y=sresp, x=emofac, fill=powfac, data=pow, geom='boxplot')
qplot(y=tpow, x=emofac, fill=powfac, data=pow, geom='boxplot')
qplot(y=opow, x=emofac, fill=powfac, data=pow, geom='boxplot')
qplot(y=noonepow, x=emofac, fill=powfac, data=pow, geom='boxplot')
qplot(y=sitpow, x=emofac, fill=powfac, data=pow, geom='boxplot')
qplot(y=immoral, x=emofac, fill=powfac, data=pow, geom='boxplot')
