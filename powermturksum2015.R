##### Power MTurk Summer 2015 Syntax #####

##### 1. Variable List #####
# SubjectID: Subject identification number
# IPAddress: Subject's IP address
# empty: indicates that the subject's data are empty (someone who didn't complete any of the study)
# duplicate: duplicate IP address, excluded to make sure it's not the same person or people in the same household who talked to each other about the study
# StartDate, EndDate: Start and end time of study participation
# testruns: Not real subjects, test runs from the resarch team
# powcond: 1 = high power, -1 = low power
# emptyappraisals: No appraisal data, from a mistake in an earlier version of the Qualtrics survey where only the power prime was displayed
# prime_prob: There was a problem with the way the subject completed the prime (or the subject didn't complete the prime)
# hp_exp: The text response for the high power prime
# lp_exp: The text response for the low power prime
# emocond: 1 = angry, -1 = sad
# 
# Self-reports of emotion (slider scales, 0-100)
# happy, symp=sympathetic, angry, comp=compassionate, sad, interest=interested, mad, anxious, proud, down, worried, frustrated
#
# Perceptions of other's emotion (slider scales, 0-100)
# ohappy, oangry, osad, ointerest=interested, omad, oanxious, oproud, odown, oworried, ofrust = frustrated
#
# Appraisals of other's situation (slider scales, 0 = not at all, 100 = extremely)
# pleas: How pleasant was it?
# tresp: How much do you feel Pat2195 was responsible for what happened?
# oresp: How much do you feel that someone aside from Pat2195 was reponsible for what happened?
# sresp: How much do you feel that circumstances beyond anyone's control are responsible for what happened to Pat2195?
# tpow: How much do you feel that Pat2195 had the power to do something about the situation?
# opow: How much do you feel that someone aside from Pat2195 had the power to do something about the situation?
# noonepow: How much do you feel that no one had the power to do anything about the situation?
# sitpow: How much do you feel that the situation was out of anyone's control?
# immoral: To what extent do you feel what happened was morally wrong?
#
# age: subject's age
# gender: open-ended response to a question about gender
# female: factor with levels - female, male, transman
# income: Annual household income; 1 = less than 20k, 2 = 20-39k, 3 = 40-59k, 4 = 60-79k, 5 = 90-99k 6 = 100k or more
# education: Highest level of education completed; 1 = less than high school, 2 = high school/GED, 3 = some college, 4 = associate's degree, 5 = bachelor's degree/4 year college, 5 = postgraduate degree (e.g., MA/MS, MD, JD, PhD)
#
# Race/ethnicity
# black: African American/Black
# mideast: Arab/Middle Eastern
# asian: East Asian/East Asian American
# white: European American/White
# latino: Hispanic/Latino/Latino American
# natam: Native American/Alaska Native
# pacisl: Native Hawaiian/Pacific Islander
# other: Other (pleas specify below)
# other_specify: response to specification of race/ethnicity
#
# other_tell: Is there anything that you want to tell us about this HIT?
# sim_hit: Have you completed any other HITs that were similar to this one in particular?
# comments: Something in the comments gives reason to exclude the subject's data, e.g., they've completed the power prime before
# suspicion: Subject suspected that the other person's letter wasn't real


##### 2. Import Data, Load Packages, Load Functions #####

powturk <- read.csv('powR2.csv', header=TRUE)
powturk <- powturk[is.na(powturk$powcond)==FALSE & is.na(powturk$emocond)==FALSE,]
powturk <- powturk[powturk$empty!=1 & powturk$duplicate!=1 & powturk$testruns!=1 & powturk$emptyappraisals!=1 & powturk$prime_prob!=1 & powturk$comments!=1 & powturk$suspicion!=1,]

library(ggplot2)
library(car)

source('../R functions/tcontrast.R')
source('../R functions/mean-plot.R')





##### 3. Recode Data #####

powturk$powfac <- factor(powturk$powcond, levels=c(-1,1), labels=c('low power','high power'))
powturk$emofac <- factor(powturk$emocond, levels=c(-1,1), labels=c('sad', 'angry')) 

table(powturk$powfac, powturk$emofac)

powturk$group <- with(powturk, ifelse(powcond==-1 & emocond==-1, 1, ifelse(powcond==-1 & emocond==1, 2, ifelse(powcond==1 & emocond==-1, 3, ifelse(powcond==1 & emocond==1, 4, NA))))) # 1 = low power/sad, 2 = low power/angry, 3 = high power/sad, 4 = high power/angry






##### 4. Analyze Emotion Data #####

#examine possible covariates
powturk$female.numb <- ifelse(powturk$female=='female',1,ifelse(powturk$female=='male',-1,NA))

with(powturk[powturk$female!='transman' & powturk$emocond==-1,], round(cor(cbind(income, education, female.numb, sad, down, comp, symp, angry, mad), use='complete.obs'), digits=2))
with(powturk[powturk$female!='transman' & powturk$emocond==1,], round(cor(cbind(income, education, female.numb, sad, down, comp, symp, angry, mad), use='complete.obs'), digits=2))


# Examine correlations among emotions
round(cor(powturk[14:25], use='complete.obs'), digits=2)

## Center condition
powturk$powcond.cent <- powturk$powcond-mean(powturk$powcond, na.rm=TRUE)
powturk$emocond.cent <- powturk$emocond-mean(powturk$emocond, na.rm=TRUE)

# Analyze sadness
qplot(y=sad, x=emofac, fill=powfac, data=powturk, geom='boxplot')
with(powturk, mean.plot(sad, f1=emofac, f2=powfac, ylab='Sadness', ylim=c(0,100)))
summary(lm(sad ~ powcond.cent*emocond.cent, data=powturk))
with(powturk, t.contrast(sad, group, c(1,0,-1,0)))

qplot(y=down, x=emofac, fill=powfac, data=powturk, geom='boxplot')
with(powturk, mean.plot(down, f1=emofac, f2=powfac, ylab='Sadness', ylim=c(0,100)))

qplot(y=(sad+down)/2, x=emofac, fill=powfac, data=powturk, geom='boxplot')
with(powturk, mean.plot((sad))

summary(lm((sad+down)/2 ~ powcond*emocond, data=powturk))

# low pow/sad, low pow/angry, high pow/sad, high pow/angry
with(powturk, t.contrast((sad)/2, group, c(1,0,-1,0)))
with(powturk, t.contrast((sad)/2, group, c(0,1,0,-1)))
with(powturk, t.contrast((sad)/2, group, c(1,1,-1,-1)))

with(powturk, t.contrast((sad+down)/2, group, c(1,0,-1,0)))
with(powturk, t.contrast((sad+down)/2, group, c(0,1,0,-1)))
with(powturk, t.contrast((sad+down)/2, group, c(1,1,-1,-1)))

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
