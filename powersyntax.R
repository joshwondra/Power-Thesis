##### Power Thesis Syntax #####


##### 1. Variable List #####

## this variable list is based on the wide form of the data, but it can be used to understand the long form of the data as well
## only the variables for the high power subject are listed, but the low power subject's variables can be accessed by changing the .hp to .lp

## Session information and condition
# dyadID: Dyad identification number
# emot_cond: 1 = target is sad, 2 = target is angry
# exper: experimenter's first name
# subjectID.hp: Subject identification number

## Now that you've finished reading the e-mail, what are your main feelings right now?
# mainfeel1.hp, mainfeel2.hp, mainfeel3.hp, mainfeel_exp.hp: open-ended own feelings and explanation

## After reading the e-mail, how much do you feel each of the following right now? 0 = not at all, 4 = extremely
# happy.hp
# symp.hp: sympathetic
# angry.hp
# compass.hp: compassionate
# interest.hp: interested
# disgust.hp: disgusted
# mad.hp
# anxious.hp
# proud.hp
# down.hp
# worried.hp
# frust.hp: frustrated
# sad.hp
# powerful.hp

## What were the main feelings of the person who wrote the e-mail?
# ofeel1.hp, ofeel2.hp, ofeel3.hp, ofeel_exp.hp: open-ended perceptions of the target's feelings

## How much did the person who wrote the e-mail feel each of the following? 0 = not at all, 4 = extremely
# thappy.hp
# tshock.hp: shocked
# thope.hp: hopeful
# tangry.hp
# tselfanger.hp: angry at self
# tinterest.hp: interested
# tdisgust.hp: disgusted
# tmad.hp
# tcurious.hp
# tafraid.hp
# tgrateful.hp
# tproud.hp
# tsurpr.hp: surprised
# tamused.hp
# tdown.hp
# tworried.hp
# tfrust.hp: frustrated
# tsad.hp
# tembarrass.hp: embarrassed
# tguilty.hp
# tconfused.hp
# tpowerful.hp

## Appraisals
## Answer the following questions about the situation that was described in the e-mail (0 = not at all, 4 = extremely)
# pleas.hp: How pleasant was it?
# targag.hp: How much do you feel that the person who wrote the e-mail was responsible for what happened?
# othag.hp: How much do you feel that someone aside from the person who wrote the e-mail was responsible for what happened?
# selfcirc.hp: How much do you feel that circumstances beyond anyone's control are responsible for what happened?
# targpow.hp: How much do you feel that the person who wrote the e-mail had the power to do something about the situation?
# othpow.hp: How much do you feel that someone aside from the person who wrote the e-mail had the power to do something about the situation?
# noonepow.hp: How much do you feel that no one had the power to do anything about the situation?
# sitcont.hp: How much do you feel that the situation was out of anyone's control?
# immoral.hp: To what extent do you feel what happened was morally wrong?
# comm.hp: How much do you feel like lack of communication played a role in the situation?

## Perceptions of Target's Appraisals
## Answer the following questions about what the person who wrote the e-mail thought about the situation that was described (0 = not at all, 4 = extremely)
# o_pleas.hp: How pleasant did they think it was?
# o_selfag.hp: How much did they feel responsible for what happened?
# o_otherag.hp: How much did they feel that someone else was responsible for what happened?
# o_sitag.hp: How much did they feel that circumstances beyond anyone's control were responsible for what happened?
# o_selfpow.hp: How much did they feel that they had the power to do something about the situation?
# o_otherpow.hp: How much did they feel that someone else had the power to do something about the situation?
# o_noonepow.hp: How much did they feel that no one had the power to do anything about the situation?
# o_sitpow.hp: How much did they feel that the situation was out of anyone's control?
# o_immoral.hp: To what extent does the other participant [sic] feel that what happened was morally wrong?
# o_comm.hp: How much did they feel like lack of communication played a role in the situation?

## Demographics
# ssc.hp: Subjective social class (ladder measure)
# female.hp: 0 = male, 1 = female
## race/ethnicity
# black.hp, asian.hp, latino.hp, white.hp, other.hp, mideast.hp, natam.hp, pacisl.hp
# other_secify.hp: Specify other race/ethnicity
#mom_ed.hp, dad_ed.hp: Parents' highest level of education completed; 1 = less than high school, 2 = high school diploma/GED, 3 = some college, 4 = associate's degree, 7 = bachelor's degree, 8 = postgraduate degree (e.g., Master's, PhD, JD, MD)

# expcomment.hp: Experimenter's comments
# othernotes.hp: other notes on the subject's data
# noshow.hp: partner did not show up to the session
# inattention.hp: subject was not attentive
# metearly.hp: subjects saw each other early
# suspicion.hp: subject suspected deception or guessed the purpose of the study
# misunderstood.hp: subject misunderstood or failed to follow directions
# other.hp: other possible problems
# kneweachother.hp: subjects knew each other from outside the lab
# minorproblem.hp: potential problems, but probably fine
# language.hp: subject is a non-native English speaker



##### 2. Import Data, Load Packages, Load Functions #####

pow <- read.csv('buswideform.csv')
powopen <- read.csv('busopenwideform.csv')

source('../R functions/mean-plot.R')
source('../R functions/nojitterbox.R')
source('../R functions/multiplot.R')
source('../R functions/proportion-plot.R')

library(lme4)


## exclusions

# closed-ended
# hp exclusions: 4 to 93
# lp exclusions: 94 to 183
pow <- pow[pow$noshow.hp==0 & pow$kneweachother.hp==0,]
pow[pow$inattention.hp!=0 | pow$suspicion.hp!=0 | pow$misunderstood.hp!=0 | pow$language.hp!=0, 4:93] <- NA
pow[pow$inattention.lp!=0 | pow$suspicion.lp!=0 | pow$misunderstood.lp!=0 | pow$language.lp!=0, 94:183] <- NA

# open-ended
# hp exclusions: 1 to 286
# lp exclusions: 287 to 572
powopen <- powopen[powopen$noshow.hp==0 & powopen$kneweachother.hp==0,]
powopen[powopen$inattention.hp!=0 | powopen$suspicion.hp!=0 | powopen$misunderstood.hp!=0 | powopen$language.hp!=0 | powopen$open_problem.hp!=0, 1:286] <- NA
powopen[powopen$inattention.lp!=0 | powopen$suspicion.lp!=0 | powopen$misunderstood.lp!=0 | powopen$language.lp!=0 | powopen$open_problem.lp!=0, 287:572] <- NA



##### 3. Own Emotions #####

## Open-Ended Data

group.emot <- function(...) {
    vars <- cbind(...)
    code <- (rowSums(vars)>0)*1
    print(table(code))
    return(code)
}

# high power
powopen$o_sad.hp <- with(powopen, group.emot(defeated.hp, despair.hp, disappointed.hp, disappointment.hp, sad.hp, sadness.hp, sorrow.hp, sorrowful.hp)) #24 cases
powopen$o_angry.hp <- with(powopen, group.emot(aggravated.hp, anger.hp, angry.hp, annoyance.hp, annoyed.hp, furious.hp, irritated.hp, mad.hp, provoked.hp)) #14 cases
powopen$o_sympathy.hp <- with(powopen, group.emot(care.hp, caring.hp, comforting.hp, compassion.hp, compassionate.hp, concern.hp, concerned.hp, empathetic.hp, empathy.hp, pity.hp, sorry.hp, sympathetic.hp, sympathy.hp, understanding.hp, want_to_help.hp, wanting_to_help.hp, willing_to_help.hp)) #66 cases
powopen$o_unpleas.hp <- with(powopen, group.emot(bad.hp, badly.hp, distressed.hp, stress.hp, stressed.hp, terrible.hp, troubled.hp, uneasy.hp, unhappy.hp, unsatisfied.hp, upset.hp)) #21 cases
powopen$o_afraid.hp <- with(powopen, group.emot(alarmed.hp, anxiety.hp, anxious.hp, cautious.hp, fear.hp, nervous.hp, nervousness.hp, panic.hp, worried.hp, worry.hp)) #21 cases
powopen$o_disbelief.hp <- with(powopen, group.emot(disbelief.hp, doubt.hp, hesitant_to_believe.hp, mistrust.hp, questioning.hp, skeptical.hp, skepticism.hp,suspicion.hp, unbelievable.hp)) #8 cases
powopen$o_frustration.hp <- with(powopen, group.emot(frustrated.hp, frustration.hp)) #16 cases
powopen$o_challenge.hp <- with(powopen, group.emot(determination.hp, determined.hp, eager.hp, eagerness.hp, focused.hp, motivated.hp, persistence.hp)) #8 cases
powopen$o_interested.hp <- with(powopen, group.emot(curiosity.hp, curious.hp, inquisitive.hp, interested.hp, intrigue.hp, intrigued.hp)) #8 cases
powopen$o_confused.hp <- with(powopen, group.emot(confused.hp, confusion.hp, puzzled.hp)) # 10 cases

# low power
powopen$o_sad.lp <- with(powopen, group.emot(defeated.lp, despair.lp, disappointed.lp, disappointment.lp, sad.lp, sadness.lp, sorrow.lp, sorrowful.lp)) #26 cases
powopen$o_angry.lp <- with(powopen, group.emot(aggravated.lp, anger.lp, angry.lp, annoyance.lp, annoyed.lp, furious.lp, irritated.lp, mad.lp, provoked.lp)) #21 cases
powopen$o_sympathy.lp <- with(powopen, group.emot(care.lp, caring.lp, comforting.lp, compassion.lp, compassionate.lp, concern.lp, concerned.lp, empathetic.lp, empathy.lp, pity.lp, sorry.lp, sympathetic.lp, sympathy.lp, understanding.lp, want_to_help.lp, wanting_to_help.lp, willing_to_help.lp)) #60 cases
powopen$o_unpleas.lp <- with(powopen, group.emot(bad.lp, badly.lp, distressed.lp, stress.lp, stressed.lp, terrible.lp, troubled.lp, uneasy.lp, unhappy.lp, unsatisfied.lp, upset.lp)) #22 cases
powopen$o_afraid.lp <- with(powopen, group.emot(alarmed.lp, anxiety.lp, anxious.lp, cautious.lp, fear.lp, nervous.lp, nervousness.lp, panic.lp, worried.lp, worry.lp)) #20 cases
powopen$o_disbelief.lp <- with(powopen, group.emot(disbelief.lp, doubt.lp, hesitant_to_believe.lp, mistrust.lp, questioning.lp, skeptical.lp, skepticism.lp,suspicion.lp, unbelievable.lp)) #4 cases
powopen$o_frustration.lp <- with(powopen, group.emot(frustrated.lp, frustration.lp)) #10 cases
powopen$o_challenge.lp <- with(powopen, group.emot(determination.lp, determined.lp, eager.lp, eagerness.lp, focused.lp, motivated.lp, persistence.lp)) #5 cases
powopen$o_interested.lp <- with(powopen, group.emot(curiosity.lp, curious.lp, inquisitive.lp, interested.lp, intrigue.lp, intrigued.lp)) #9 cases
powopen$o_confused.lp <- with(powopen, group.emot(confused.lp, confusion.lp, puzzled.lp)) # 13 cases


## plots of proportions
powopen.plots <- with(powopen, data.frame(pow.cond=factor(rep(c(1,-1), each=99), levels=c(-1,1), labels=c('low power','high power')), emot.cond=factor(c(emot_cond.hp, emot_cond.lp), levels=c(1,2), labels=c('sad','angry')), angry=c(o_angry.hp, o_angry.lp), sad=c(o_sad.hp, o_sad.lp), sympathy=c(o_sympathy.hp, o_sympathy.lp), frustration=c(o_frustration.hp, o_frustration.lp), unpleasant=c(o_unpleas.hp, o_unpleas.lp), afraid=c(o_afraid.hp, o_afraid.lp), disbelief=c(o_disbelief.hp, o_disbelief.lp), challenge=c(o_challenge.hp, o_challenge.lp), interest=c(o_interested.hp, o_interested.lp), confused=c(o_confused.hp, o_confused.lp)))

powopen.plots <- powopen.plots[is.na(powopen.plots$emot.cond)==FALSE,]

with(powopen, by(o_angry.hp, emot_cond.hp, function(x){sum(x, na.rm=TRUE)/length(x)}))
with(powopen, by(o_angry.lp, emot_cond.hp, function(x){sum(x, na.rm=TRUE)/length(x)}))
with(powopen.plots, by(angry, list(emot.cond, pow.cond), function(x){sum(x, na.rm=TRUE)/length(x)}))
with(powopen.plots, prop.plot(angry, emot.cond, pow.cond, ylab='Anger'))
with(powopen.plots, prop.plot(sad, emot.cond, pow.cond, ylab='Sad'))
with(powopen.plots, prop.plot(sympathy, emot.cond, pow.cond, ylab='Sympathy'))
with(powopen.plots, prop.plot(frustration, emot.cond, pow.cond, ylab='Frustration'))
with(powopen.plots, prop.plot(unpleasant, emot.cond, pow.cond, ylab='Unpleasant'))
with(powopen.plots, prop.plot(afraid, emot.cond, pow.cond, ylab='Afraid'))
with(powopen.plots, prop.plot(disbelief, emot.cond, pow.cond, ylab='Disbelief'))
with(powopen.plots, prop.plot(challenge, emot.cond, pow.cond, ylab='Challenge'))
with(powopen.plots, prop.plot(interest, emot.cond, pow.cond, ylab='Interested'))
with(powopen.plots, prop.plot(confused, emot.cond, pow.cond, ylab='Confused'))

powopen.concise <- powopen[,c('dyadID.hp','subjectID.hp','pt_cond.hp','emot_cond.hp','exper.hp','ssc.hp','age.hp','female.hp','black.hp','asian.hp','latino.hp','white.hp','other.hp','other_secify.hp','mideast.hp','natam.hp','pacisl.hp','mom_ed.hp','dad_ed.hp','o_sad.hp','o_angry.hp','o_sympathy.hp','dyadID.lp','subjectID.lp','pt_cond.lp','emot_cond.lp','exper.lp','ssc.lp','age.lp','female.lp','black.lp','asian.lp','latino.lp','white.lp','other.lp','other_secify.lp','mideast.lp','natam.lp','pacisl.lp','mom_ed.lp','dad_ed.lp','o_sad.lp','o_angry.lp','o_sympathy.lp')]

powopen.hp <- powopen.concise[,c(1:22)]
colnames(powopen.hp) <- c('DyadID','SubjectID','powcond','emocond','exper','ssc','age','female','black','asian','latino','white','other','other_specify','mideast','natam','pacisl','mom_ed','dad_ed','sad','angry','sympathetic')
powopen.lp <- powopen.concise[,c(23:44)]
colnames(powopen.lp) <- c('DyadID','SubjectID','powcond','emocond','exper','ssc','age','female','black','asian','latino','white','other','other_specify','mideast','natam','pacisl','mom_ed','dad_ed','sad','angry','sympathetic')
powopen.long <- rbind(powopen.hp,powopen.lp)
rownames(powopen.long) <- 1:198

powopen.long$powfac <- with(powopen.long, factor(powcond, levels=c(1,2), labels=c('high power','low power')))
powopen.long$emofac <- with(powopen.long, factor(emocond, levels=c(1,2), labels=c('sad','angry')))
save(powopen.long, file='powopenlong.RData')

with(powopen.long, by(sad,list(powfac, emofac),sum))
with(powopen.long, by(angry,list(powfac, emofac),sum))
with(powopen.long, by(sympathetic,list(powfac, emofac),sum))
with(powopen.long, table(powfac,emofac))

powopen.long$femcent <- powopen.long$female - mean(powopen.long$female, na.rm=TRUE)

m.osad <- glmer(sad ~ (1|DyadID) + emofac*powfac + femcent, data=powopen.long, family=binomial, control=glmerControl(optimizer='bobyqa'), nAGQ=10)
summary(m.osad)


## Closed-Ended Data

## Create long form for MLM
pow.hp <- pow[,c('dyadID','emot_cond','exper','subjectID.hp','symp.hp','angry.hp','compass.hp','mad.hp','down.hp','sad.hp','powerful.hp','pleas.hp','targag.hp','othag.hp','selfcirc.hp','targpow.hp','othpow.hp','noonepow.hp','sitcont.hp','immoral.hp','comm.hp','ssc.hp','age.hp','female.hp','black.hp','asian.hp','latino.hp','white.hp','other.hp','other_secify.hp','mideast.hp','natam.hp','pacisl.hp','mom_ed.hp','dad_ed.hp')]
pow.hp$powcond <- rep(1, length(pow.hp$dyadID))
colnames(pow.hp) <- c('dyadID', 'emocond', 'exper','SubjectID','symp','angry','compass','mad','down','sad','powerful','pleas','targag','othag','selfcirc','targpow','othpow','noonepow','sitcont','immoral','comm','ssc','age','female','black','asian','latino','white','other','other_specify','mideast','natam','pacisl','mom_ed','dad_ed','powcond')
pow.lp <- pow[,c('dyadID','emot_cond','exper','subjectID.lp','symp.lp','angry.lp','compass.lp','mad.lp','down.lp','sad.lp','powerful.lp','pleas.lp','targag.lp','othag.lp','selfcirc.lp','targpow.lp','othpow.lp','noonepow.lp','sitcont.lp','immoral.lp','comm.lp','ssc.lp','age.lp','female.lp','black.lp','asian.lp','latino.lp','white.lp','other.lp','other_secify.lp','mideast.lp','natam.lp','pacisl.lp','mom_ed.lp','dad_ed.lp')]
pow.lp$powcond <- rep(-1, length(pow.lp$dyadID))
colnames(pow.lp) <- c('dyadID', 'emocond', 'exper','SubjectID','symp','angry','compass','mad','down','sad','powerful','pleas','targag','othag','selfcirc','targpow','othpow','noonepow','sitcont','immoral','comm','ssc','age','female','black','asian','latino','white','other','other_specify','mideast','natam','pacisl','mom_ed','dad_ed','powcond')

powclose <- rbind(pow.hp,pow.lp)
powclose$emofac <- factor(powclose$emocond, levels=c(1,2), labels=c('sad','angry'))
powclose$powfac <- factor(powclose$powcond, levels=c(-1,1), labels=c('low power','high power'))
save(powclose, file='powcloselong.RData')

round(cor(pow[,9:22], use='complete.obs'), digits=2)
round(cor(pow[,99:112], use='complete.obs'), digits=2)

pow$angermean.hp <- rowMeans(pow[,c('angry.hp', 'mad.hp')])
pow$sadmean.hp <- rowMeans(pow[,c('sad.hp', 'down.hp')])
pow$sympmean.hp <- rowMeans(pow[,c('symp.hp', 'compass.hp')])


pow$angermean.lp <- rowMeans(pow[,c('angry.lp', 'mad.lp')])
pow$sadmean.lp <- rowMeans(pow[,c('sad.lp', 'down.lp')])
pow$sympmean.lp <- rowMeans(pow[,c('symp.lp', 'compass.lp')])

meanplot.data <- data.frame(powcond=factor(rep(c(1,-1), each=length(pow$dyadID)), levels=c(-1,1), labels=c('low power','high power')), emot.cond=factor(pow$emot_cond), anger=c(pow$angermean.hp, pow$angermean.lp), sadness=c(pow$sadmean.hp, pow$sadmean.lp), sympathy=c(pow$sympmean.hp, pow$sympmean.lp), angry=c(pow$angry.hp, pow$angry.lp), mad=c(pow$mad.hp, pow$mad.lp), sad=c(pow$sad.hp, pow$sad.lp), down=c(pow$down.hp, pow$down.lp), symp=c(pow$symp.hp, pow$symp.lp), comp=c(pow$compass.hp, pow$compass.lp))

#correctly scale responses
meanplot.data$anger <- meanplot.data$anger-1 
meanplot.data$sadness <- meanplot.data$sadness-1 
meanplot.data$sympathy <- meanplot.data$sympathy-1 
meanplot.data$angry <- meanplot.data$angry-1
meanplot.data$mad <- meanplot.data$mad-1
meanplot.data$sad <- meanplot.data$sad-1
meanplot.data$down <- meanplot.data$down-1
meanplot.data$symp <- meanplot.data$symp-1
meanplot.data$comp <- meanplot.data$comp-1


with(meanplot.data, mean.plot(anger, emot.cond, powcond, ylab='Anger', ylim=c(0,4)))
with(meanplot.data, mean.plot(sadness, emot.cond, powcond, ylab='Sadness', ylim=c(0,4)))
with(meanplot.data, mean.plot(sympathy, emot.cond, powcond, ylab='Sympathy', ylim=c(0,4)))

with(meanplot.data, mean.plot(angry, emot.cond, powcond, ylab='Angry', ylim=c(0,4)))
with(meanplot.data, mean.plot(mad, emot.cond, powcond, ylab='Mad', ylim=c(0,4)))
with(meanplot.data, mean.plot(sad, emot.cond, powcond, ylab='Sad', ylim=c(0,4)))
with(meanplot.data, mean.plot(down, emot.cond, powcond, ylab='Down', ylim=c(0,4)))
with(meanplot.data, mean.plot(symp, emot.cond, powcond, ylab='Sympathy', ylim=c(0,4)))
with(meanplot.data, mean.plot(comp, emot.cond, powcond, ylab='Compassion', ylim=c(0,4)))

with(meanplot.data, nojitterbox(anger, emot.cond, powcond, ylab='Anger'))
with(meanplot.data, nojitterbox(sadness, emot.cond, powcond, ylab='Sadness'))
with(meanplot.data, nojitterbox(sympathy, emot.cond, powcond, ylab='Sympathy'))
with(meanplot.data, nojitterbox(comp, emot.cond, powcond, ylab='Sympathy'))
with(meanplot.data, nojitterbox(symp, emot.cond, powcond, ylab='Sympathy'))
with(meanplot.data, nojitterbox(sad, emot.cond, powcond, ylab='Sympathy'))
with(meanplot.data, nojitterbox(down, emot.cond, powcond, ylab='Sympathy'))

pow$emot_cond <- factor(pow$emot_cond, levels=c(1,2), labels=c('sadness','anger'))
ggplot(data=pow, aes(y=angermean.hp-angermean.lp, x=factor(emot_cond))) + geom_boxplot()
ggplot(data=pow, aes(y=sadmean.hp-sadmean.lp, x=factor(emot_cond))) + geom_boxplot()
ggplot(data=pow, aes(y=sympmean.hp-sympmean.lp, x=factor(emot_cond))) + geom_boxplot()

ggplot(data=pow, aes(y=angry.hp-angry.lp, x=factor(emot_cond))) + geom_boxplot()
ggplot(data=pow, aes(y=mad.hp-mad.lp, x=factor(emot_cond))) + geom_boxplot()
ggplot(data=pow, aes(y=sad.hp-sad.lp, x=factor(emot_cond))) + geom_boxplot()
ggplot(data=pow, aes(y=down.hp-down.lp, x=factor(emot_cond))) + geom_boxplot()

with(pow, plot(jitter(angermean.hp), jitter(othag.hp)))
pow$othagmean.hp <- pow$othag.hp-mean(pow$othag.hp, na.rm=TRUE)
pow$othagsq <- pow$othagmean.hp^2
pow$othagcu <- pow$othagmean.hp^3
fit1 <- lm(angermean.hp ~ othagmean.hp, data=pow)
summary(fit1)
qqnorm(residuals(fit1));qqline(residuals(fit1))
fit2 <- lm(angermean.hp ~ othagmean.hp+othagsq+othagcu, data=pow)
summary(fit2)
qqnorm(residuals(fit2));qqline(residuals(fit2))
fit3 <- lm(angermean.hp ~ othagcu, data=pow)
summary(fit3)
qqnorm(residuals(fit3));qqline(residuals(fit3))



##### 4. Own Appraisals #####
library(psych)
set.seed(81715)
fa.parallel(powclose[,c('pleas','immoral','comm','targag','targpow','othag','othpow','selfcirc','noonepow','sitcont')], n.iter=10000)
fa(powclose[,c('pleas','immoral','comm','targag','targpow','othag','othpow','selfcirc','noonepow','sitcont')], nfactors=4, rotate='promax', fm='ml') #also tested 3 and 2 factors; in general, pleasantness doesn't load on anything (probably due to restricted range); immorality and communication go with the other-agency appraisals; other agency and other power load together; target agency and target power load together; no one power and situational control load together; situational agency loads more with other-agency and power than with situational control/no one power

names(powclose)
ap.cor.data <- with(powclose, data.frame(emofac, powfac, pleas=jitter(pleas), immoral=jitter(immoral), comm=jitter(comm), targag=jitter(targag), targpow=jitter(targpow), othag=jitter(othag), othpow=jitter(othpow), noonepow=jitter(noonepow), sitag=jitter(selfcirc), sitcont=jitter(sitcont)))
ggpairs(data=ap.cor.data, c(3:5), color='emofac', shape='powfac')
ggpairs(data=ap.cor.data, c(6:12), color='emofac', shape='powfac')


library(GGally)
ggpairs(data=powclose, c(12:14), color='emofac', shape='powfac', lower=list(continuous='points'))
round(cor(pow[,49:58], use='complete.obs'), digits=2)
round(cor(pow[,59:68], use='complete.obs'), digits=2)

round(cor(powclose[,12:21], use='complete.obs'), digits=2)

pow$blamevic.hp <- rowMeans(pow[,c('targag.hp','targpow.hp')])
pow$blameoth.hp <- rowMeans(pow[,c('othag.hp','othpow.hp')])
pow$blamesit.hp <- rowMeans(pow[,c('selfcirc.hp','noonepow.hp','sitcont.hp')])
pow$blamevic.lp <- rowMeans(pow[,c('targag.lp','targpow.lp')])
pow$blameoth.lp <- rowMeans(pow[,c('othag.lp','othpow.lp')])
pow$blamesit.lp <- rowMeans(pow[,c('selfcirc.lp','noonepow.lp','sitcont.lp')])

# rescale
meanapp.data <- data.frame(powcond=factor(rep(c(1,-1), each=length(pow$dyadID)), levels=c(-1,1), labels=c('low power','high power')), emot.cond=pow$emot_cond, pleas=c(pow$pleas.hp, pow$pleas.lp), blamevic=c(pow$blamevic.hp, pow$blamevic.lp), blameoth=c(pow$blameoth.hp, pow$blameoth.lp), blamesit=c(pow$blamesit.hp, pow$blamesit.lp), immoral=c(pow$immoral.hp, pow$immoral.lp), comm=c(pow$comm.hp, pow$comm.lp), targag=c(pow$targag.hp,pow$targag.lp), targpow=c(pow$targpow.hp, pow$targpow.lp))

meanapp.data$pleas <- meanapp.data$pleas-1
meanapp.data$blamevic <- meanapp.data$blamevic-1
meanapp.data$blameoth <- meanapp.data$blameoth-1
meanapp.data$blamesit <- meanapp.data$blamesit-1
meanapp.data$immoral <- meanapp.data$immoral-1
meanapp.data$comm <- meanapp.data$comm-1

with(meanapp.data, mean.plot(pleas, emot.cond, powcond, ylab='Pleasantness', ylim=c(0,4)))
with(meanapp.data, mean.plot(blamevic, emot.cond, powcond, ylab='Blame Target', ylim=c(0,4)))
with(meanapp.data, mean.plot(blamesit, emot.cond, powcond, ylab='Blame Situation', ylim=c(0,4)))
with(meanapp.data, mean.plot(blameoth, emot.cond, powcond, ylab='Blame Other', ylim=c(0,4)))
with(meanapp.data, mean.plot(immoral, emot.cond, powcond, ylab='Immoral', ylim=c(0,4)))
with(meanapp.data, mean.plot(comm, emot.cond, powcond, ylab='Communication Problem', ylim=c(0,4)))

with(meanapp.data, mean.plot(targag, emot.cond, powcond, ylab='Target Agency', ylim=c(0,4)))
with(meanapp.data, mean.plot(targpow, emot.cond, powcond, ylab='Target Power', ylim=c(0,4)))

with(meanapp.data, nojitterbox(pleas, emot.cond, powcond, ylab='Pleasantness'))
with(meanapp.data, nojitterbox(blamevic, emot.cond, powcond, ylab='Blame Target'))
with(meanapp.data, nojitterbox(blamesit, emot.cond, powcond, ylab='Blame Situation'))
with(meanapp.data, nojitterbox(blameoth, emot.cond, powcond, ylab='Blame Other'))
with(meanapp.data, nojitterbox(immoral, emot.cond, powcond, ylab='Immoral'))
with(meanapp.data, nojitterbox(comm, emot.cond, powcond, ylab='Communication Problem'))



##### Plots for ISRE presentation #####

pow$angerdummy <- ifelse(pow$emot_cond=='sadness',0,1)
## test
ifelse(!is.na(pow$minorproblem.hp) & pow$minorproblem.hp==1, pow, pow[])
if(is.na(pow$minorproblem.hp)==FALSE){pow[pow$minorproblem.hp==1, 4:93] <- NA}
pow[pow$minorproblem.lp==1, 94:183] <- NA

#sadness
with(meanplot.data, mean.plot(sadness, emot.cond, powcond, ylab='Sadness', ylim=c(0,4))) + xlab("Other's Emotion") + guides(fill=guide_legend(title='Power Condition'))
pow$sad.sum <- with(pow, sadmean.hp+sadmean.lp)
pow$sad.diff <- with(pow, sadmean.hp-sadmean.lp)
m.sad.sum <- lm(sad.sum ~ angerdummy, data=pow)
summary(m.sad.sum); confint(m.sad.sum)
m.sad.diff <- lm(sad.diff ~ angerdummy, data=pow)
summary(m.sad.diff); confint(m.sad.diff)
summary(lm(sad.diff ~ 1, data=pow))

## exploratory analyses
pow$ssc.hp.cent <- pow$ssc.hp-mean(pow$ssc.hp, na.rm=TRUE)
pow$ssc.lp.cent <- pow$ssc.lp-mean(pow$ssc.lp, na.rm=TRUE)
pow$emocent <- pow$angerdummy-mean(pow$angerdummy, na.rm=TRUE)
summary(lm(sadmean.hp ~ ssc.hp.cent*emocent, data=pow))
summary(lm(sadmean.lp ~ ssc.lp.cent*emocent, data=pow))

pow$momedcent.hp <- pow$mom_ed.hp-mean(pow$mom_ed.hp, na.rm=TRUE)
pow$dadedcent.hp <- pow$dad_ed.hp-mean(pow$dad_ed.hp, na.rm=TRUE)
pow$momedcent.lp <- pow$mom_ed.lp-mean(pow$mom_ed.lp, na.rm=TRUE)
pow$dadedcent.lp <- pow$dad_ed.lp-mean(pow$dad_ed.lp, na.rm=TRUE)
summary(lm(sadmean.hp ~ momedcent.hp*emocent, data=pow))
summary(lm(sadmean.hp ~ dadedcent.hp*emocent, data=pow))
summary(lm(sadmean.lp ~ momedcent.lp*emocent, data=pow))
summary(lm(sadmean.lp ~ dadedcent.lp*emocent, data=pow))



with(meanplot.data, mean.plot(anger, emot.cond, powcond, ylab='Anger', ylim=c(0,4))) + xlab("Other's Emotion") + guides(fill=guide_legend(title='Power Condition'))
pow$anger.sum <- with(pow, angermean.hp+angermean.lp)
pow$anger.diff <- with(pow, angermean.hp-angermean.lp)
m.anger.sum <- lm(anger.sum ~ angerdummy, data=pow)
summary(m.anger.sum);confint(m.anger.sum)

with(meanplot.data, mean.plot(sympathy, emot.cond, powcond, ylab='Sympathy', ylim=c(0,4))) + xlab("Target's Emotion") + guides(fill=guide_legend(title='Power Condition'))

with(meanapp.data, mean.plot(blamevic, emot.cond, powcond, ylab='Blame Victim', ylim=c(0,4))) + xlab("Target's Emotion") + guides(fill=guide_legend(title='Power Condition'))

with(meanapp.data, mean.plot(blamesit, emot.cond, powcond, ylab='Blame Situation', ylim=c(0,4))) + xlab("Target's Emotion") + guides(fill=guide_legend(title='Power Condition'))

with(meanapp.data, mean.plot(blameoth, emot.cond, powcond, ylab='Blame Other', ylim=c(0,4))) + xlab("Target's Emotion") + guides(fill=guide_legend(title='Power Condition'))

with(meanapp.data, mean.plot(pleas, emot.cond, powcond, ylab='Blame Other', ylim=c(0,4))) + xlab("Target's Emotion") + guides(fill=guide_legend(title='Power Condition'))

with(meanapp.data, mean.plot(immoral, emot.cond, powcond, ylab='Blame Other', ylim=c(0,4))) + xlab("Target's Emotion") + guides(fill=guide_legend(title='Power Condition'))

powmlm <- read.csv('bus.csv', header=TRUE)
powmlm <- powmlm[powmlm$noshow==0 & powmlm$inattention==0 & powmlm$suspicion==0 & powmlm$misunderstood==0 & powmlm$kneweachother==0 & powmlm$language==0,]

### MLM attempt
library(foreign)
library(lme4)
library(arm)

powmlm$angermean <- rowMeans(data.frame(powmlm$angry, powmlm$mad))
powmlm$sadmean <- rowMeans(data.frame(powmlm$sad, powmlm$down))
powmlm$sympmean <- rowMeans(data.frame(powmlm$symp, powmlm$compass))

## create contrasts
hp lp
sadd angry
powmlm$angry.cont <- with(powmlm, ifelse(emot_cond==1, -1, ifelse(emot_cond==2, 1, NA)))
powmlm$hp.angry <- with(powmlm, ifelse(emot_cond==2 & pt_cond==1, 1, ifelse(emot_cond==2 & pt_cond==2, -1, 0)))
powmlm$hp.sad <- with(powmlm, ifelse(emot_cond==1 & pt_cond==1, 1, ifelse(emot_cond==1 & pt_cond==2, -1, 0)))

powmlm$emot.cent <- (powmlm$emot_cond-1.5)*2
powmlm$pow.cent <- (powmlm$pt_cond-1.5)*2
powmlm$ssc.cent <- with(powmlm, ssc-mean(ssc, na.rm=TRUE))

m1 <- lmer(sadmean ~ emot.cent*pow.cent + (1 | dyadID), data=powmlm)
display(m1)
coef(m1)
confint.merMod(m1, method='boot', nsim=5000)

m2 <- lmer(angermean ~ emot.cent*pow.cent + (1 | dyadID), data=powmlm)
display(m2)
coef(m2)
confint.merMod(m2, method='boot', nsim=5000)

powmlm$blamevic <- rowMeans(powmlm[,c('targag','targpow')])
powmlm$blameoth <- rowMeans(powmlm[,c('othag','othpow')])
powmlm$blamesit <- rowMeans(powmlm[,c('selfcirc','noonepow','sitcont')])

m3 <- lmer(blamevic ~ emot.cent*pow.cent + (1 | dyadID), data=powmlm)
display(m3)
coef(m3)
confint.merMod(m3, method='boot', nsim=5000)

m5 <- lmer(blamevic ~ angry.cont + hp.sad + hp.angry + (1 | dyadID), data=powmlm)
display(m3)
coef(m3)
confint.merMod(m5, method='boot', nsim=5000)

m4 <- lmer(blamesit ~ emot.cent*pow.cent + (1 | dyadID), data=powmlm)
display(m4)
coef(m4)
confint.merMod(m4, method='boot', nsim=5000)

m6 <- lmer(blamesit ~ angry.cont + hp.sad + hp.angry + (1 | dyadID), data=powmlm)
display(m6)
coef(m6)
confint.merMod(m6, method='boot', nsim=5000)


m7 <- lmer(sadmean ~ angry.cont + hp.sad + hp.angry + (1 | dyadID), data=powmlm)
display(m6)
coef(m6)
confint.merMod(m7, method='boot', nsim=5000)

m8 <- lmer(angermean ~ angry.cont + hp.sad + hp.angry + (1 | dyadID), data=powmlm)
confint.merMod(m8, method='boot', nsim=5000)
