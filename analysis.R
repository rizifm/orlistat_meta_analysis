# load required libraries ----
library(readxl)
library(metafor)
library(tidyverse)

# load the dataset ----
dat <- read_excel('orlistat_bmi_extractions.xlsx', sheet = 1)

# explore the main effects ----
# how many studies?
dim(dat)

# what variables are included
str(dat)

# What is the distribution of placebo and drug effects?
hist(dat$m1i)
hist(dat$m2i)

# Are the effects correlated within studies?
cor(dat$m1i, dat$m2i)
cor.test(dat$m1i, dat$m2i)

# What is the difference in change-score between the groups?
hist(dat$m2i - dat$m1i)

# How big is the uncertainty in the measures?
summary(dat$sd1i)
summary(dat$sd2i)

# Does uncertainty correlate with sample size?
plot(dat$n1i, dat$sd1i)
plot(dat$n2i, dat$sd2i)

# check all at once
pairs(dat[, 5:10])

# explore the study variables ----
# the dose
boxplot((dat$m2i-dat$m1i) ~ dat$dose)

# the age
boxplot((dat$m2i-dat$m1i) ~ dat$age)

# the comorbidities
boxplot((dat$m2i-dat$m1i) ~ dat$comorbidities)

# calculating the effect size ----
# raw mean difference
dat2 <- escalc("MD", 
               data = dat,
               m1i = m1i, sd1i = sd1i, n1i = n1i, 
               m2i = m2i, sd2i = sd2i, n2i = n2i,
               append = TRUE,
               slab = paste(author, year, sep = ', '))

setdiff(names(dat2), names(dat))

hist(dat2$yi)
summary(dat2$vi)

plot(dat2$yi, dat2$vi)

# random effects model ----
bmi.m1 <- rma(yi, vi, data = dat2)
bmi.m1

forest(bmi.m1)

# png(filename = 'figures/forest.png', width = 7, height = 6, units = 'in', res = 300)
# par(mar = c(4.5,4,0,2))
# forest(bmi.m1,
#        xlab = 'Reduction in BMI (MD)')
# text(15.5, 18, "Mean Difference (MD) [5%, 95% CI]", pos = 2, font = 2, cex = .9)
# dev.off()

# mixed effects ----
# dose and duration
dat2$dose <- as.factor(dat2$dose)
dat2$comorbidities <- as.factor(dat2$comorbidities)

bmi.m2 <- rma(yi, vi,
              data = dat2,
              mods = ~ dose)
bmi.m2

regplot(bmi.m2, mod = 2)

# remove the two studies with dose 60
dat3 <- dat2[dat2$dose == '120',]
dim(dat3)

# age and comorbidities
bmi.m3 <- rma(yi, vi,
              data = dat3,
              mods = ~duration+age+comorbidities)
bmi.m3

regplot(bmi.m3, mod = 2)
regplot(bmi.m3, mod = 3)
regplot(bmi.m3, mod = 4)
# 
# png('figures/interventions.png', width = 7, height = 3.3, units = 'in', res = 300)
# par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))
# # dose
# set.seed(123)
# stripchart(dat2$yi ~ dat2$dose,
#            vertical = TRUE,
#            method = 'jitter',
#            jitter = .2,
#            pch = 1,
#            cex = dat2$vi + 1,
#            lwd = 2,
#            xlim = c(.5,2.5),
#            xlab = 'Dose (mg/day)',
#            ylab = 'Reduction in BMI',
#            main = 'Estimate = .5, P = .9')
# 
# dose.ave <- aggregate(dat2$yi, by = list(dose = dat2$dose), mean)
# dose.sd <- aggregate(dat2$yi, by = list(dose = dat2$dose), sd)
# 
# points(c(1, 2),
#        dose.ave$x,
#        pch = 19,
#        col = 'red')
# 
# segments(c(1,2), dose.ave$x - dose.sd$x, y1 = dose.ave$x + dose.sd$x,
#          col = 'red',
#          lwd = 1.5)
# mtext('A', at = -.2, line = 1.2, adj = 1)
# # duration
# plot(dat3$duration, dat3$yi,
#      pch = 1,
#      cex = dat2$vi + 1,
#      lwd = 2,
#      xlab = 'Duration (month)',
#      ylab = 'Reduction in BMI',
#      main = 'Estimate = .13, P = .01')
# bmi.m3
# abline(bmi.m3$b[1], bmi.m3$b[2],
#        lty = 2,
#        lwd = 2,
#        col = 'red')
# mtext('B', at = -4, line = 1.2, adj = 1)
# 
# dev.off()
# 
# png('figures/study_variables.png', width = 7, height = 3.3, units = 'in', res = 300)
# par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))
# 
# # age
# set.seed(123)
# stripchart(dat3$yi ~ dat3$age,
#            vertical = TRUE,
#            method = 'jitter',
#            jitter = .2,
#            pch = 1,
#            cex = dat2$vi + 1,
#            lwd = 2,
#            xlim = c(.5,2.5),
#            xlab = 'Age Group',
#            ylab = 'Reduction in BMI',
#            main = 'Estimate = .3, P = .5')
# bmi.m3
# age.ave <- aggregate(dat3$yi, by = list(age.ave = dat3$age), mean)
# age.sd <- aggregate(dat3$yi, by = list(age = dat3$age), sd)
# 
# points(c(1, 2),
#        age.ave$x,
#        pch = 19,
#        col = 'red')
# 
# segments(c(1,2), age.ave$x - age.sd$x, y1 = age.ave$x + age.sd$x,
#          col = 'red',
#          lwd = 1.5)
# mtext('A', at = -.2, line = 1.2, adj = 1)
# 
# # comorbidities
# set.seed(123)
# x <- c('Without', 'With')[dat3$comorbidities]
# stripchart(dat3$yi ~ x,
#            vertical = TRUE,
#            method = 'jitter',
#            jitter = .2,
#            pch = 1,
#            cex = dat2$vi + 1,
#            lwd = 2,
#            xlim = c(.5,2.5),
#            xlab = 'Co-morbidities',
#            ylab = 'Reduction in BMI',
#            main = 'Estimate = -1.02, P = .03')
# 
# comorbid.ave <- aggregate(dat3$yi, by = list(comorbid = dat3$age), mean)
# comorbid.sd <- aggregate(dat3$yi, by = list(comorbid = dat3$age), sd)
# 
# points(c(2, 1),
#        comorbid.ave$x,
#        pch = 19,
#        col = 'red')
# 
# segments(c(2, 1), comorbid.ave$x - comorbid.sd$x, y1 = comorbid.ave$x + comorbid.sd$x,
#          col = 'red',
#          lwd = 1.5)
# 
# mtext('B', at = -.2, line = 1.2, adj = 1)
# 
# dev.off()

# publication bias ----
funnel(bmi.m3)

# sensitivity analysis ----
inf.m3 <- influence(bmi.m3)
plot(inf.m3$inf$tau2.del)

# png('figures/bias.png', width = 7, height = 3.3, units = 'in', res = 300)
# par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))
# 
# # publication bias
# res <- rstandard(bmi.m3)
# res.out <- res[res$resid > 2,]
# 
# plot(res$resid, res$se,
#      xlim = c(-2.6, 2.6), ylim = c(1, 0),
#      xlab = 'Residual Value', ylab = 'Standard Error',
#      lwd = 2,
#      main = paste(res.out$slab, "(SE > 2)"))
# 
# segments(-2, 1.1, 0, 0, lty = 2, col = 'red')
# segments(2, 1.1, 0, 0, lty = 2, col = 'red')
# segments(0, 1.1, 0,0, lty = 2, col = 'red')
# 
# points(res.out$resid, res.out$se, col = 'blue')
# 
# mtext('A', at = -4.5, line = 1.2, adj = 1)
# 
# # influence
# cooks <- inf.m3$inf$cook.d
# cooks.out <- inf.m3$inf[cooks > 2,]
# 
# ind <- order(cooks)
# plot(cooks[ind],
#      xlab = 'Study Index',
#      ylab = "Cook's Distance",
#      lwd = 2,
#      xaxt = 'n',
#      main = paste(cooks.out$slab, "(C > 2)"))
# 
# points(14, cooks.out$cook.d, col = 'blue', lwd = 2)
# 
# xseq <- rep(c(TRUE, FALSE), 6)
# axis(1, at = (1:nrow(dat3)), labels = rep('', nrow(dat3)), cex.axis = .4)
# 
# axis(1, at = (1:nrow(dat3))[xseq], labels = ind[xseq], cex.axis = .8, tick = FALSE, line = -.2)
# axis(1, at = (1:nrow(dat3))[!xseq], labels = ind[!xseq], cex.axis = .8, tick = FALSE, line = .8)
# 
# mtext('B', at = -4, line = 1.2, adj = 1)
# 
# dev.off()
