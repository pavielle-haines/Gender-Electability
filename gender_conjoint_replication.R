#####LOAD LIBRARIES#####

rm(list=ls())
library(car)
library(ggplot2)
library(reshape)
library(survey)
library(gplots)
library(MASS)
library(clusterSEs)
library(Zelig)
library(survey)
library(weights)
library(pBrackets)
library(plotrix)



#####IMPORT DATA FILES#####

setwd("~/Dropbox/Academic Papers/Strategic Partisans/Data") #Set working directory

stacked <- read.csv("Final Stacked Partisan Strategy Data 12-28-18.csv") #Read in stacked dataset
head(stacked)
nrow(stacked)

clean <- read.csv("Final Clean Partisan Strategy Data 12-28-18.csv")
head(clean)
nrow(clean)



#####CREATE VARIABLES AND SUBSETS#####

stacked$candideolorder <- ifelse(stacked$candideology == 2, stacked$candideology - 2, stacked$candideology) #Create a variable that establishes "Moderately liberal" rather than "Slightly liberal" as the candidate baseline
stacked$candideolorder <- ifelse(stacked$candideolorder == 3, stacked$candideolorder - 1, stacked$candideolorder)
table(stacked$candideolorder) #0 is moderately liberal, 1 is slightly, 2 is very



#####MANIPULATION CHECK (FIGURE 2)#####

clean.allc <- subset(clean, treatment == 0) #Subset clean dataset by treatment and gender
clean.allt <- subset(clean, treatment == 1)

clean.womenc <- subset(clean.allc, female == 1)
clean.woment <- subset(clean.allt, female == 1)

clean.menc <- subset(clean.allc, female == 0)
clean.ment <- subset(clean.allt, female == 0)



allc.mean <- mean(clean.allc$clintonid) #Get means and standard errors
allc.se <- sd(clean.allc$clintonid)/sqrt(length(clean.allc$clintonid))

allt.mean <- mean(clean.allt$clintonid)
allt.se <- sd(clean.allt$clintonid)/sqrt(length(clean.allt$clintonid))

womenc.mean <- mean(clean.womenc$clintonid)
womenc.se <- sd(clean.womenc$clintonid)/sqrt(length(clean.womenc$clintonid))

woment.mean <- mean(clean.woment$clintonid)
woment.se <- sd(clean.woment$clintonid)/sqrt(length(clean.woment$clintonid))

menc.mean <- mean(clean.menc$clintonid)
menc.se <- sd(clean.menc$clintonid)/sqrt(length(clean.menc$clintonid))

ment.mean <- mean(clean.ment$clintonid)
ment.se <- sd(clean.ment$clintonid)/sqrt(length(clean.ment$clintonid))



treatment.test1 <- t.test(clean.allc$clintonid, clean.allt$clintonid) #Conduct t-test for treatment effect
print(treatment.test1)

treatment.test2 <- t.test(clean.womenc$clintonid, clean.woment$clintonid)
print(treatment.test2)

treatment.test3 <- t.test(clean.menc$clintonid, clean.ment$clintonid)
print(treatment.test3)



means <- c(allc.mean, allt.mean, menc.mean, ment.mean, womenc.mean, woment.mean)*100

se.lower <- means - c(allc.se, allt.se, menc.se, ment.se,  womenc.se, woment.se)*100
se.upper <- means + c(allc.se, allt.se, menc.se, ment.se, womenc.se, woment.se)*100


#Set dimensions to 6.5x4.75
par(mar = c(6, 3.5, 1, .25))
par(family = "serif")
x <- barplot(means, col = c("grey80", "grey40"), space = c(0, 0, 1, 0, 1, 0), ylim = c(0, 40), cex.axis = .9)
axis(1, at = c(-.5, 9))
text(c(1, 4, 7), -2, c("All Respondents", "Men", "Women"), cex = .95, xpd = NA)
text(-1.20, 20, "Percentage", cex = .9, srt = 90, xpd = NA)
legend(-.35, -6.15, fill = c("grey80", "grey40"), c("Without Identity Politics Loss Narrative  ", "With Identity Politics Loss Narrative"), cex = .95, y.intersp = 1.1,  xpd = NA)
lines(c(x[1], x[1]), c(se.lower[1], se.upper[1]))
lines(c(x[1]-.05, x[1]+.05), c(se.lower[1], se.lower[1]))
lines(c(x[1]-.05, x[1]+.05), c(se.upper[1], se.upper[1]))
lines(c(x[2], x[2]), c(se.lower[2], se.upper[2]))
lines(c(x[2]-.05, x[2]+.05), c(se.lower[2], se.lower[2]))
lines(c(x[2]-.05, x[2]+.05), c(se.upper[2], se.upper[2]))
lines(c(x[3], x[3]), c(se.lower[3], se.upper[3]))
lines(c(x[3]-.05, x[3]+.05), c(se.lower[3], se.lower[3]))
lines(c(x[3]-.05, x[3]+.05), c(se.upper[3], se.upper[3]))
lines(c(x[4], x[4]), c(se.lower[4], se.upper[4]))
lines(c(x[4]-.05, x[4]+.05), c(se.lower[4], se.lower[4]))
lines(c(x[4]-.05, x[4]+.05), c(se.upper[4], se.upper[4]))
lines(c(x[5], x[5]), c(se.lower[5], se.upper[5]))
lines(c(x[5]-.05, x[5]+.05), c(se.lower[5], se.lower[5]))
lines(c(x[5]-.05, x[5]+.05), c(se.upper[5], se.upper[5]))
lines(c(x[6], x[6]), c(se.lower[6], se.upper[6]))
lines(c(x[6]-.05, x[6]+.05), c(se.lower[6], se.lower[6]))
lines(c(x[6]-.05, x[6]+.05), c(se.upper[6], se.upper[6]))
lines(c(x[1], x[2]), c(se.upper[4], se.upper[4]) + 3)
lines(c(x[3], x[4]), c(se.upper[4], se.upper[4]) + 3)
lines(c(x[5], x[6]), c(se.upper[4], se.upper[4]) + 3)
lines(c(x[1], x[1]), c(se.upper[4] + 3, se.upper[4] - 7))
lines(c(x[2], x[2]), c(se.upper[4] + 3, se.upper[2] + 1))
lines(c(x[3], x[3]), c(se.upper[4] + 3, se.upper[3] + 1))
lines(c(x[4], x[4]), c(se.upper[4] + 3, se.upper[4] + 1))
lines(c(x[5], x[5]), c(se.upper[4] + 3, se.upper[4] - 7))
lines(c(x[6], x[6]), c(se.upper[4] + 3, se.upper[6] + 1))
text(1, se.upper[4] + 4.25, expression(paste("+8.0%, ", italic("p"), " < .01")), cex = .85)
text(4, se.upper[4] + 4.25, expression(paste("+1.2%, ", italic("p"), " = .80")), cex = .85)
text(7, se.upper[4] + 4.25, expression(paste("+14.8%, ", italic("p"), " < .001")), cex = .85)


#####REGRESSIONS FOR CANDIDATE VOTE CHOICE (TABLE 2)#####

smallstacked <- subset(stacked, select = c(responsenum, candvote, treatment, candgender, candrace, candideology, candideolorder, candpolicy, female)) #Create a subset that includes only the variables needed for the regressions
head(smallstacked)
nrow(smallstacked)

smallstacked$candrace <- as.factor(smallstacked$candrace) #Create factor variables, white, moderate, good jobs, male as the baseline
smallstacked$candpolicy <- as.factor(smallstacked$candpolicy)
smallstacked$candideolorder <- as.factor(smallstacked$candideolorder)

women.small <- subset(smallstacked, female == 1) #Create subsets by respondent gender and condition
men.small <- subset(smallstacked, female == 0)
all.small <- smallstacked
women.smalltreat <- subset(women.small, treatment == 1)
women.smallcont <- subset(women.small, treatment == 0)
men.smalltreat <- subset(men.small, treatment == 1)
men.smallcont <- subset(men.small, treatment == 0)
all.smallcont <- subset(smallstacked, treatment == 0)
all.smalltreat <- subset(smallstacked, treatment == 1)


m1 <- zelig(candvote ~ candgender*treatment + candrace*treatment + candideolorder*treatment + candpolicy*treatment, model = "logit.gee", id = "responsenum", data = all.small, cite = FALSE)
summary(m1)
nrow(na.omit(all.small))

m2 <- zelig(candvote ~ candgender*treatment + candrace*treatment + candideolorder*treatment + candpolicy*treatment, model = "logit.gee", id = "responsenum", data = men.small, cite = FALSE)
summary(m2)
nrow(na.omit(men.small))

m3 <- zelig(candvote ~ candgender*treatment + candrace*treatment + candideolorder*treatment + candpolicy*treatment, model = "logit.gee", id = "responsenum", data = women.small, cite = FALSE)
summary(m3)
nrow(na.omit(women.small))

#####MARGINAL PROBABILITY OF VOTE CHOICE (FIGURE 3)#####

#m1 is all
#m2 is men
#m3 is women

#Get predicted probabilities for all respondents

#All respondents, gender
predprob1 <- setx(m1, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1) #Get predicted probabilities for overall gender
predprob2 <- setx(m1, treatment = 0, candgender = 1, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob3 <- setx(m1, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob4 <- setx(m1, treatment = 1, candgender = 1, candrace = 1, candideolorder = 0, candpolicy = 1)

gender.controlfull <- sim(m1, x = predprob1, x1 = predprob2, num = 100000)
summary(gender.controlfull)

gender.treatfull <- sim(m1, x = predprob3, x1 = predprob4, num = 1000)
summary(gender.treatfull)

genderdiff.full <- c(.075, .060)
genderdiff.fullbar <- genderdiff.full - c(.052, .039)


#All respondents, race
predprob1 <- setx(m1, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1) #Get predicted probabilities for overall race
predprob2 <- setx(m1, treatment = 0, candgender = 0, candrace = 2, candideolorder = 0, candpolicy = 1)
predprob3 <- setx(m1, treatment = 0, candgender = 0, candrace = 3, candideolorder = 0, candpolicy = 1)
predprob4 <- setx(m1, treatment = 0, candgender = 0, candrace = 4, candideolorder = 0, candpolicy = 1)
predprob5 <- setx(m1, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob6 <- setx(m1, treatment = 1, candgender = 0, candrace = 2, candideolorder = 0, candpolicy = 1)
predprob7 <- setx(m1, treatment = 1, candgender = 0, candrace = 3, candideolorder = 0, candpolicy = 1)
predprob8 <- setx(m1, treatment = 1, candgender = 0, candrace = 4, candideolorder = 0, candpolicy = 1)

raceasian.controlfull <- sim(m1, x = predprob1, x1 = predprob2, num = 100000)
summary(raceasian.controlfull)

raceasian.treatfull <- sim(m1, x = predprob5, x1 = predprob6, num = 100000)
summary(raceasian.treatfull)

racehispanic.controlfull <- sim(m1, x = predprob1, x1 = predprob3, num = 100000)
summary(racehispanic.controlfull)

racehispanic.treatfull <- sim(m1, x = predprob5, x1 = predprob7, num = 100000)
summary(racehispanic.treatfull)

raceblack.controlfull <- sim(m1, x = predprob1, x1 = predprob4, num = 100000)
summary(raceblack.controlfull)

raceblack.treatfull <- sim(m1, x = predprob5, x1 = predprob8, num = 100000)
summary(raceblack.treatfull)


racediff.full <- c(.010, .001, -.047, -.047, -.044, -.031)
racediff.fullbar <- racediff.full- c(-.022, -.028, -.080, -.078, -.076, -.060)



#All respondents, ideology
predprob1 <- setx(m1, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1) #Get predicted probabilities for overall ideology
predprob2 <- setx(m1, treatment = 0, candgender = 0, candrace = 1, candideolorder = 1, candpolicy = 1)
predprob3 <- setx(m1, treatment = 0, candgender = 0, candrace = 1, candideolorder = 2, candpolicy = 1)
predprob4 <- setx(m1, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob5 <- setx(m1, treatment = 1, candgender = 0, candrace = 1, candideolorder = 1, candpolicy = 1)
predprob6 <- setx(m1, treatment = 1, candgender = 0, candrace = 1, candideolorder = 2, candpolicy = 1)

ideologycon.controlfull <- sim(m1, x = predprob1, x1 = predprob2, num = 100000)
summary(ideologycon.controlfull)

ideologycon.treatfull <- sim(m1, x = predprob4, x1 = predprob5, num = 100000)
summary(ideologycon.treatfull)

ideologylib.controlfull <- sim(m1, x = predprob1, x1 = predprob3, num = 100000)
summary(ideologylib.controlfull)

ideologylib.treatfull <- sim(m1, x = predprob4, x1 = predprob6, num = 100000)
summary(ideologylib.treatfull)


ideologydiff.full <- c(-.067, -.066, -.039, -.057)
ideologydiff.fullbar <- ideologydiff.full - c(-.098, -.096, -.069, -.085)



#All respondents, policy
predprob1 <- setx(m1, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob2 <- setx(m1, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 3)
predprob3 <- setx(m1, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 2)
predprob4 <- setx(m1, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 4)
predprob5 <- setx(m1, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob6 <- setx(m1, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 3)
predprob7 <- setx(m1, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 2)
predprob8 <- setx(m1, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 4)


policyreform.controlfull <- sim(m1, x = predprob1, x1 = predprob2, num = 100000)
summary(policyreform.controlfull)

policyreform.treatfull <- sim(m1, x = predprob5, x1 = predprob6, num = 100000)
summary(policyreform.treatfull)

policyfairjob.controlfull <- sim(m1, x = predprob1, x1 = predprob3, num = 100000)
summary(policyfairjob.controlfull)

policyfairjob.treatfull <- sim(m1, x = predprob5, x1 = predprob7, num = 100000)
summary(policyfairjob.treatfull)

policyfairprison.controlfull <- sim(m1, x = predprob1, x1 = predprob4, num = 100000)
summary(policyfairprison.controlfull)

policyfairprison.treatfull <- sim(m1, x = predprob5, x1 = predprob8, num = 100000)
summary(policyfairprison.treatfull)


policydiff.full <- c(-.141, -.186, -.091, -.117, -.081, -.126)
policydiff.fullbar <- policydiff.full - c(-.188, -.228, -.132, -.155, -.123, -.164)


all.predprob <- c(policydiff.full, ideologydiff.full, racediff.full, genderdiff.full)
all.ci <- c(policydiff.fullbar, ideologydiff.fullbar, racediff.fullbar, genderdiff.fullbar)





#Get predicted probabilities for men respondents

#men respondents, gender
predprob1 <- setx(m2, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1) #Get predicted probabilities for overmen gender
predprob2 <- setx(m2, treatment = 0, candgender = 1, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob3 <- setx(m2, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob4 <- setx(m2, treatment = 1, candgender = 1, candrace = 1, candideolorder = 0, candpolicy = 1)

gender.controlmen <- sim(m2, x = predprob1, x1 = predprob2, num = 100000)
summary(gender.controlmen)

gender.treatmen <- sim(m2, x = predprob3, x1 = predprob4, num = 1000)
summary(gender.treatmen)

genderdiff.men <- c(.040, .059)
genderdiff.menbar <- genderdiff.men - c(.008, .027)


#men respondents, race
predprob1 <- setx(m2, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1) #Get predicted probabilities for overmen race
predprob2 <- setx(m2, treatment = 0, candgender = 0, candrace = 2, candideolorder = 0, candpolicy = 1)
predprob3 <- setx(m2, treatment = 0, candgender = 0, candrace = 3, candideolorder = 0, candpolicy = 1)
predprob4 <- setx(m2, treatment = 0, candgender = 0, candrace = 4, candideolorder = 0, candpolicy = 1)
predprob5 <- setx(m2, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob6 <- setx(m2, treatment = 1, candgender = 0, candrace = 2, candideolorder = 0, candpolicy = 1)
predprob7 <- setx(m2, treatment = 1, candgender = 0, candrace = 3, candideolorder = 0, candpolicy = 1)
predprob8 <- setx(m2, treatment = 1, candgender = 0, candrace = 4, candideolorder = 0, candpolicy = 1)

raceasian.controlmen <- sim(m2, x = predprob1, x1 = predprob2, num = 100000)
summary(raceasian.controlmen)

raceasian.treatmen <- sim(m2, x = predprob5, x1 = predprob6, num = 100000)
summary(raceasian.treatmen)

racehispanic.controlmen <- sim(m2, x = predprob1, x1 = predprob3, num = 100000)
summary(racehispanic.controlmen)

racehispanic.treatmen <- sim(m2, x = predprob5, x1 = predprob7, num = 100000)
summary(racehispanic.treatmen)

raceblack.controlmen <- sim(m2, x = predprob1, x1 = predprob4, num = 100000)
summary(raceblack.controlmen)

raceblack.treatmen <- sim(m2, x = predprob5, x1 = predprob8, num = 100000)
summary(raceblack.treatmen)


racediff.men <- c(-.032, -.013, -.073, -.058, -.041, -.040)
racediff.menbar <- racediff.men- c(-.076, -.058, -.121, -.104, -.085, -.082)



#men respondents, ideology
predprob1 <- setx(m2, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1) #Get predicted probabilities for overmen ideology
predprob2 <- setx(m2, treatment = 0, candgender = 0, candrace = 1, candideolorder = 1, candpolicy = 1)
predprob3 <- setx(m2, treatment = 0, candgender = 0, candrace = 1, candideolorder = 2, candpolicy = 1)
predprob4 <- setx(m2, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob5 <- setx(m2, treatment = 1, candgender = 0, candrace = 1, candideolorder = 1, candpolicy = 1)
predprob6 <- setx(m2, treatment = 1, candgender = 0, candrace = 1, candideolorder = 2, candpolicy = 1)

ideologycon.controlmen <- sim(m2, x = predprob1, x1 = predprob2, num = 100000)
summary(ideologycon.controlmen)

ideologycon.treatmen <- sim(m2, x = predprob4, x1 = predprob5, num = 100000)
summary(ideologycon.treatmen)

ideologylib.controlmen <- sim(m2, x = predprob1, x1 = predprob3, num = 100000)
summary(ideologylib.controlmen)

ideologylib.treatmen <- sim(m2, x = predprob4, x1 = predprob6, num = 100000)
summary(ideologylib.treatmen)


ideologydiff.men <- c(-.066, -.049, -.021, -.025)
ideologydiff.menbar <- ideologydiff.men - c(-.109, -.093, -.063, -.064)



#men respondents, policy
predprob1 <- setx(m2, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob2 <- setx(m2, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 3)
predprob3 <- setx(m2, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 2)
predprob4 <- setx(m2, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 4)
predprob5 <- setx(m2, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob6 <- setx(m2, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 3)
predprob7 <- setx(m2, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 2)
predprob8 <- setx(m2, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 4)


policyreform.controlmen <- sim(m2, x = predprob1, x1 = predprob2, num = 100000)
summary(policyreform.controlmen)

policyreform.treatmen <- sim(m2, x = predprob5, x1 = predprob6, num = 100000)
summary(policyreform.treatmen)

policyfairjob.controlmen <- sim(m2, x = predprob1, x1 = predprob3, num = 100000)
summary(policyfairjob.controlmen)

policyfairjob.treatmen <- sim(m2, x = predprob5, x1 = predprob7, num = 100000)
summary(policyfairjob.treatmen)

policyfairprison.controlmen <- sim(m2, x = predprob1, x1 = predprob4, num = 100000)
summary(policyfairprison.controlmen)

policyfairprison.treatmen <- sim(m2, x = predprob5, x1 = predprob8, num = 100000)
summary(policyfairprison.treatmen)


policydiff.men <- c(-.167, -.146, -.107, -.084, -.150, -.142)
policydiff.menbar <- policydiff.men - c(-.226, -.202, -.159, -.135, -.205, -.192)


men.predprob <- c(policydiff.men, ideologydiff.men, racediff.men, genderdiff.men)
men.ci <- c(policydiff.menbar, ideologydiff.menbar, racediff.menbar, genderdiff.menbar)









#Get predicted probabilities for women respondents

#women respondents, gender
predprob1 <- setx(m3, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1) #Get predicted probabilities for overwomen gender
predprob2 <- setx(m3, treatment = 0, candgender = 1, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob3 <- setx(m3, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob4 <- setx(m3, treatment = 1, candgender = 1, candrace = 1, candideolorder = 0, candpolicy = 1)

gender.controlwomen <- sim(m3, x = predprob1, x1 = predprob2, num = 100000)
summary(gender.controlwomen)

gender.treatwomen <- sim(m3, x = predprob3, x1 = predprob4, num = 1000)
summary(gender.treatwomen)

genderdiff.women <- c(.110, .062)
genderdiff.womenbar <- genderdiff.women - c(.078, .032)


#women respondents, race
predprob1 <- setx(m3, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1) #Get predicted probabilities for overwomen race
predprob2 <- setx(m3, treatment = 0, candgender = 0, candrace = 2, candideolorder = 0, candpolicy = 1)
predprob3 <- setx(m3, treatment = 0, candgender = 0, candrace = 3, candideolorder = 0, candpolicy = 1)
predprob4 <- setx(m3, treatment = 0, candgender = 0, candrace = 4, candideolorder = 0, candpolicy = 1)
predprob5 <- setx(m3, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob6 <- setx(m3, treatment = 1, candgender = 0, candrace = 2, candideolorder = 0, candpolicy = 1)
predprob7 <- setx(m3, treatment = 1, candgender = 0, candrace = 3, candideolorder = 0, candpolicy = 1)
predprob8 <- setx(m3, treatment = 1, candgender = 0, candrace = 4, candideolorder = 0, candpolicy = 1)

raceasian.controlwomen <- sim(m3, x = predprob1, x1 = predprob2, num = 100000)
summary(raceasian.controlwomen)

raceasian.treatwomen <- sim(m3, x = predprob5, x1 = predprob6, num = 100000)
summary(raceasian.treatwomen)

racehispanic.controlwomen <- sim(m3, x = predprob1, x1 = predprob3, num = 100000)
summary(racehispanic.controlwomen)

racehispanic.treatwomen <- sim(m3, x = predprob5, x1 = predprob7, num = 100000)
summary(racehispanic.treatwomen)

raceblack.controlwomen <- sim(m3, x = predprob1, x1 = predprob4, num = 100000)
summary(raceblack.controlwomen)

raceblack.treatwomen <- sim(m3, x = predprob5, x1 = predprob8, num = 100000)
summary(raceblack.treatwomen)


racediff.women <- c(.056, .014, -.018, -.037, -.042, -.022)
racediff.womenbar <- racediff.women- c(.011, -.024, -.061, -.077, -.087, -.064)



#women respondents, ideology
predprob1 <- setx(m3, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1) #Get predicted probabilities for overwomen ideology
predprob2 <- setx(m3, treatment = 0, candgender = 0, candrace = 1, candideolorder = 1, candpolicy = 1)
predprob3 <- setx(m3, treatment = 0, candgender = 0, candrace = 1, candideolorder = 2, candpolicy = 1)
predprob4 <- setx(m3, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob5 <- setx(m3, treatment = 1, candgender = 0, candrace = 1, candideolorder = 1, candpolicy = 1)
predprob6 <- setx(m3, treatment = 1, candgender = 0, candrace = 1, candideolorder = 2, candpolicy = 1)

ideologycon.controlwomen <- sim(m3, x = predprob1, x1 = predprob2, num = 100000)
summary(ideologycon.controlwomen)

ideologycon.treatwomen <- sim(m3, x = predprob4, x1 = predprob5, num = 100000)
summary(ideologycon.treatwomen)

ideologylib.controlwomen <- sim(m3, x = predprob1, x1 = predprob3, num = 100000)
summary(ideologylib.controlwomen)

ideologylib.treatwomen <- sim(m3, x = predprob4, x1 = predprob6, num = 100000)
summary(ideologylib.treatwomen)


ideologydiff.women <- c(-.064, -.082, -.055, -.088)
ideologydiff.womenbar <- ideologydiff.women - c(-.107, -.125, -.097, -.126)



#women respondents, policy
predprob1 <- setx(m3, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob2 <- setx(m3, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 3)
predprob3 <- setx(m3, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 2)
predprob4 <- setx(m3, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 4)
predprob5 <- setx(m3, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob6 <- setx(m3, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 3)
predprob7 <- setx(m3, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 2)
predprob8 <- setx(m3, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 4)


policyreform.controlwomen <- sim(m3, x = predprob1, x1 = predprob2, num = 100000)
summary(policyreform.controlwomen)

policyreform.treatwomen <- sim(m3, x = predprob5, x1 = predprob6, num = 100000)
summary(policyreform.treatwomen)

policyfairjob.controlwomen <- sim(m3, x = predprob1, x1 = predprob3, num = 100000)
summary(policyfairjob.controlwomen)

policyfairjob.treatwomen <- sim(m3, x = predprob5, x1 = predprob7, num = 100000)
summary(policyfairjob.treatwomen)

policyfairprison.controlwomen <- sim(m3, x = predprob1, x1 = predprob4, num = 100000)
summary(policyfairprison.controlwomen)

policyfairprison.treatwomen <- sim(m3, x = predprob5, x1 = predprob8, num = 100000)
summary(policyfairprison.treatwomen)


policydiff.women <- c(-.115, -.225, -.078, -.151, -.009, -.110)
policydiff.womenbar <- policydiff.women - c(-.186, -.289, -.141, -.206, -.074, -.167)


women.predprob <- c(policydiff.women, ideologydiff.women, racediff.women, genderdiff.women)
women.ci <- c(policydiff.womenbar, ideologydiff.womenbar, racediff.womenbar, genderdiff.womenbar)


fullyvalue <- c(1, 1.5, 2.2, 4.2, 4.7, 5.4, 7.4, 7.9, 8.6,9.6, 12.6, 13.1, 13.8,15.8, 16.3, 17.0, 18.0,    21.0, 21.5, 22.2, 24.2, 24.7, 25.4,   27.4, 27.9, 28.6,   29.6,   32.6, 33.1, 33.8,   34.8)
pointyvalue <- c(1, 1.5, 4.2, 4.7, 7.4, 7.9, 12.6, 13.1, 15.8, 16.3, 21.0, 21.5, 24.2, 24.7, 27.4, 27.9, 32.6, 33.1)
groupyvalue <- c(2.2, 5.4, 8.6, 13.8, 17.0, 22.2, 25.4, 28.6, 33.8)
labyvalue <- c(9.6, 18.0, 29.6, 34.8)



par(mar=c(3, .45, 2, .45), oma = c(5, 12.0, 0, 0))
par(mfrow = c(1,3))
plotCI(all.predprob, pointyvalue, xlab = "", uiw = all.ci, sfrac = .002, err = "x", lwd = 1, gap = 0, pch = c(21, 16), yaxt = "n", ylab = "", main = "All Respondents", xlim = c(-.30, .30), ylim = c(.5, 35.3), cex.axis = .8, cex.main = 1)
text(-.33, pointyvalue, pos = 2, c("Control", "Treatment", "Control", "Treatment","Control", "Treatment","Control", "Treatment","Control", "Treatment","Control", "Treatment") , srt = 360, xpd = NA)
text(-.33, groupyvalue, c("Fair Sentencing", "Fair Employment", "Justice Reform", "Very Liberal", "Slightly Liberal", "African American", "Hispanic", "Asian American", "Female"), pos = 2, font = 3, xpd = NA)
text(-.33, labyvalue, c("Policy (Good Jobs Baseline)", "Ideology (Moderate Baseline)", "Race (White Basline)", "Gender (Male Baseline)"), pos = 2, xpd = NA, font = 2)
abline(v = 0, lty = 2)



plotCI(men.predprob, pointyvalue, xlab = "", uiw = men.ci, sfrac = .002, err = "x", lwd = 1, gap = 0, pch = c(21, 16), yaxt = "n", ylab = "", main = "Men", xlim = c(-.30, .30), ylim = c(.5, 35.3), cex.axis = .8, cex.main = 1)
abline(v = 0, lty = 2)
legend(-.42, -3.35, c("With Identity Politics Loss Narrative", "Without Identity Politics Loss Narrative ", expression(paste("Significant Effect, ", italic("p"), " < .10 "))), col = c("black"), pch = c(16, 21, 8), xpd = NA)


plotCI(women.predprob, pointyvalue, xlab = "", uiw = women.ci, sfrac = .002, err = "x", lwd = 1, gap = 0, pch = c(21, 8, 21, 8, 21, 8, 21, 16, 21, 16, 21, 16, 21, 16, 21, 16, 21, 8), yaxt = "n", ylab = "", main = "Women", xlim = c(-.30, .30), ylim = c(.5, 35.3), cex.axis = .8, cex.main = 1)
abline(v = 0, lty = 2)

#####PROBABILITY BY CANDIDATE PROFILE (FIGURE 4)#####

#m1 is all
#m2 is men
#m3 is women

#Generate the predicted probabilities, view them in excel, manually input results into figure

#Get predicted probabilities for all candidates

allcombos.control <- expand.grid(treatment = 0, candgender = c(0, 1), candrace = c(1, 2, 3, 4), candideolorder = c(0, 1, 2), candpolicy = c(1, 2, 3, 4))
head(allcombos.control)
nrow(allcombos.control)

allcombos.treatment <- expand.grid(treatment = 1, candgender = c(0, 1), candrace = c(1, 2, 3, 4), candideolorder = c(0, 1, 2), candpolicy = c(1, 2, 3, 4))
head(allcombos.treatment)
nrow(allcombos.treatment)



control.all <- data.frame(matrix(ncol = 7, nrow = 0))
x <- c("treatment", "candgender", "candrace", "candideolorder", "candpolicy", "prob", "se")
colnames(control.summary) <- x

treatment.all <- data.frame(matrix(ncol = 7, nrow = 0))
x <- c("treatment", "candgender", "candrace", "candideolorder", "candpolicy", "prob", "se")
colnames(treatment.summary) <- x

i <- 1

while (i <= 96){
  newdata <- setx(m1, treatment = allcombos.control[i, 1], candgender = allcombos.control[i, 2], candrace = allcombos.control[i, 3], candideolorder = allcombos.control[i, 4], candpolicy = allcombos.control[i, 5])
  predprob1 <- sim(m1, x = newdata, num = 100000)
  predprob2 <- zelig_qi_to_df((predprob1))
  prob <- mean(predprob2$expected_value)
  se <- sd(predprob2$expected_value)
  predprob.all <- cbind(allcombos.control[i, ], prob, se)
  control.all <- rbind(control.all, predprob.all)
  i <- i + 1
}

control.allfull <- control.all
write.csv(control.allfull, "All Control New.csv")



i <- 1

while (i <= 96){
  newdata <- setx(m1, treatment = allcombos.treatment[i, 1], candgender = allcombos.treatment[i, 2], candrace = allcombos.treatment[i, 3], candideolorder = allcombos.treatment[i, 4], candpolicy = allcombos.treatment[i, 5])
  predprob1 <- sim(m1, x = newdata, num = 100000)
  predprob2 <- zelig_qi_to_df((predprob1))
  prob <- mean(predprob2$expected_value)
  se <- sd(predprob2$expected_value)
  predprob.all <- cbind(allcombos.treatment[i, ], prob, se)
  treatment.all <- rbind(treatment.all, predprob.all)
  i <- i + 1
}

treatment.allfull <- treatment.all
write.csv(treatment.allfull, "All Treatment New.csv")





control.men <- data.frame(matrix(ncol = 7, nrow = 0))
x <- c("treatment", "candgender", "candrace", "candideolorder", "candpolicy", "prob", "se")
colnames(control.men) <- x

treatment.men <- data.frame(matrix(ncol = 7, nrow = 0))
x <- c("treatment", "candgender", "candrace", "candideolorder", "candpolicy", "prob", "se")
colnames(treatment.men) <- x

i <- 1

while (i <= 96){
  newdata <- setx(m2, treatment = allcombos.control[i, 1], candgender = allcombos.control[i, 2], candrace = allcombos.control[i, 3], candideolorder = allcombos.control[i, 4], candpolicy = allcombos.control[i, 5])
  predprob1 <- sim(m2, x = newdata, num = 100000)
  predprob2 <- zelig_qi_to_df((predprob1))
  prob <- mean(predprob2$expected_value)
  se <- sd(predprob2$expected_value)
  predprob.all <- cbind(allcombos.control[i, ], prob, se)
  control.all <- rbind(control.all, predprob.all)
  i <- i + 1
}

control.menfull <- control.all



i <- 1

while (i <= 96){
  newdata <- setx(m2, treatment = allcombos.treatment[i, 1], candgender = allcombos.treatment[i, 2], candrace = allcombos.treatment[i, 3], candideolorder = allcombos.treatment[i, 4], candpolicy = allcombos.treatment[i, 5])
  predprob1 <- sim(m2, x = newdata, num = 100000)
  predprob2 <- zelig_qi_to_df((predprob1))
  prob <- mean(predprob2$expected_value)
  se <- sd(predprob2$expected_value)
  predprob.all <- cbind(allcombos.treatment[i, ], prob, se)
  treatment.all <- rbind(treatment.all, predprob.all)
  i <- i + 1
}

treatment.menfull <- treatment.all

write.csv(control.menfull, "mencontrol.csv")
write.csv(treatment.menfull, "mentreatment.csv")





control.women <- data.frame(matrix(ncol = 7, nrow = 0))
x <- c("treatment", "candgender", "candrace", "candideolorder", "candpolicy", "prob", "se")
colnames(control.women) <- x

treatment.women <- data.frame(matrix(ncol = 7, nrow = 0))
x <- c("treatment", "candgender", "candrace", "candideolorder", "candpolicy", "prob", "se")
colnames(treatment.summary) <- x

i <- 1

while (i <= 96){
  newdata <- setx(m3, treatment = allcombos.control[i, 1], candgender = allcombos.control[i, 2], candrace = allcombos.control[i, 3], candideolorder = allcombos.control[i, 4], candpolicy = allcombos.control[i, 5])
  predprob1 <- sim(m3, x = newdata, num = 100000)
  predprob2 <- zelig_qi_to_df((predprob1))
  prob <- mean(predprob2$expected_value)
  se <- sd(predprob2$expected_value)
  predprob.women <- cbind(allcombos.control[i, ], prob, se)
  control.women <- rbind(control.women, predprob.women)
  i <- i + 1
}

control.womenfull <- control.women
write.csv(control.womenfull, "womencontrol.csv")


i <- 1

while (i <= 96){
  newdata <- setx(m3, treatment = allcombos.treatment[i, 1], candgender = allcombos.treatment[i, 2], candrace = allcombos.treatment[i, 3], candideolorder = allcombos.treatment[i, 4], candpolicy = allcombos.treatment[i, 5])
  predprob1 <- sim(m3, x = newdata, num = 100000)
  predprob2 <- zelig_qi_to_df((predprob1))
  prob <- mean(predprob2$expected_value)
  se <- sd(predprob2$expected_value)
  predprob.women <- cbind(allcombos.treatment[i, ], prob, se)
  treatment.women <- rbind(treatment.women, predprob.women)
  i <- i + 1
}

treatment.womenfull <- treatment.women
write.csv(control.womenfull, "womentreatment.csv")




prob.womenc <- c(.694, .590, .624, .686, .588)
low.womenc <- c(.647, .537, .574, .637, .533)
high.womenc <- c(.741, .642, .674, .734, .642)

prob.woment <- c(.730, .670, .590, .630, .518)
low.woment <- c(.688, .625, .542, .581, .456)
high.woment <- c(.771, .715, .639, .679, .579)


prob.menc <- c(.693, .653, .590, .548, .531)
low.menc <- c(.640, .596, .542, .498, .478)
high.menc <- c(.746, .711, .638, .599, .585)

prob.ment <- c(.675, .616, .596, .539, .534)
low.ment <- c(.628, .564, .546, .491, .489)
high.ment <- c(.722, .669, .646, .588, .581)

#Dimensions are 6.5x6.5
layout(matrix(c(1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6), 2, 13, byrow = TRUE))
par(mar = c(3, .25, 5, 2.25))
par(family = "serif")
barplot(0, 0, col = "white", xlim = c(0, 6.5), ylim = c(0, .80), yaxt ="n", xaxt = "n", ylab = "", xlab = "")
box()
text(3.1, .85, "Preferred Candidate", xpd = NA, cex = 1.25)
text(4.95, .98, "White Democratic Women", xpd = NA, cex = 1.5)
text(3.1, .45, "Woman\n \nAfrican American\n \nModerately Liberal\n \nFocused on Jobs", cex = 1.15)

par(mar = c(3, 1, 5, .25))
x <- barplot(prob.womenc, ylim = c(0, .90), col = c("grey45","grey72","grey72","grey72", "grey72"), ylab = "Probability")
box()
text(3.1, .95, "Without Identity Politics Loss Narrative", xpd = NA, cex = 1.25)
lines(c(x[1], x[1]), c(low.womenc[1], high.womenc[1]))
lines(c(x[1] - .05, x[1] + .05), c(low.womenc[1], low.womenc[1]))
lines(c(x[1] - .05, x[1] + .05), c(high.womenc[1], high.womenc[1]))
lines(c(x[2], x[2]), c(low.womenc[2], high.womenc[2]))
lines(c(x[2] - .05, x[2] + .05), c(low.womenc[2], low.womenc[2]))
lines(c(x[2] - .05, x[2] + .05), c(high.womenc[2], high.womenc[2]))
lines(c(x[3], x[3]), c(low.womenc[3], high.womenc[3]))
lines(c(x[3] - .05, x[3] + .05), c(low.womenc[3], low.womenc[3]))
lines(c(x[3] - .05, x[3] + .05), c(high.womenc[3], high.womenc[3]))
lines(c(x[4], x[4]), c(low.womenc[4], high.womenc[4]))
lines(c(x[4] - .05, x[4] + .05), c(low.womenc[4], low.womenc[4]))
lines(c(x[4] - .05, x[4] + .05), c(high.womenc[4], high.womenc[4]))
lines(c(x[5], x[5]), c(low.womenc[5], high.womenc[5]))
lines(c(x[5] - .05, x[5] + .05), c(low.womenc[5], low.womenc[5]))
lines(c(x[5] - .05, x[5] + .05), c(high.womenc[5], high.womenc[5]))
text(x, -.06, c("Preferred\n ", "Man\n ", "Justice\nReform", "Fair\nEmploy", "Fair\nSentenes"), xpd = NA, cex = .9)
text(x[1], high.womenc[1] + .035, "0.69", font = 2)
text(x[2], high.womenc[2] + .035, "-.10 ", cex = 1.05)
text(x[3], high.womenc[3] + .035, "-.07 ", cex = 1.05)
text(x[4], high.womenc[4] + .035, "-.01 ", cex = 1.05)
text(x[5], high.womenc[5] + .035, "-.11 ", cex = 1.05)

par(mar = c(3, 1, 5, .25))
x <- barplot(prob.woment, ylim = c(0, .90), col = c("grey45","grey72","grey72","grey72", "grey72"), ylab = "", yaxt = "n")
box()
axis(2, at = c(0, .2, .4, .6, .8), c("", "", "", "", ""))
text(3.1, .95, "With Identity Politics Loss Narrative", xpd = NA, cex = 1.25)
lines(c(x[1], x[1]), c(low.woment[1], high.woment[1]))
lines(c(x[1] - .05, x[1] + .05), c(low.woment[1], low.woment[1]))
lines(c(x[1] - .05, x[1] + .05), c(high.woment[1], high.woment[1]))
lines(c(x[2], x[2]), c(low.woment[2], high.woment[2]))
lines(c(x[2] - .05, x[2] + .05), c(low.woment[2], low.woment[2]))
lines(c(x[2] - .05, x[2] + .05), c(high.woment[2], high.woment[2]))
lines(c(x[3], x[3]), c(low.woment[3], high.woment[3]))
lines(c(x[3] - .05, x[3] + .05), c(low.woment[3], low.woment[3]))
lines(c(x[3] - .05, x[3] + .05), c(high.woment[3], high.woment[3]))
lines(c(x[4], x[4]), c(low.woment[4], high.woment[4]))
lines(c(x[4] - .05, x[4] + .05), c(low.woment[4], low.woment[4]))
lines(c(x[4] - .05, x[4] + .05), c(high.woment[4], high.woment[4]))
lines(c(x[5], x[5]), c(low.woment[5], high.woment[5]))
lines(c(x[5] - .05, x[5] + .05), c(low.woment[5], low.woment[5]))
lines(c(x[5] - .05, x[5] + .05), c(high.woment[5], high.woment[5]))
text(x, -.06, c("Preffered\n ", "Man\n ", "Justice\nReform", "Fair\nEmploy", "Fair\nSentenes"), xpd = NA, cex = .9)
text(x[1], high.woment[1] + .035, "0.73", font = 2, cex = 1)
text(x[2], high.woment[2] + .035, "-.06 ", cex = 1.05)
text(x[3], high.woment[3] + .035, "-.14 ", cex = 1.05)
text(x[4], high.woment[4] + .035, "-.10 ", cex = 1.05)
text(x[5], high.woment[5] + .035, "-.21 ", cex = 1.05)


par(mar = c(2.5, .25, 5.4, 2.25))
par(family = "serif")
barplot(0, 0, col = "white", xlim = c(0, 6.5), ylim = c(0, .80), yaxt ="n", xaxt = "n", ylab = "", xlab = "")
box()
text(3.1, .85, "Preferred Candidate", xpd = NA, cex = 1.25)
text(4.35, .98, "White Democratic Men", xpd = NA, cex = 1.5)
text(3.1, .45, "Woman\n \nWhite\n \nModerately Liberal\n \nFocused on Jobs", cex = 1.15)

par(mar = c(2.5, 1, 5.5, .25))
x <- barplot(prob.menc, ylim = c(0, .90), col = c("grey45","grey72","grey72","grey72", "grey72"), ylab = "Probability")
box()
text(3.1, .95, "Without Identity Politics Loss Narrative", xpd = NA, cex = 1.25)
lines(c(x[1], x[1]), c(low.menc[1], high.menc[1]))
lines(c(x[1] - .05, x[1] + .05), c(low.menc[1], low.menc[1]))
lines(c(x[1] - .05, x[1] + .05), c(high.menc[1], high.menc[1]))
lines(c(x[2], x[2]), c(low.menc[2], high.menc[2]))
lines(c(x[2] - .05, x[2] + .05), c(low.menc[2], low.menc[2]))
lines(c(x[2] - .05, x[2] + .05), c(high.menc[2], high.menc[2]))
lines(c(x[3], x[3]), c(low.menc[3], high.menc[3]))
lines(c(x[3] - .05, x[3] + .05), c(low.menc[3], low.menc[3]))
lines(c(x[3] - .05, x[3] + .05), c(high.menc[3], high.menc[3]))
lines(c(x[4], x[4]), c(low.menc[4], high.menc[4]))
lines(c(x[4] - .05, x[4] + .05), c(low.menc[4], low.menc[4]))
lines(c(x[4] - .05, x[4] + .05), c(high.menc[4], high.menc[4]))
lines(c(x[5], x[5]), c(low.menc[5], high.menc[5]))
lines(c(x[5] - .05, x[5] + .05), c(low.menc[5], low.menc[5]))
lines(c(x[5] - .05, x[5] + .05), c(high.menc[5], high.menc[5]))
text(x, -.06, c("Preferred\n ", "Man\n ", "Justice\nReform", "Fair\nEmploy", "Fair\nSentenes"), xpd = NA, cex = .9)
text(x[1], high.menc[1] + .035, "0.69", font = 2)
text(x[2], high.menc[2] + .035, "-.04 ", cex = 1.05)
text(x[3], high.menc[3] + .035, "-.10 ", cex = 1.05)
text(x[4], high.menc[4] + .035, "-.14 ", cex = 1.05)
text(x[5], high.menc[5] + .035, "-.16 ", cex = 1.05)

par(mar = c(2.5, 1, 5.5, .25))
x <- barplot(prob.ment, ylim = c(0, .90), col = c("grey45","grey72","grey72","grey72", "grey72"), ylab = "", yaxt = "n")
box()
axis(2, at = c(0, .2, .4, .6, .8), c("", "", "", "", ""))
text(3.1, .95, "With Identity Politics Loss Narrative", xpd = NA, cex = 1.25)
lines(c(x[1], x[1]), c(low.ment[1], high.ment[1]))
lines(c(x[1] - .05, x[1] + .05), c(low.ment[1], low.ment[1]))
lines(c(x[1] - .05, x[1] + .05), c(high.ment[1], high.ment[1]))
lines(c(x[2], x[2]), c(low.ment[2], high.ment[2]))
lines(c(x[2] - .05, x[2] + .05), c(low.ment[2], low.ment[2]))
lines(c(x[2] - .05, x[2] + .05), c(high.ment[2], high.ment[2]))
lines(c(x[3], x[3]), c(low.ment[3], high.ment[3]))
lines(c(x[3] - .05, x[3] + .05), c(low.ment[3], low.ment[3]))
lines(c(x[3] - .05, x[3] + .05), c(high.ment[3], high.ment[3]))
lines(c(x[4], x[4]), c(low.ment[4], high.ment[4]))
lines(c(x[4] - .05, x[4] + .05), c(low.ment[4], low.ment[4]))
lines(c(x[4] - .05, x[4] + .05), c(high.ment[4], high.ment[4]))
lines(c(x[5], x[5]), c(low.ment[5], high.ment[5]))
lines(c(x[5] - .05, x[5] + .05), c(low.ment[5], low.ment[5]))
lines(c(x[5] - .05, x[5] + .05), c(high.ment[5], high.ment[5]))
text(x, -.06, c("Preferred\n ", "Man\n ", "Justice\nReform", "Fair\nEmploy", "Fair\nSentenes"), xpd = NA, cex = .9)
text(x[1], high.ment[1] + .035, "0.68", font = 2)
text(x[2], high.ment[2] + .035, "-.06 ", cex = 1.05)
text(x[3], high.ment[3] + .035, "-.08 ", cex = 1.05)
text(x[4], high.ment[4] + .035, "-.13 ", cex = 1.05)
text(x[5], high.ment[5] + .035, "-.14 ", cex = 1.05)

#####PROBABILITY OF WIN BY CANDIDATE TRAIT (FIGURE 5)#####

#m1 is all
#m2 is men
#m3 is women

#Probabilities are calculated in excel files; double check results there and import into graph

prob.womenc <- c(.250, .750, .502, .385, .447, .666, .630, .450, .419, .649, .417, .621, .312)

prob.woment <- c(.351, .649, .532, .465, .422, .580, .685, .398, .416, .860, .406, .531, .203)


compare.women <- rbind(prob.womenc, prob.woment)


prob.menc <- c(.402, .598, .609, .484, .394, .514, .599, .527, .374, .848, .482, .358, .313)

prob.ment <- c(0.351, .648, .588, .463, .401, .547, .591, .496, .413, .805, .522, .341, .331)

compare.men <- rbind(prob.menc, prob.ment)


#Dimensions are 6.5x6.5
par(family = "serif")
par(mar = c(3.5, 2.5, 1.5, .25))
par(mfrow = c(2, 1))
groups <- c("Man", "Woman", "White", "Asian", "Latino", "Black", "Moderate\nLiberal", "Less\nLiberal", "More\nLiberal", "Good\nJobs", "Justice\nReform", "Fair\nEmploy", "Fair\nSentences")

x <- barplot(compare.women, ylim = c(0, 1), xaxt = "n", col = c("grey80", "grey40"), beside = TRUE, space =c(0, 0, .75, 0, .75, 0, .75, 0, .75, 0, .75, 0, .75, 0, .75, 0, .75, 0, .75, 0, .75, 0, .75, 0, .75, 0), cex.axis = .8)
box()
xspot <- x[1,] + (x[2,] - x[1,])/2
text(17.5, 1.07, xpd = NA, "White Democratic Women", cex = .9)
text(xspot, -.07, xpd = NA, c("Man\n ", "Woman\n ", "White\n ", "Asian\n ", "Latino\n ", "Black\n ", "Moderate\nLiberal", "Less\nLiberal", "More\nLiberal", "Good\nJobs", "Justice\nReform", "Fair\nEmploy", "Fair\nSentences"), cex = .65)
text(x, compare.women + .04, c(".25 ", " .35", ".75 ", " .65", ".50 ", " .53", ".39 ", " .47", ".45 ", " .42", ".67 ", " .58", ".63 ", " .69", ".45 ", " .40", ".42 ", " .42", ".65 ", " .86", ".42 ", " .41", ".62 ", " .53", ".32 ", " .20"), cex = .65)


par(mar = c(4.5, 2.5, .5, .25))
x< - barplot(compare.men, ylim = c(0, 1), xaxt = "n", col = c("grey80", "grey40"), beside = TRUE, space =c(0, 0, .75, 0, .75, 0, .75, 0, .75, 0, .75, 0, .75, 0, .75, 0, .75, 0, .75, 0, .75, 0, .75, 0, .75, 0), cex.axis = .8)
box()
text(17.5, 1.07, xpd = NA, "White Democratic Men", cex = .9)
text(xspot, -.07, xpd = NA, c("Man\n ", "Woman\n ", "White\n ", "Asian\n ", "Latino\n ", "Black\n ", "Moderate\nLiberal", "Less\nLiberal", "More\nLiberal", "Good\nJobs", "Justice\nReform", "Fair\nEmploy", "Fair\nSentences"), cex = .65)
text(x, compare.men + .04, c(".40 ", " .35", ".60 ", " .65", ".61 ", " .59", ".48 ", " .46", ".39 ", " .40",".51 ", " .55",".60 ", " .59", ".53 "," .50", ".37 ", " .41", ".85 ", " .80", ".48 ", " .52", ".36 ", " .34", ".31 ", " .33"), cex = .65)
legend(-1.5, -.17, xpd = NA, c("Without Identity Politics Narrative   ", "With Identity Politics Narrative"), fill = c("grey80", "grey40"), cex = .8)




#####REGRESSIONS FOR FIRST HEAT ROBUSTNESS CHECK (OA-H)#####

smallstacked.robust <- subset(stacked, heat == 1, select = c(responsenum, heat, candvote, treatment, candgender, candrace, candideology, candideolorder, candpolicy, female)) #Create a subset that includes only the variables needed for the regressions
head(smallstacked.robust)
nrow(smallstacked.robust)

smallstacked.robust$candrace <- as.factor(smallstacked.robust$candrace) #Create factor variables, white, moderate, good jobs, male as the baseline
smallstacked.robust$candpolicy <- as.factor(smallstacked.robust$candpolicy)
smallstacked.robust$candideolorder <- as.factor(smallstacked.robust$candideolorder)

women.small <- subset(smallstacked.robust, female == 1) #Create subsets by respondent gender and condition
men.small <- subset(smallstacked.robust, female == 0)
all.small <- smallstacked.robust
women.smalltreat <- subset(smallstacked.robust, treatment == 1)
women.smallcont <- subset(smallstacked.robust, treatment == 0)
men.smalltreat <- subset(smallstacked.robust, treatment == 1)
men.smallcont <- subset(smallstacked.robust, treatment == 0)
all.smallcont <- subset(smallstacked.robust, treatment == 0)
all.smalltreat <- subset(smallstacked.robust, treatment == 1)


m1 <- zelig(candvote ~ candgender*treatment + candrace*treatment + candideolorder*treatment + candpolicy*treatment, model = "logit.gee", id = "responsenum", data = all.small, cite = FALSE)
summary(m1)
nrow(na.omit(all.small))

m2 <- zelig(candvote ~ candgender*treatment + candrace*treatment + candideolorder*treatment + candpolicy*treatment, model = "logit.gee", id = "responsenum", data = men.small, cite = FALSE)
summary(m2)
nrow(na.omit(men.small))

m3 <- zelig(candvote ~ candgender*treatment + candrace*treatment + candideolorder*treatment + candpolicy*treatment, model = "logit.gee", id = "responsenum", data = women.small, cite = FALSE)
summary(m3)
nrow(na.omit(women.small))

#####MARGINAL PROBABILITY OF VOTE CHOICE IN FIRST HEAT (OA-H)#####

#m1 is all
#m2 is men
#m3 is women

#Get predicted probabilities for all respondents

#All respondents, gender
predprob1 <- setx(m1, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1) #Get predicted probabilities for overall gender
predprob2 <- setx(m1, treatment = 0, candgender = 1, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob3 <- setx(m1, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob4 <- setx(m1, treatment = 1, candgender = 1, candrace = 1, candideolorder = 0, candpolicy = 1)

gender.controlfull <- sim(m1, x = predprob1, x1 = predprob2, num = 100000)
summary(gender.controlfull)

gender.treatfull <- sim(m1, x = predprob3, x1 = predprob4, num = 1000)
summary(gender.treatfull)

genderdiff.full <- c(.08, .072)
genderdiff.fullbar <- genderdiff.full - c(.021, .017)


#All respondents, race
predprob1 <- setx(m1, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1) #Get predicted probabilities for overall race
predprob2 <- setx(m1, treatment = 0, candgender = 0, candrace = 2, candideolorder = 0, candpolicy = 1)
predprob3 <- setx(m1, treatment = 0, candgender = 0, candrace = 3, candideolorder = 0, candpolicy = 1)
predprob4 <- setx(m1, treatment = 0, candgender = 0, candrace = 4, candideolorder = 0, candpolicy = 1)
predprob5 <- setx(m1, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob6 <- setx(m1, treatment = 1, candgender = 0, candrace = 2, candideolorder = 0, candpolicy = 1)
predprob7 <- setx(m1, treatment = 1, candgender = 0, candrace = 3, candideolorder = 0, candpolicy = 1)
predprob8 <- setx(m1, treatment = 1, candgender = 0, candrace = 4, candideolorder = 0, candpolicy = 1)

raceasian.controlfull <- sim(m1, x = predprob1, x1 = predprob2, num = 100000)
summary(raceasian.controlfull)

raceasian.treatfull <- sim(m1, x = predprob5, x1 = predprob6, num = 100000)
summary(raceasian.treatfull)

racehispanic.controlfull <- sim(m1, x = predprob1, x1 = predprob3, num = 100000)
summary(racehispanic.controlfull)

racehispanic.treatfull <- sim(m1, x = predprob5, x1 = predprob7, num = 100000)
summary(racehispanic.treatfull)

raceblack.controlfull <- sim(m1, x = predprob1, x1 = predprob4, num = 100000)
summary(raceblack.controlfull)

raceblack.treatfull <- sim(m1, x = predprob5, x1 = predprob8, num = 100000)
summary(raceblack.treatfull)


racediff.full <- c(-0.08, -.062, -.133, -.048, -.028, .003)
racediff.fullbar <- racediff.full- c(-.174, -.147, -.222, -.131, -.113, -.079)



#All respondents, ideology
predprob1 <- setx(m1, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1) #Get predicted probabilities for overall ideology
predprob2 <- setx(m1, treatment = 0, candgender = 0, candrace = 1, candideolorder = 1, candpolicy = 1)
predprob3 <- setx(m1, treatment = 0, candgender = 0, candrace = 1, candideolorder = 2, candpolicy = 1)
predprob4 <- setx(m1, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob5 <- setx(m1, treatment = 1, candgender = 0, candrace = 1, candideolorder = 1, candpolicy = 1)
predprob6 <- setx(m1, treatment = 1, candgender = 0, candrace = 1, candideolorder = 2, candpolicy = 1)

ideologycon.controlfull <- sim(m1, x = predprob1, x1 = predprob2, num = 100000)
summary(ideologycon.controlfull)

ideologycon.treatfull <- sim(m1, x = predprob4, x1 = predprob5, num = 100000)
summary(ideologycon.treatfull)

ideologylib.controlfull <- sim(m1, x = predprob1, x1 = predprob3, num = 100000)
summary(ideologylib.controlfull)

ideologylib.treatfull <- sim(m1, x = predprob4, x1 = predprob6, num = 100000)
summary(ideologylib.treatfull)


ideologydiff.full <- c(-.082, -.161, -.113, -.128)
ideologydiff.fullbar <- ideologydiff.full - c(-.163, -.239, -.192, -.206)



#All respondents, policy
predprob1 <- setx(m1, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob2 <- setx(m1, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 3)
predprob3 <- setx(m1, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 2)
predprob4 <- setx(m1, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 4)
predprob5 <- setx(m1, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob6 <- setx(m1, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 3)
predprob7 <- setx(m1, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 2)
predprob8 <- setx(m1, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 4)


policyreform.controlfull <- sim(m1, x = predprob1, x1 = predprob2, num = 100000)
summary(policyreform.controlfull)

policyreform.treatfull <- sim(m1, x = predprob5, x1 = predprob6, num = 100000)
summary(policyreform.treatfull)

policyfairjob.controlfull <- sim(m1, x = predprob1, x1 = predprob3, num = 100000)
summary(policyfairjob.controlfull)

policyfairjob.treatfull <- sim(m1, x = predprob5, x1 = predprob7, num = 100000)
summary(policyfairjob.treatfull)

policyfairprison.controlfull <- sim(m1, x = predprob1, x1 = predprob4, num = 100000)
summary(policyfairprison.controlfull)

policyfairprison.treatfull <- sim(m1, x = predprob5, x1 = predprob8, num = 100000)
summary(policyfairprison.treatfull)


policydiff.full <- c(-.102, -.203, -.107, -.127, -.112, -.208)
policydiff.fullbar <- policydiff.full - c(-.192, -.298, -.203, -.217, -.203, -.301)


all.predprob <- c(policydiff.full, ideologydiff.full, racediff.full, genderdiff.full)
all.ci <- c(policydiff.fullbar, ideologydiff.fullbar, racediff.fullbar, genderdiff.fullbar)





#Get predicted probabilities for men respondents

#men respondents, gender
predprob1 <- setx(m2, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1) #Get predicted probabilities for overmen gender
predprob2 <- setx(m2, treatment = 0, candgender = 1, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob3 <- setx(m2, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob4 <- setx(m2, treatment = 1, candgender = 1, candrace = 1, candideolorder = 0, candpolicy = 1)

gender.controlmen <- sim(m2, x = predprob1, x1 = predprob2, num = 100000)
summary(gender.controlmen)

gender.treatmen <- sim(m2, x = predprob3, x1 = predprob4, num = 1000)
summary(gender.treatmen)

genderdiff.men <- c(.010, .061)
genderdiff.menbar <- genderdiff.men - c(-.056, -.023)


#men respondents, race
predprob1 <- setx(m2, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1) #Get predicted probabilities for overmen race
predprob2 <- setx(m2, treatment = 0, candgender = 0, candrace = 2, candideolorder = 0, candpolicy = 1)
predprob3 <- setx(m2, treatment = 0, candgender = 0, candrace = 3, candideolorder = 0, candpolicy = 1)
predprob4 <- setx(m2, treatment = 0, candgender = 0, candrace = 4, candideolorder = 0, candpolicy = 1)
predprob5 <- setx(m2, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob6 <- setx(m2, treatment = 1, candgender = 0, candrace = 2, candideolorder = 0, candpolicy = 1)
predprob7 <- setx(m2, treatment = 1, candgender = 0, candrace = 3, candideolorder = 0, candpolicy = 1)
predprob8 <- setx(m2, treatment = 1, candgender = 0, candrace = 4, candideolorder = 0, candpolicy = 1)

raceasian.controlmen <- sim(m2, x = predprob1, x1 = predprob2, num = 100000)
summary(raceasian.controlmen)

raceasian.treatmen <- sim(m2, x = predprob5, x1 = predprob6, num = 100000)
summary(raceasian.treatmen)

racehispanic.controlmen <- sim(m2, x = predprob1, x1 = predprob3, num = 100000)
summary(racehispanic.controlmen)

racehispanic.treatmen <- sim(m2, x = predprob5, x1 = predprob7, num = 100000)
summary(racehispanic.treatmen)

raceblack.controlmen <- sim(m2, x = predprob1, x1 = predprob4, num = 100000)
summary(raceblack.controlmen)

raceblack.treatmen <- sim(m2, x = predprob5, x1 = predprob8, num = 100000)
summary(raceblack.treatmen)


racediff.men <- c(-.115, -.102, -.181, -.075, -.116, -.041)
racediff.menbar <- racediff.men- c(-.231, -.221, -.301, -.201, -.221, -.160)



#men respondents, ideology
predprob1 <- setx(m2, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1) #Get predicted probabilities for overmen ideology
predprob2 <- setx(m2, treatment = 0, candgender = 0, candrace = 1, candideolorder = 1, candpolicy = 1)
predprob3 <- setx(m2, treatment = 0, candgender = 0, candrace = 1, candideolorder = 2, candpolicy = 1)
predprob4 <- setx(m2, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob5 <- setx(m2, treatment = 1, candgender = 0, candrace = 1, candideolorder = 1, candpolicy = 1)
predprob6 <- setx(m2, treatment = 1, candgender = 0, candrace = 1, candideolorder = 2, candpolicy = 1)

ideologycon.controlmen <- sim(m2, x = predprob1, x1 = predprob2, num = 100000)
summary(ideologycon.controlmen)

ideologycon.treatmen <- sim(m2, x = predprob4, x1 = predprob5, num = 100000)
summary(ideologycon.treatmen)

ideologylib.controlmen <- sim(m2, x = predprob1, x1 = predprob3, num = 100000)
summary(ideologylib.controlmen)

ideologylib.treatmen <- sim(m2, x = predprob4, x1 = predprob6, num = 100000)
summary(ideologylib.treatmen)


ideologydiff.men <- c(-.055, -.113, -.093, -.067)
ideologydiff.menbar <- ideologydiff.men - c(-.143, -.221, -.186, -.175)



#men respondents, policy
predprob1 <- setx(m2, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob2 <- setx(m2, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 3)
predprob3 <- setx(m2, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 2)
predprob4 <- setx(m2, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 4)
predprob5 <- setx(m2, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob6 <- setx(m2, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 3)
predprob7 <- setx(m2, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 2)
predprob8 <- setx(m2, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 4)


policyreform.controlmen <- sim(m2, x = predprob1, x1 = predprob2, num = 100000)
summary(policyreform.controlmen)

policyreform.treatmen <- sim(m2, x = predprob5, x1 = predprob6, num = 100000)
summary(policyreform.treatmen)

policyfairjob.controlmen <- sim(m2, x = predprob1, x1 = predprob3, num = 100000)
summary(policyfairjob.controlmen)

policyfairjob.treatmen <- sim(m2, x = predprob5, x1 = predprob7, num = 100000)
summary(policyfairjob.treatmen)

policyfairprison.controlmen <- sim(m2, x = predprob1, x1 = predprob4, num = 100000)
summary(policyfairprison.controlmen)

policyfairprison.treatmen <- sim(m2, x = predprob5, x1 = predprob8, num = 100000)
summary(policyfairprison.treatmen)


policydiff.men <- c(-.175, -.119, -.154, -.094, -.144, -.188)
policydiff.menbar <- policydiff.men - c(-.281, -.246, -.286, -.224, -.259, -.317)


men.predprob <- c(policydiff.men, ideologydiff.men, racediff.men, genderdiff.men)
men.ci <- c(policydiff.menbar, ideologydiff.menbar, racediff.menbar, genderdiff.menbar)









#Get predicted probabilities for women respondents

#women respondents, gender
predprob1 <- setx(m3, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1) #Get predicted probabilities for overwomen gender
predprob2 <- setx(m3, treatment = 0, candgender = 1, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob3 <- setx(m3, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob4 <- setx(m3, treatment = 1, candgender = 1, candrace = 1, candideolorder = 0, candpolicy = 1)

gender.controlwomen <- sim(m3, x = predprob1, x1 = predprob2, num = 100000)
summary(gender.controlwomen)

gender.treatwomen <- sim(m3, x = predprob3, x1 = predprob4, num = 1000)
summary(gender.treatwomen)

genderdiff.women <- c(.17, .081)
genderdiff.womenbar <- genderdiff.women - c(.073, .012)


#women respondents, race
predprob1 <- setx(m3, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1) #Get predicted probabilities for overwomen race
predprob2 <- setx(m3, treatment = 0, candgender = 0, candrace = 2, candideolorder = 0, candpolicy = 1)
predprob3 <- setx(m3, treatment = 0, candgender = 0, candrace = 3, candideolorder = 0, candpolicy = 1)
predprob4 <- setx(m3, treatment = 0, candgender = 0, candrace = 4, candideolorder = 0, candpolicy = 1)
predprob5 <- setx(m3, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob6 <- setx(m3, treatment = 1, candgender = 0, candrace = 2, candideolorder = 0, candpolicy = 1)
predprob7 <- setx(m3, treatment = 1, candgender = 0, candrace = 3, candideolorder = 0, candpolicy = 1)
predprob8 <- setx(m3, treatment = 1, candgender = 0, candrace = 4, candideolorder = 0, candpolicy = 1)

raceasian.controlwomen <- sim(m3, x = predprob1, x1 = predprob2, num = 100000)
summary(raceasian.controlwomen)

raceasian.treatwomen <- sim(m3, x = predprob5, x1 = predprob6, num = 100000)
summary(raceasian.treatwomen)

racehispanic.controlwomen <- sim(m3, x = predprob1, x1 = predprob3, num = 100000)
summary(racehispanic.controlwomen)

racehispanic.treatwomen <- sim(m3, x = predprob5, x1 = predprob7, num = 100000)
summary(racehispanic.treatwomen)

raceblack.controlwomen <- sim(m3, x = predprob1, x1 = predprob4, num = 100000)
summary(raceblack.controlwomen)

raceblack.treatwomen <- sim(m3, x = predprob5, x1 = predprob8, num = 100000)
summary(raceblack.treatwomen)


racediff.women <- c(-.013, -.026, -.073, -.033, .079, .047)
racediff.womenbar <- racediff.women- c(-.158, -.141, -.201, -.138, -.051, -.055)



#women respondents, ideology
predprob1 <- setx(m3, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1) #Get predicted probabilities for overwomen ideology
predprob2 <- setx(m3, treatment = 0, candgender = 0, candrace = 1, candideolorder = 1, candpolicy = 1)
predprob3 <- setx(m3, treatment = 0, candgender = 0, candrace = 1, candideolorder = 2, candpolicy = 1)
predprob4 <- setx(m3, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob5 <- setx(m3, treatment = 1, candgender = 0, candrace = 1, candideolorder = 1, candpolicy = 1)
predprob6 <- setx(m3, treatment = 1, candgender = 0, candrace = 1, candideolorder = 2, candpolicy = 1)

ideologycon.controlwomen <- sim(m3, x = predprob1, x1 = predprob2, num = 100000)
summary(ideologycon.controlwomen)

ideologycon.treatwomen <- sim(m3, x = predprob4, x1 = predprob5, num = 100000)
summary(ideologycon.treatwomen)

ideologylib.controlwomen <- sim(m3, x = predprob1, x1 = predprob3, num = 100000)
summary(ideologylib.controlwomen)

ideologylib.treatwomen <- sim(m3, x = predprob4, x1 = predprob6, num = 100000)
summary(ideologylib.treatwomen)


ideologydiff.women <- c(-.091, -.211, -.122, -.186)
ideologydiff.womenbar <- ideologydiff.women - c(-.212, -.325, -.24, -.30)



#women respondents, policy
predprob1 <- setx(m3, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob2 <- setx(m3, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 3)
predprob3 <- setx(m3, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 2)
predprob4 <- setx(m3, treatment = 0, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 4)
predprob5 <- setx(m3, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 1)
predprob6 <- setx(m3, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 3)
predprob7 <- setx(m3, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 2)
predprob8 <- setx(m3, treatment = 1, candgender = 0, candrace = 1, candideolorder = 0, candpolicy = 4)


policyreform.controlwomen <- sim(m3, x = predprob1, x1 = predprob2, num = 100000)
summary(policyreform.controlwomen)

policyreform.treatwomen <- sim(m3, x = predprob5, x1 = predprob6, num = 100000)
summary(policyreform.treatwomen)

policyfairjob.controlwomen <- sim(m3, x = predprob1, x1 = predprob3, num = 100000)
summary(policyfairjob.controlwomen)

policyfairjob.treatwomen <- sim(m3, x = predprob5, x1 = predprob7, num = 100000)
summary(policyfairjob.treatwomen)

policyfairprison.controlwomen <- sim(m3, x = predprob1, x1 = predprob4, num = 100000)
summary(policyfairprison.controlwomen)

policyfairprison.treatwomen <- sim(m3, x = predprob5, x1 = predprob8, num = 100000)
summary(policyfairprison.treatwomen)


policydiff.women <- c(-.004, -.307, -.037, -.16, -.043, -.225)
policydiff.womenbar <- policydiff.women - c(-.152, -.447, -.174, -.284, -.182, -.357)


women.predprob <- c(policydiff.women, ideologydiff.women, racediff.women, genderdiff.women)
women.ci <- c(policydiff.womenbar, ideologydiff.womenbar, racediff.womenbar, genderdiff.womenbar)


fullyvalue <- c(1, 1.5, 2.2, 4.2, 4.7, 5.4, 7.4, 7.9, 8.6,9.6, 12.6, 13.1, 13.8,15.8, 16.3, 17.0, 18.0,    21.0, 21.5, 22.2, 24.2, 24.7, 25.4,   27.4, 27.9, 28.6,   29.6,   32.6, 33.1, 33.8,   34.8)
pointyvalue <- c(1, 1.5, 4.2, 4.7, 7.4, 7.9, 12.6, 13.1, 15.8, 16.3, 21.0, 21.5, 24.2, 24.7, 27.4, 27.9, 32.6, 33.1)
groupyvalue <- c(2.2, 5.4, 8.6, 13.8, 17.0, 22.2, 25.4, 28.6, 33.8)
labyvalue <- c(9.6, 18.0, 29.6, 34.8)



par(mar=c(3, .45, 2, .45), oma = c(5, 12.0, 0, 0))
par(mfrow = c(1,3))
par(family = "serif")
plotCI(all.predprob, pointyvalue, xlab = "", uiw = all.ci, sfrac = .002, err = "x", lwd = 1, gap = 0, pch = c(21, 16), yaxt = "n", ylab = "", main = "All Respondents", xlim = c(-.50, .50), ylim = c(.5, 35.3), cex.axis = .8, cex.main = 1)
text(-.53, pointyvalue, pos = 2, c("Control", "Treatment", "Control", "Treatment","Control", "Treatment","Control", "Treatment","Control", "Treatment","Control", "Treatment") , srt = 360, xpd = NA)
text(-.53, groupyvalue, c("Fair Sentencing", "Fair Employment", "Justice Reform", "Very Liberal", "Slightly Liberal", "African American", "Hispanic", "Asian American", "Female"), pos = 2, font = 3, xpd = NA)
text(-.53, labyvalue, c("Policy (Good Jobs Baseline)", "Ideology (Moderate Baseline)", "Race (White Basline)", "Gender (Male Baseline)"), pos = 2, xpd = NA, font = 2)
abline(v = 0, lty = 2)



plotCI(men.predprob, pointyvalue, xlab = "", uiw = men.ci, sfrac = .002, err = "x", lwd = 1, gap = 0, pch = c(21, 16), yaxt = "n", ylab = "", main = "Men", xlim = c(-.50, .50), ylim = c(.5, 35.3), cex.axis = .8, cex.main = 1)
abline(v = 0, lty = 2)
legend(-.71, -3.35, c("With Identity Politics Loss Narrative", "Without Identity Politics Loss Narrative ", expression(paste("Significant Effect, ", italic("p"), " < .10 "))), col = c("black"), pch = c(16, 21, 8), xpd = NA)


plotCI(women.predprob, pointyvalue, xlab = "", uiw = women.ci, sfrac = .002, err = "x", lwd = 1, gap = 0, pch = c(21, 8, 21, 8, 21, 16, 21, 16, 21, 16, 21, 16, 21, 16, 21, 16, 21, 16), yaxt = "n", ylab = "", main = "Women", xlim = c(-.50, .50), ylim = c(.5, 35.3), cex.axis = .8, cex.main = 1)
abline(v = 0, lty = 2)

