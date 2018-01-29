#----------------------------------------------------------------------------------------------------#
# Created by: Drew Engelhardt 6-26-17
# Last modified by: Drew Engelhardt 6-26-17
# Filename: cjs_cleaning
# File purpose: Cleaning file for SSI data
#----------------------------------------------------------------------------------------------------#
setwd("~/Dropbox/ClassDivision_CarceralState/Data/SSI2017")

cjs.df <- read.csv("data_weight.csv", stringsAsFactors = F)
cjs.df <- cjs.df[,-1]

## Recoding
# race
cjs.df$black <- ifelse(cjs.df$race == 2, 1, 0)

# Open when coding
library(survey)
# d.all <- svydesign(~1, data = cjs.df, weights = ~wts_whole)
# d.wht <- svydesign(~1, data = subset(cjs.df, black == 0), weights = ~wts_white)
# d.blk <- svydesign(~1, data = subset(cjs.df, black == 1), weights = ~wts_black)

### SES
cjs.df$inc <- cjs.df$C011/11
cjs.df$chood.class <- cjs.df$C010

## income terciles
# full sample
# prop.table(svytable(~ C011, d.all))
cjs.df$inc3 <- NA
cjs.df$inc3[which(cjs.df$C011 <= 2)] <- 1
cjs.df$inc3[which(cjs.df$C011 >= 3 & cjs.df$C011 <= 6)] <- 2 # based on suvey weighting
cjs.df$inc3[which(cjs.df$C011 >= 7)] <- 3

# prop.table(svytable(~inc3, d.all))

# intra-racial group samples
# white 
# prop.table(svytable(~ C011, d.wht))
cjs.df$inc3.rac <- NA
cjs.df$inc3.rac[which(cjs.df$C011 <= 3 & cjs.df$black == 0)] <- 1
cjs.df$inc3.rac[which((cjs.df$C011 >= 4 & cjs.df$C011 <= 7) & cjs.df$black == 0)] <- 2 # based on suvey weighting
cjs.df$inc3.rac[which(cjs.df$C011 >= 8 & cjs.df$black == 0)] <- 3
# black
# prop.table(svytable(~ C011, d.blk))
cjs.df$inc3.rac[which(cjs.df$C011 <= 1 & cjs.df$black == 1)] <- 1
cjs.df$inc3.rac[which((cjs.df$C011 >= 2 & cjs.df$C011 <= 4) & cjs.df$black == 1)] <- 2 # based on suvey weighting
cjs.df$inc3.rac[which(cjs.df$C011 >= 5 & cjs.df$black == 1)] <- 3


## Education terciles
# full sample
# prop.table(svytable(~ edu, d.all))
cjs.df$educ3 <- NA
cjs.df$educ3[which(cjs.df$edu <= 1)] <- 1 #HS or less
cjs.df$educ3[which(cjs.df$edu == 2 | cjs.df$edu == 3)] <- 2 # based on suvey weighting
cjs.df$educ3[which(cjs.df$edu >= 4)] <- 3 # College degree or postgrad

# prop.table(svytable(~educ3, d.all))

# intra-racial group samples
# white 
# prop.table(svytable(~ edu, d.wht))
cjs.df$educ3.rac <- NA
cjs.df$educ3.rac[which(cjs.df$edu <= 1 & cjs.df$black == 0)] <- 1
cjs.df$educ3.rac[which((cjs.df$edu == 2 | cjs.df$edu == 3) & cjs.df$black == 0)] <- 2 # based on suvey weighting
cjs.df$educ3.rac[which(cjs.df$edu >= 4 & cjs.df$black == 0)] <- 3
# black
# prop.table(svytable(~ edu, d.blk))
cjs.df$educ3.rac[which(cjs.df$edu <= 1 & cjs.df$black == 1)] <- 1
cjs.df$educ3.rac[which((cjs.df$edu == 2 | cjs.df$edu == 3) & cjs.df$black == 1)] <- 2 # based on suvey weighting
cjs.df$educ3.rac[which(cjs.df$edu >= 4 & cjs.df$black == 1)] <- 3

cjs.df$class <- NA
cjs.df$class <- (cjs.df$inc3 + cjs.df$educ3 - 2)/4
cjs.df$class3 <- NA
cjs.df$class3[which(cjs.df$educ3 == 1 & cjs.df$inc3 == 1)] <- 1
cjs.df$class3[which(cjs.df$educ3 == 2 & cjs.df$inc3 == 2)] <- 2
cjs.df$class3[which(cjs.df$educ3 == 3 & cjs.df$inc3 == 3)] <- 3
cjs.df$class.rac <- NA
cjs.df$class.rac <- (cjs.df$inc3.rac + cjs.df$educ3.rac - 2)/4
# chisq.test(prop.table(svytable(~class + black, d.all), 2))
# chisq.test(prop.table(svytable(~class.rac + black, d.all), 2))

cjs.df$age <- NA
cjs.df$age <- cjs.df$birthyr-30
cjs.df$age[which(cjs.df$birthyr == 122)] <- 68
cjs.df$age[which(cjs.df$birthyr == 123)] <- 69
cjs.df$age[which(cjs.df$birthyr == 124)] <- 70
cjs.df$age <- ((cjs.df$age - 70)*-1)/70

#Stopped by Police
cjs.df$pol.stop <- NA
cjs.df$pol.stop <- cjs.df$D001/3

#Dangerous Police Encounter (excludes those never stopped)
cjs.df$pol.danger <- NA
cjs.df$pol.danger <- cjs.df$D003

#Ask police for help
cjs.df$pol.help <- NA
cjs.df$pol.help <- cjs.df$D004/3

#Unfair police interactions (excludes those never stopped or helped)
cjs.df$pol.unfair <- NA
cjs.df$pol.unfair <- cjs.df$D005

#Police Experience (of those whove had experiences (90% of blacks, 93% whites))
cjs.df$pol.experience <- NA
cjs.df$pol.experience[which(cjs.df$D006 < 9)] <- (cjs.df$D006[which(cjs.df$D006 < 9)]*-1+2)/2

### Spec 1: stops and unfair danger (remove people asking for help on the unfair item?)
cjs.df$stop.poor <- NA
cjs.df$stop.poor[which(cjs.df$pol.stop == 0)] <- 0
cjs.df$stop.poor[which(cjs.df$pol.stop > 0 & cjs.df$pol.danger == 0 & cjs.df$pol.unfair == 0)] <- 0.5
cjs.df$stop.poor[which(cjs.df$pol.stop > 0 & (cjs.df$pol.danger == 1 | cjs.df$pol.unfair == 1))] <- 1


### Spec 2: stops and unfair danger
cjs.df$stop.poor2 <- NA
cjs.df$stop.poor2[which(cjs.df$pol.stop == 0)] <- 0
cjs.df$stop.poor2[which(cjs.df$pol.stop == 1/3 & cjs.df$pol.danger == 0 & cjs.df$pol.unfair == 0)] <- 1/3
cjs.df$stop.poor2[which(cjs.df$pol.stop >= 2/3 & cjs.df$pol.danger == 0 & cjs.df$pol.unfair == 0)] <- 2/3
cjs.df$stop.poor2[which(cjs.df$pol.stop > 0 & (cjs.df$pol.danger == 1 | cjs.df$pol.unfair == 1))] <- 1


# Crime victim
cjs.df$crim.vict <- NA
cjs.df$crim.vict[which(cjs.df$A002 == 1)] <- 1
cjs.df$crim.vict[which(cjs.df$A002 == 0)] <- 0

# Prison time
cjs.df$prison.time <- NA
cjs.df$prison.time[which(cjs.df$D010 == 1)] <- 1
cjs.df$prison.time[which(cjs.df$D010 == 0)] <- 0

# Interactions with Police
cjs.df$pol.mistreat <- NA
cjs.df$pol.mistreat <- cjs.df$I001/3

# Friends/Family w/convictions
cjs.df$peer.felony <- NA
cjs.df$peer.felony <- cjs.df$I002/3

# Felony conviction self
cjs.df$self.felony <- NA
cjs.df$self.felony <- ifelse(cjs.df$D011 == 1, 1, 0)


## Employment
# govt
cjs.df$employ.gov <- NA
cjs.df$employ.gov <- ifelse(cjs.df$C003 > 0, 1, 0)

# cjs
cjs.df$employ.cjs <- cjs.df$C004
cjs.df$employ.cjs[which(cjs.df$C002 == 0)] <- 0
cjs.df$cjs.pos <- cjs.df$C005


## Racial Group attitudes
# RR
cjs.df$R001_resent_generations.r <- NA
cjs.df$R001_resent_generations.r[which(cjs.df$R001_resent_generations == 4)] <- 0
cjs.df$R001_resent_generations.r[which(cjs.df$R001_resent_generations == 3)] <- 1
cjs.df$R001_resent_generations.r[which(cjs.df$R001_resent_generations == 2)] <- 2
cjs.df$R001_resent_generations.r[which(cjs.df$R001_resent_generations == 1)] <- 3
cjs.df$R001_resent_generations.r[which(cjs.df$R001_resent_generations == 0)] <- 4

cjs.df$R001_resent_over.r <- NA
cjs.df$R001_resent_over.r[which(cjs.df$R001_resent_over == 4)] <- 0
cjs.df$R001_resent_over.r[which(cjs.df$R001_resent_over == 3)] <- 1
cjs.df$R001_resent_over.r[which(cjs.df$R001_resent_over == 2)] <- 2
cjs.df$R001_resent_over.r[which(cjs.df$R001_resent_over == 1)] <- 3
cjs.df$R001_resent_over.r[which(cjs.df$R001_resent_over == 0)] <- 4

cjs.df$rr_sc <- (cjs.df$R001_resent_generations.r + cjs.df$R001_resent_over.r + cjs.df$R001_resent_irish + cjs.df$R001_resent_itsreally)/16
# x <- subset(cjs.df, black == 1)
# cronbach(cbind(x$R001_resent_generations.r, x$R001_resent_over.r, x$R001_resent_irish, x$R001_resent_itsreally))

# Linked Fate
cjs.df$blk.lfate <- NA
cjs.df$blk.lfate[which(cjs.df$R002 == 0)] <- 0
cjs.df$blk.lfate[which(cjs.df$R003 == 0)] <- 1
cjs.df$blk.lfate[which(cjs.df$R003 == 1)] <- 2
cjs.df$blk.lfate[which(cjs.df$R003 == 2)] <- 3
cjs.df$blk.lfate.sc <- cjs.df$blk.lfate/3

cjs.df$wht.lfate <- NA
cjs.df$wht.lfate[which(cjs.df$R004 == 0)] <- 0
cjs.df$wht.lfate[which(cjs.df$R004B == 0)] <- 1
cjs.df$wht.lfate[which(cjs.df$R004B == 1)] <- 2
cjs.df$wht.lfate[which(cjs.df$R004B == 2)] <- 3
cjs.df$wht.lfate.sc <- cjs.df$wht.lfate/3

cjs.df$lfate <- NA
cjs.df$lfate[which(cjs.df$black == 1)] <- cjs.df$blk.lfate.sc[which(cjs.df$black == 1)]
cjs.df$lfate[which(cjs.df$black == 0)] <- cjs.df$wht.lfate.sc[which(cjs.df$black == 0)]

cjs.df$pid7 <- cjs.df$C015/6
cjs.df$pid3 <- NA
cjs.df$pid3[which(cjs.df$pid7 < .5)] <- -1
cjs.df$pid3[which(cjs.df$pid7 == .5)] <- 0
cjs.df$pid3[which(cjs.df$pid7 > .5)] <- 1

cjs.df$ideo <- ifelse(cjs.df$C014 < 9, cjs.df$C014, NA)
cjs.df$ideo[which(cjs.df$C014 == 9)] <- 2
cjs.df$ideo_sc <- cjs.df$ideo/4

#----------------------------------------------------------------------------------------------------#
# Recoding outcomes
#----------------------------------------------------------------------------------------------------#
# p.crim.solve, p.viol.crim, p.race.fair, p.exces.force, p.account, police.rate.sc
# Police Ratings (excellent - poor)
cjs.df$p.crim.solve <- abs(cjs.df$A005_1 - 4)/4
cjs.df$p.viol.crim <- abs(cjs.df$A005_2 - 4)/4
cjs.df$p.race.fair <- abs(cjs.df$A005_3 - 4)/4
cjs.df$p.exces.force <- abs(cjs.df$A005_4 - 4)/4
cjs.df$p.account <- abs(cjs.df$A005_5 - 4)/4

cjs.df$p.race.fair.bin <- NA
cjs.df$p.race.fair.bin[which(cjs.df$p.race.fair < 1)] <- 0
cjs.df$p.race.fair.bin[which(cjs.df$p.race.fair == 1)] <- 1
cjs.df$p.exces.force.bin <- NA
cjs.df$p.exces.force.bin[which(cjs.df$p.exces.force < 1)] <- 0
cjs.df$p.exces.force.bin[which(cjs.df$p.exces.force == 1)] <- 1
cjs.df$p.account.bin <- NA
cjs.df$p.account.bin[which(cjs.df$p.account < 1)] <- 0
cjs.df$p.account.bin[which(cjs.df$p.account == 1)] <- 1

cjs.df$p.race.fair.med <- NA
cjs.df$p.race.fair.med[which(cjs.df$p.race.fair < .75)] <- 0
cjs.df$p.race.fair.med[which(cjs.df$p.race.fair >= .75)] <- 1
cjs.df$p.exces.force.med <- NA
cjs.df$p.exces.force.med[which(cjs.df$p.exces.force < .75)] <- 0
cjs.df$p.exces.force.med[which(cjs.df$p.exces.force >= .75)] <- 1
cjs.df$p.account.med <- NA
cjs.df$p.account.med[which(cjs.df$p.account < .75)] <- 0
cjs.df$p.account.med[which(cjs.df$p.account >= .75)] <- 1

cjs.df$police.rate <- NA
cjs.df$police.rate.sc <- (cjs.df$p.crim.solve + cjs.df$p.viol.crim + cjs.df$p.race.fair + cjs.df$p.exces.force + cjs.df$p.account)/5

cjs.df$police.account <- NA
cjs.df$police.account.sc <- (cjs.df$p.race.fair + cjs.df$p.exces.force + cjs.df$p.account)/3

cjs.df$police.account.bin <- NA
cjs.df$police.account.bin.sc <- (cjs.df$p.race.fair.bin + cjs.df$p.exces.force.bin + cjs.df$p.account.bin)/3

cjs.df$police.account.med <- NA
cjs.df$police.account.med.sc <- (cjs.df$p.race.fair.med + cjs.df$p.exces.force.med + cjs.df$p.account.med)/3


# library(psych)
# alpha(cbind(cjs.df$p.crim.solve, cjs.df$p.viol.crim, cjs.df$p.race.fair, cjs.df$p.exces.force, cjs.df$p.account))

# Court fairness (lots to no confiedene)
cjs.df$court.fair <- NA
cjs.df$court.fair[which(cjs.df$A006_r == "law?")] <- cjs.df$A006[which(cjs.df$A006_r == "law?")]
cjs.df$court.fair <- abs(cjs.df$court.fair - 3)/3

cjs.df$court.fair.race <- NA
cjs.df$court.fair.race[which(cjs.df$A006_r == "law regardless of a person\x89۪s race?")] <- cjs.df$A006[which(cjs.df$A006_r == "law regardless of a person\x89۪s race?")]
cjs.df$court.fair.race <- abs(cjs.df$court.fair.race - 3)/3

cjs.df$court.fair.class <- NA
cjs.df$court.fair.class[which(cjs.df$A006_r == "law regardless of a person\x89۪s class?")] <- cjs.df$A006[which(cjs.df$A006_r == "law regardless of a person\x89۪s class?")]
cjs.df$court.fair.class <- abs(cjs.df$court.fair.class - 3)/3

cjs.df$court.fair.treat <- NA
cjs.df$court.fair.treat[which(cjs.df$A006_r == "law?")] <- "Control"
cjs.df$court.fair.treat[which(cjs.df$A006_r == "law regardless of a person\x89۪s race?")] <- "Race"
cjs.df$court.fair.treat[which(cjs.df$A006_r == "law regardless of a person\x89۪s class?")] <- "Class"
cjs.df$court.fair.treat <- as.factor(cjs.df$court.fair.treat)
cjs.df$court.fair.treat <- factor(cjs.df$court.fair.treat, levels = c("Control", "Race", "Class"))

cjs.df$court.fair.all <- NA
cjs.df$court.fair.all[which(cjs.df$court.fair.treat == "Control")] <- cjs.df$court.fair[which(cjs.df$court.fair.treat == "Control")]
cjs.df$court.fair.all[which(cjs.df$court.fair.treat == "Race")] <- cjs.df$court.fair.race[which(cjs.df$court.fair.treat == "Race")] 
cjs.df$court.fair.all[which(cjs.df$court.fair.treat == "Class")] <- cjs.df$court.fair.class[which(cjs.df$court.fair.treat == "Class")]

# Police Violence and Respect
cjs.df$respect.police <- NA
cjs.df$respect.police <- cjs.df$A007

# Police Negligence/Corruption bad apples
cjs.df$pol.badapples <- NA
cjs.df$pol.badapples[which(cjs.df$A008 == 1)] <- 1
cjs.df$pol.badapples[which(cjs.df$A008 == 3)] <- 2
cjs.df$pol.badapples[which(cjs.df$A008 == 2)] <- 3

## Redefining survey objects
d.all <- svydesign(~1, data = cjs.df, weights = ~wts_whole)
d.wht <- svydesign(~1, data = subset(cjs.df, black == 0), weights = ~wts_white)
d.blk <- svydesign(~1, data = subset(cjs.df, black == 1), weights = ~wts_black)

# library(lavaan)
# 
# m1 <- '# latent variable definitions
# factor_1 =~ NA*p.crim.solve + p.viol.crim + p.race.fair + p.exces.force + p.account
# factor_1 ~~ 1*factor_1
# '
# m2 <- '# latent variable definitions
# factor_1 =~ NA*p.crim.solve + p.viol.crim
# factor_2 =~ NA*p.race.fair + p.exces.force + p.account
# factor_1 ~~ 1*factor_1
# factor_2 ~~ 1*factor_2
# '
# 
# fit1 <- cfa(m1, data=w.df, mimic = "Mplus")
# summary(fit1, fit.measures = T, standardized = T)
# fit2 <- cfa(m2, data=w.df, mimic = "Mplus")
# summary(fit2, fit.measures = T, standardized = T)
# 
# fit2 <- cfa(m2, data=b.df, mimic = "Mplus")
# summary(fit2, fit.measures = T, standardized = T)