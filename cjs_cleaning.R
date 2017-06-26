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
cjs.df$inc <- cjs.df$C011
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
cjs.df$educ3[which(cjs.df$edu <= 1)] <- 1
cjs.df$educ3[which(cjs.df$edu == 2 | cjs.df$edu == 3)] <- 2 # based on suvey weighting
cjs.df$educ3[which(cjs.df$edu >= 4)] <- 3

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
cjs.df$class.rac <- NA
cjs.df$class.rac <- (cjs.df$inc3.rac + cjs.df$educ3.rac - 2)/4
# chisq.test(prop.table(svytable(~class + black, d.all), 2))
# chisq.test(prop.table(svytable(~class.rac + black, d.all), 2))

# Interactions with Police
cjs.df$pol.mistreat <- NA
cjs.df$pol.mistreat <- cjs.df$I001

# Friends/Family w/convictions
cjs.df$peer.felony <- NA
cjs.df$peer.felony <- cjs.df$I002


# Employment
# govt
cjs.df$employ.gov <- NA
cjs.df$employ.gov <- ifelse(cjs.df$C003 > 0, 1, 0)

# cjs
cjs.df$employ.cjs <- cjs.df$C004
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

cjs.df$wht.lfate <- NA
cjs.df$wht.lfate[which(cjs.df$R004 == 0)] <- 0
cjs.df$wht.lfate[which(cjs.df$R004B == 0)] <- 1
cjs.df$wht.lfate[which(cjs.df$R004B == 1)] <- 2
cjs.df$wht.lfate[which(cjs.df$R004B == 2)] <- 3

#----------------------------------------------------------------------------------------------------#
# Recoding outcomes
#----------------------------------------------------------------------------------------------------#
# Police Ratings
cjs.df$p.crim.solve <- cjs.df$A005_1
cjs.df$p.viol.crim <- cjs.df$A005_2
cjs.df$p.race.fair <- cjs.df$A005_3
cjs.df$p.exces.force <- cjs.df$A005_4
cjs.df$p.account <- cjs.df$A005_5

cjs.df$police.rate <- NA
cjs.df$police.rate <- (cjs.df$p.crim.solve + cjs.df$p.viol.crim + cjs.df$p.race.fair + cjs.df$p.exces.force + cjs.df$p.account)/5
cjs.df$police.rate.sc <- cjs.df$police.rate/4

# library(psy)
# cronbach(cbind(cjs.df$p.crim.solve, cjs.df$p.viol.crim, cjs.df$p.race.fair, cjs.df$p.exces.force, cjs.df$p.account))

# Court fairness
cjs.df$court.fair <- NA
cjs.df$court.fair[which(cjs.df$A006_r == "law?")] <- cjs.df$A006[which(cjs.df$A006_r == "law?")]
cjs.df$court.fair <- cjs.df$court.fair/3

cjs.df$court.fair.race <- NA
cjs.df$court.fair.race[which(cjs.df$A006_r == "law regardless of a person\x89۪s race?")] <- cjs.df$A006[which(cjs.df$A006_r == "law regardless of a person\x89۪s race?")]
cjs.df$court.fair.race <- cjs.df$court.fair.race/3

cjs.df$court.fair.class <- NA
cjs.df$court.fair.class[which(cjs.df$A006_r == "law regardless of a person\x89۪s class?")] <- cjs.df$A006[which(cjs.df$A006_r == "law regardless of a person\x89۪s class?")]
cjs.df$court.fair.class <- cjs.df$court.fair.class/3

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
cjs.df$pol.badapples <- cjs.df$A008

## Redefining survey objects
d.all <- svydesign(~1, data = cjs.df, weights = ~wts_whole)
d.wht <- svydesign(~1, data = subset(cjs.df, black == 0), weights = ~wts_white)
d.blk <- svydesign(~1, data = subset(cjs.df, black == 1), weights = ~wts_black)

