library(lmtest)
library(car)
library(sandwich)

source("~/Documents/git/cjs project/cjs_cleaning.R")

## Linked Fate
m1.lf <- lm(police.account.sc ~ lfate,
            data = cjs.df, weights = wts_black)
m1.lf.se <- sqrt(diag(vcovHC(m1.lf, "HC1")))

m10.lf <- lm(police.account.sc ~ lfate + prison.time + crim.vict + employ.cjs + inc + educ3 + age + woman,
             data = cjs.df, weights = wts_black)
m10.lf.se <- sqrt(diag(vcovHC(m10.lf, "HC1")))

# Direct (IV operationalizations look the same substantively for stops. Police experience ratings strong association)
m1.1.direct <- lm(police.account.sc ~ stop.poor,
                  data = cjs.df, weights = wts_black)
m1.1.direct.se <- sqrt(diag(vcovHC(m1.1.direct, "HC1")))




# Combined
m1.comb <- lm(police.account.sc ~ lfate + stop.poor + pol.mistreat + peer.felony,
              data = cjs.df, weights = wts_black)
m1.comb.se <- sqrt(diag(vcovHC(m1.comb, "HC1")))
m10.comb <- lm(police.account.sc ~ lfate + stop.poor + pol.mistreat + peer.felony + prison.time + crim.vict + employ.cjs + inc + educ3 + age + woman,
               data = cjs.df, weights = wts_black)
m10.comb.se <- sqrt(diag(vcovHC(m10.comb, "HC1")))