library(lmtest)
library(car)
library(sandwich)

source("~/Documents/git/cjs project/cjs_cleaning.R")

#### REWRITE INTO WRAPPER FORMULAS

## Linked Fate
m1.lf <- lm(police.account.sc ~ lfate,
            data = cjs.df, weights = wts_black)
m1.lf.se <- sqrt(diag(vcovHC(m1.lf, "HC1")))

m10.lf <- lm(police.account.sc ~ lfate + prison.time + crim.vict + employ.cjs + inc + educ3 + age + woman,
             data = cjs.df, weights = wts_black)
m10.lf.se <- sqrt(diag(vcovHC(m10.lf, "HC1")))
linearHypothesis(m10.lf, c("lfate = prison.time"), 
                 white.adjust = "hc1")

# Direct (IV operationalizations look the same substantively for stops. Police experience ratings strong association)
m1.1.direct <- lm(police.account.sc ~ stop.poor + pol.experience,
                  data = cjs.df, weights = wts_black)
m1.1.direct.se <- sqrt(diag(vcovHC(m1.1.direct, "HC1")))
m10.1.direct <- lm(police.account.sc ~ stop.poor + pol.experience + prison.time + crim.vict + employ.cjs + inc + educ3 + age + woman,
                   data = cjs.df, weights = wts_black)
m10.1.direct.se <- sqrt(diag(vcovHC(m10.1.direct, "HC1")))
linearHypothesis(m10.1.direct, c("pol.experience = employ.cjs"), 
                 white.adjust = "hc1")

# Proximal
m1.prox <- lm(police.account.sc ~ pol.mistreat + peer.felony,
              data = cjs.df, weights = wts_black)
m1.prox.se <- sqrt(diag(vcovHC(m1.prox, "HC1")))
m10.prox <- lm(police.account.sc ~ pol.mistreat + peer.felony + prison.time + crim.vict + employ.cjs + inc + educ3 + age + woman,
               data = cjs.df, weights = wts_black)
m10.prox.se <- sqrt(diag(vcovHC(m10.prox, "HC1")))
linearHypothesis(m10.prox, c("pol.mistreat = employ.cjs"), 
                 white.adjust = "hc1")


# Combined
m1.comb <- lm(police.account.sc ~ lfate + stop.poor + pol.experience + pol.mistreat + peer.felony,
              data = cjs.df, weights = wts_black)
m1.comb.se <- sqrt(diag(vcovHC(m1.comb, "HC1")))
m10.comb <- lm(police.account.sc ~ lfate + stop.poor + pol.experience + pol.mistreat + peer.felony + prison.time + crim.vict + employ.cjs + inc + educ3 + age + woman,
               data = cjs.df, weights = wts_black)
m10.comb.se <- sqrt(diag(vcovHC(m10.comb, "HC1")))
#.241/sd(cjs.df$police.account.sc[which(cjs.df$black == 1)], na.rm = T)
prop.table(svytable(~lfate, d.blk))
prop.table(svytable(~pol.experience, d.blk))
prop.table(svytable(~pol.mistreat, d.blk))

### Table Generation
stargazer(m1.lf, m10.lf, m1.1.direct, m10.1.direct, m1.prox, m10.prox, m1.comb, m10.comb, 
          title = "Explaining Unfairness Evaluations",
          label = c("account_evals"),
          column.labels = c("Linked Fate", "Linked Fate", "Direct Experiences", "Direct Experiences",
                            "Proximal Experiences", "Proximal Experiences",  "Combined", "Combined"),
          covariate.labels = c("Linked Fate",
                               "Prison Time", "Crime Victim",
                               "CJS Employee", "Income", "Education", "Age",
                               "Female",
                               "Bad Police Stops",
                               "Negative Police Interactions",
                               "Peer Police Mistreatment",
                               "Peer Felony Convictions"),
          dep.var.labels = c("Police Unfairness"),
          se = list(m1.lf.se, m10.lf.se, m1.1.direct.se, m10.1.direct.se,
                    m1.prox.se, m10.prox.se, m1.comb.se, m10.comb.se),
          no.space = T, notes = c("OLS regression results with robust standard errors in parentheses. Variables scaled 0-1."), 
          notes.align = "l", intercept.bottom = T, 
          #out = c("~/dropbox/ClassDivision_CarceralState/Analysis/do.Drew/figures/account_evals.tex"),
          type = "text",
          digits = 3, omit.stat = c("f", "adj.rsq"), 
          model.numbers = F, df = F, header = F, star.char = c("*"), star.cutoffs = c(0.05)
)

## Add in predicted outcomes based on estimates?
