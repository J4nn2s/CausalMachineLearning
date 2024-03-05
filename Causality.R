load("pension.rda")

# ATT / ATNT compute


net_tfa_1 = ifelse(pension$p401==1, pension$net_tfa,NA)
net_tfa_0 = ifelse(pension$p401==0, pension$net_tfa,NA)


# ein naiver estimator ist der mean difference
# hierfür eignet sich ein simpler t test

t.test(net_tfa~ p401,
       data = pension,
       var.equal = TRUE)

# oder man macht ein linear model hierfür

lmFit = lm(net_tfa ~p401,
           data = pension)

summary(lmFit)

# das sind eigentlich die Werte aus dem t.test
# Das ist quasi die manuelle computation der Werte mit lm für beide Gruppen

net_tfa_0_naive = predict(lmFit, 
                          newdata = data.frame(p401=0))

net_tfa_1_naive = predict(lmFit,
                          newdata = data.frame(p401=1))

net_tfa_0_naive
net_tfa_1_naive
# beides das gleiche LOL mit test

pension$indiTreat = ifelse(pension$p401 ==1, pension$net_tfa- net_tfa_0_naive,
                   net_tfa_1_naive-pension$net_tfa)

zweiteOption = ifelse(pension$p401 ==1, net_tfa_1- net_tfa_0_naive,
                      net_tfa_1_naive-net_tfa_0)


gleichesErgebnis <- indiTreat == zweiteOption
alleGleich <- all(gleichesErgebnis)



ATE = mean(pension$indiTreat)

ATT = mean(pension$indiTreat[pension$p401==1])

ATNT = mean(pension$indiTreat[pension$p401==0])



# 2. Aufgabe

# Probabilty that someone took part on p401 plan

probit_Fit = glm(formula = p401 ~ age + db + educ + fsize + hown + inc + male + marr
                 + pira + twoearn,
                 data = pension,
                 family = binomial(link = "probit"))
summary(probit_Fit)

pension$pScore = predict(probit_Fit, type = "response")




member = pension[pension$inc == 48252, "pScore"]

pension$pScoreDiff = ifelse(pension$p401 == 0, abs(pension$pScore-member), NA)

pensionNew = na.omit(pension)

# Most similar means in this case minimal difference in pScoreDiff
individual = pensionNew[pensionNew$pScoreDiff==min(pensionNew$pScoreDiff),]
individual$pScore


'''
Hier ist tricky, dass na.omit zwar die NAs entfernt, aber die dennoch bleiben diese na
gespeichert. Das erste Element, ist das wonach wir suchen, deshalb einfach [1]
'''

delta = na.omit(ifelse(pension$inc==48252, pension$net_tfa - individual$net_tfa,NA))
delta



delta = ifelse(pension$inc == 48252, pension$net_tfa - individual$net_tfa, NA)
na.omit(delta)[1]
delta[1]

# Load package
library(MatchIt)
# Matching step
mod = matchit(formula = p401 ~ age + db + educ + fsize + hown + inc + male + marr + pira
              + twoearn,
              data = pension,
              method = "nearest",
              link ="probit",
              replace =TRUE)
matches = get_matches(mod)
# Regression step
fitmatch = lm(net_tfa ~ p401,
              data = matches,
              weights = matches$weights)
fitmatch$coefficients


