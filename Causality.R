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





