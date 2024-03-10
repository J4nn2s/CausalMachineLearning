load("pension.rda")

library(MatchIt)

mod = matchit(formula = p401 ~age + db + educ + fsize + hown + inc + male + marr + pira
              + twoearn,
              data = pension,
              method = "nearest",
              link = "probit",
              replace = TRUE
)

matches = get_matches(mod)


fitMatch = lm(net_tfa ~p401,
              data = matches,
              weights = matches$weights)
fitMatch$coefficients

for (i in 1:nrow(pension)) {
  temp = matches[matches$id==i,]
  pension$weighti[i] = temp$weight
}
