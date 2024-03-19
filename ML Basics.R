set.seed(123)
x = rnorm(n=100, mean=0, sd=1)
y = x-2*x^2 + rnorm(n=100, mean=0, sd=1)

exercDat = data.frame(x,y)


plot(x,y)



library(boot)


set.seed(123)



glmFit = glm(y~x)

cv.glm(exercDat, glmFit, K=10)$delta


glmFit2 = glm(y ~ poly(x, 2))
cv.glm(exercDat, glmFit2, K =10)$delta

load("pension.rda")
library(tree)

newData <- pension[,c("net_tfa","age", "db","educ","fsize", "hown","marr", "pira","male",
                      "twoearn","inc")]


set.seed(123)
train <- sample(1:nrow(newData), 5000)
test <- (-train)
yTest <- newData$net_tfa[test]


treeMod <- tree(formula = net_tfa ~., data = newData[train,])
summary(treeMod)

yRegTest <- predict(treeMod, newdata = newData[test,])

mean((yRegTest-yTest)^2)
set.seed(123)
crossTree <- cv.tree(treeMod, FUN = prune.tree)
     


prunedTree <- prune.tree(tree = treeMod, best = 5)
pruni <- predict(prunedTree, newdata = newData[test,])

mean((yTest-pruni)^2)
