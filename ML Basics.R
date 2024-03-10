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
newData = pension[,c("net_tfa","age", "db","educ","fsize", "hown","marr", "pira","male",
                     "twoearn","inc")]
  

x <- model.matrix(net_tfa~. , data = newData)[,-1]
y <- newData$net_tfa


# Split data


set.seed(123)
train = sample(1:nrow(x), nrow(x) * 0.8)
test = (-train)


y.test = y[test]


grid <- 10^seq(10, -2, length.out= 100)

ridgeMod=glmnet(x[train,],y[train],alpha=0,lambda=grid)

library(glmnet)


# Cross validation
set.seed(123)
cvRidge=cv.glmnet(x[train,],y[train],alpha=0)
# Optimal lambda
bestLamRidge=cvRidge$lambda.min
bestLamRidge

