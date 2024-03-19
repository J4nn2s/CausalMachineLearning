library(dplyr)


process <- function() {
  data <- read.csv("socialneighbor.csv", sep = ",")
  data <- data[, c("outcome_voted", "treat_neighbors", "sex", "yob",
                          "g2000", "g2002", "p2000", "p2002", "p2004", "city",
                          "hh_size", "totalpopulation_estimate", "percent_male",
                          "median_age", "median_income", "percent_62yearsandover", 
                          "percent_white", "percent_black", "percent_asian",
                          "percent_hispanicorlatino", "employ_20to64", "highschool", 
                          "bach_orhigher")]
  
  continuous_var <- c('hh_size','totalpopulation_estimate', 'yob', 'median_age', 'median_income', 'percent_62yearsandover','percent_male', 'percent_white', 'percent_black',
                      'percent_asian', 'percent_hispanicorlatino', 'employ_20to64', 'highschool', 'bach_orhigher')
  
  data <- data[sample(1:nrow(data), 5000), ]
  data[,continuous_var] <- scale(data[,continuous_var], center = TRUE, scale = TRUE)
  
  names(data)[names(data)=="outcome_voted"] <- "Y"
  names(data)[names(data) == "treat_neighbors"] <- "W"
  train = sample(1:nrow(data), nrow(data*0.7))
  test = (-train)
  
  return (list(train = train , test = test , data = data))
}

loaded <- process()

data <- loaded$data
train <- loaded$train
test <- loaded$test


library(randomForest)
data$Y = as.factor(data$Y)

forestMod <- randomForest(Y ~ .,
                          data = data[train,],
                          ntree=500,
                          splitrule = "gini")


forestMod$importance


partial_plots <- list()
for (var in names(forestMod$forest$xlevels)) {
  partial_plots[[var]] <- partial(forestMod, pred.var = "bach_orhigher")
}


library(pdp)

pdp_yob <- partial(forestMod, pred.var = "bach_orhigher")
plot(pdp_yob)



library(ggplot2)
ggplot(datimportanceggplot(data, aes(x = variable_name, y = outcome_voted)) +
  geom_point() +
  facet_wrap(~ other_variable_name)

  