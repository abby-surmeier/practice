# log regression using sonar dataset
library(mlbench)
data(Sonar)
summary(Sonar)
str(Sonar)
apply(Sonar, 2, var)

#80/20 split
train_index <- sample(1:nrow(Sonar), 166)
train <- Sonar[train_index,]
test <- Sonar[-train_index,]

#Log regression
set.seed(123)
library(caret)
library(LogicReg)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
log_mod <- train(Class ~., data = train, method = "glm", family = "binomial"(link = 'logit'), preProcess= c("center", "scale"))
log_pred <- predict(log_mod, newdata = test)
confusionMatrix(log_pred, test$Class)
