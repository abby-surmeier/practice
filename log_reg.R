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
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)
log_mod <- train(Class ~., data = train, method = "glm", family = "binomial"(link = 'logit'), preProcess= c("center", "scale"), maxit = 100)
log_pred <- predict(log_mod, newdata = test, type = "prob")
confusionMatrix(log_pred, test$Class)

mod <- glm(Class ~., family = binomial(link = 'logit'), data = train)
p <- predict(mod, test, type = "response")
p_class <- ifelse(p >0.5, "M", "R")
table(p_class)
table(p_class, test[["Class"]])
confusionMatrix(as.factor(p_class), test[["Class"]])

rmarkdown::render("log_reg.R", "pdf_document")
