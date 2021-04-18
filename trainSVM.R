library(e1071)

data<-read.csv("data.csv", sep = ",")

# Conversion des données de la colonne classes comme facteur
data$classes <- as.factor(as.numeric(data$classes))

set.seed(100)
train <- sample(nrow(data), 0.5*nrow(data), replace = FALSE)
TrainSet <- data[train,]
ValidSet <- data[-train,]

# Définition du modèle
fit = svm(classes ~ ., data = TrainSet, scale = FALSE, kernel = "radial", cost = 5)

predValid = predict(fit, ValidSet)
mean(predValid == ValidSet$classes)
table(predValid, ValidSet$classes)

saveRDS(fit, "model_SVM.rds")

# Checking classification accuracy
set.seed(500)
# 10 fold cross validation
k <- 10
# Results from cv
outs <- NULL
# Train test split proportions
proportion <- 0.5

# Crossvalidate, go!
for(i in 1:k)
{
  index <- sample(1:nrow(data), round(proportion*nrow(data)))
  train_cv <- data[index, ]
  test_cv <- data[-index, ]
  model2 <- svm(classes ~ ., data = train_cv, scale = FALSE, kernel = "radial", cost = 5)
  predValid <- predict(model2, test_cv)
  outs[i] <- mean(predValid == test_cv$classes)
}
mean(outs)