library(randomForest)

data<-read.csv("data.csv", sep = ",")

# Conversion des données de la colonne classes comme facteur
data$classes <- as.factor(as.numeric(data$classes))

set.seed(100)
train <- sample(nrow(data), 0.5*nrow(data), replace = FALSE)
TrainSet <- data[train,]
ValidSet <- data[-train,]

# Définition du modèle
model <- randomForest(classes ~ ., data = data, ntree = 200)
model

saveRDS(model, "model_random_forest.rds")

predValid <- predict(model, data, type = "class")
# Checking classification accuracy
mean(predValid == data$classes)
table(predValid, data$classes)

# Checking classification accuracy
set.seed(500)
# 10 fold cross validation
k <- 10
# Results from cv
outs <- NULL
# Train test split proportions
proportion <- 0.5 # Set to 0.995 for LOOCV

# Crossvalidate, go!
for(i in 1:k)
{
  index <- sample(1:nrow(data), round(proportion*nrow(data)))
  train_cv <- data[index, ]
  test_cv <- data[-index, ]
  model2 <- randomForest(classes ~ ., data = train_cv, ntree = 200, proximity=T)
  predValid <- predict(model2, test_cv)
  outs[i] <- mean(predValid == test_cv$classes)
}
mean(outs)