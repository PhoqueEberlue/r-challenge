library(neuralnet)

trainData<-read.csv("data.csv", sep = ",")

trainData<-cbind(trainData[,-ncol(trainData)], nnet::class.ind(as.factor(trainData$classes)))
names(trainData)<-c(names(trainData)[1:(ncol(trainData)-7)], "l1", "l2", "l3", "l4", "l5", "l6", "l7")

n<-names(trainData)
f<-as.formula(paste("l1 + l2 + l3 + l4 + l5 + l6 + l7 ~", paste(n[!n %in% c("l1","l2","l3","l4","l5","l6","l7")], collapse = " + ")))
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
  index <- sample(1:nrow(trainData), round(proportion*nrow(trainData)))
  train_cv <- trainData[index, ]
  test_cv <- trainData[-index, ]
  nn_cv <- neuralnet(f,
                     data = train_cv,
                     hidden = 30,
                     act.fct = "logistic",
                     linear.output = FALSE,
                     stepmax = 300,
                     lifesign = "minimal")
  
  # Compute predictions
  pr.nn <- compute(nn_cv, test_cv[,1:(ncol(trainData)-7)])
  # Extract results
  pr.nn_ <- pr.nn$net.result
  # Accuracy (test set)
  original_values <- max.col(test_cv[,(ncol(trainData)-6):ncol(trainData)])
  pr.nn_2 <- max.col(pr.nn_)
  outs[i] <- mean(pr.nn_2 == original_values)
}
mean(outs)