library(tm)
library(neuralnet)
library(randomForest)

merge<-function(t){
  PlainTextDocument(paste(t,collapse=""))
}

#Suppression des script s(<script .... </script>)
removeScript<-function(t){
  #découpage de la chaine en utilisant "<split"
  sp<-strsplit(as.character(t), "<script")[[1]]
  #pour chaque partie du split, le début (jusqu'a </script>) est supprimé
  vec<-sapply(1:length(sp),function(i) gsub(sp[i], pattern=".*</script>", replace=" "))
  #les élements du split nettoyés sont concaténés
  PlainTextDocument(paste(vec,collapse=""))
}

#Suppression de toutes les balises
removeBalises<-function(t){
  t1<-gsub("<[^>]*>", " ", t)
  #suppression des occurrences multiples d'espaces (ou de tabulations)
  PlainTextDocument(gsub("[ \t]+"," ",t1))
}

#Fonction de nettoyage permettant de récupérer uniquement le contenu textuel d'une page HTML
nettoyage<-function(corpus){
  corpus<-tm_map(corpus,merge)
  corpus<-tm_map(corpus,content_transformer(tolower))
  #on utilise content_tranformer pour forcer le traitement via tolower à renvoyer un PlainTextDocument
  corpus<-tm_map(corpus,removeScript)
  corpus<-tm_map(corpus,removeBalises)
  corpus<-tm_map(corpus, removeWords, words=stopwords('en'))
  corpus<-tm_map(corpus, removePunctuation)
  corpus<-tm_map(corpus, stemDocument, language='en')
  corpus<-tm_map(corpus, removeNumbers)
}

corpus<-VCorpus(DirSource("training",recursive=TRUE))
corpusN<-nettoyage(corpus)
matDoc<-DocumentTermMatrix(corpusN)
classes<-c(rep(1,150),rep(2,150),rep(3,150),rep(4,150),rep(5,150),rep(6,150),rep(7,150))

genVocab<-function(mat, lowF=NULL, highF=NULL){
  if (is.null(lowF)) {
    vocab<-findFreqTerms(mat, highfreq = highF)
  } else if (is.null(highF)) {
    vocab<-findFreqTerms(mat, lowfreq = lowF)
  } else {
    vocab<-findFreqTerms(mat, lowfreq = lowF, highfreq = highF)
  }
  return(vocab)
}

genMat<-function(corpusN, voc, classes) {
  matF<-DocumentTermMatrix(corpusN, list(dictionary=voc))
  M<-as.matrix(matF)
  
  M<-cbind(M,classes)
  write.csv(M, "./data.csv", row.names = FALSE)
  #return(M)
}




trainNNCross<-function(trainData, hiddenLayers, stepm) {
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
                       hidden = hiddenLayers,
                       act.fct = "logistic",
                       linear.output = FALSE,
                       stepmax = stepm,
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
  return(mean(outs))
}

trainNN<-function(trainData, hiddenLayers, stepm) {
  trainData<-cbind(trainData[,-ncol(trainData)], nnet::class.ind(as.factor(trainData$classes)))
  names(trainData)<-c(names(trainData)[1:(ncol(trainData)-7)], "l1", "l2", "l3", "l4", "l5", "l6", "l7")
  
  #scl <- function(x){ (x - min(x))/(max(x) - min(x)) }
  #trainData[, 1:(ncol(trainData)-7)] <- data.frame(lapply(trainData[, 1:(ncol(trainData)-7)], scl))
  
  n<-names(trainData)
  f<-as.formula(paste("l1 + l2 + l3 + l4 + l5 + l6 + l7 ~", paste(n[!n %in% c("l1","l2","l3","l4","l5","l6","l7")], collapse = " + ")))
  nn=neuralnet(f, data = trainData, hidden = hiddenLayers, act.fct = "logistic", linear.output = FALSE, lifesign = "minimal", stepmax = stepm)
  
  #plot(nn)
  
  pr.nn <- compute(nn, trainData[,1:(ncol(trainData)-7)])
  # Extract results
  pr.nn_ <- pr.nn$net.result
  head(pr.nn_)
  
  original_values <- max.col(trainData[,(ncol(trainData)-6):ncol(trainData)])
  pr.nn_2 <- max.col(pr.nn_)
  return(mean(pr.nn_2 == original_values))
}

randomForestCross<-function(trainData) {
  trainData$classes <- as.factor(as.numeric(trainData$classes))
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
    index <- sample(1:nrow(trainData), round(proportion*nrow(trainData)))
    train_cv <- trainData[index, ]
    test_cv <- trainData[-index, ]
    model2 <- randomForest(classes ~ ., data = train_cv, ntree = 200, proximity=T)
    predValid <- predict(model2, test_cv)
    outs[i] <- mean(predValid == test_cv$classes)
  }
  return(mean(outs))
}


lowFreqs<-seq(100, 1000, by=100)
#highFreqs<-c(5000, 4000, 3000, 2000, 1750, 1500, 1250, 1000, 750)
goodClassification<-c()

for (freq in lowFreqs) {
  vocab<-genVocab(matDoc, lowF = freq, highF = 1400)
  genMat(corpusN, vocab, classes)
  mat<-read.csv("data.csv", sep = ",")
  percent<-randomForestCross(mat)
  print(percent)
  goodClassification<-append(goodClassification, percent)
}

plot(x = lowFreqs, y = goodClassification)

neurons<-c(30)
goodClassification<-c()

vocab<-genVocab(matDoc, lowF = 400, highF = 1400)
genMat(corpusN, vocab, classes)
mat<-read.csv("data.csv", sep = ",")
for (i in 1:6) {
  print(neurons)
  percent<-trainNNCross(mat, neurons, 300)
  print(percent)
  goodClassification<-append(goodClassification, percent)
  neurons<-append(neurons, 30)
}

plot(x = c(1,2,3,4,5), y = goodClassification)