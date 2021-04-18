model_rf<-readRDS("model_random_forest.rds")
vocab<-t(colnames(read.csv("data.csv", sep = ",")))
vocab<-vocab[-ncol(vocab)]
nomClasses<-c("accueil", "blog", "commerce", "FAQ", "home", "liste", "recherche")
library(tm)
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

classer<-function(fic) {
  corpus<-VCorpus(URISource(fic))
  corpusN<-nettoyage(corpus)
  tmp<-DocumentTermMatrix(corpusN, list(dictionary=vocab))
  mat<-as.matrix(tmp)
  res<-predict(model_rf, mat)
  return (nomClasses[res])
}

tot<-0
for (class in nomClasses) {
  i<-0
  for (file in list.files(paste(c("training/", class), collapse = ""))) {
    ext<-c("training/", class, "/",file)
    nameFile<- paste(ext, collapse = "")
    res<-classer(nameFile)
    if (res == class) {
      i<-i+1
    }
  }
  print(class)
  print(i/150*100)
  tot<-tot + i
}
print("total")
print(tot/1050*100)


