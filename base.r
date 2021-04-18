#Beaucoup d'operations sont possibles en R sur les chaines, incluant des expressions régulieres.
#Voir l'aide des fonctions: gsub, strsplit, grep, ...
#ou http://www.duclert.org/Aide-memoire-R/Le-langage/Chaines-de-caracteres.php


library(tm)
#----------------------------------------------------------
#SUPPRESSION DES BALISES SCRIPT

#un texte peut parfois etre considere comme un vecteur ou une liste de lignes.
#La fonction suivante concatene toutes les lignes du texte en parametre et retourne
#une unique chaine de caracteres
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

distance<-function(x,y){
  return((sqrt(sum((x-y)^2))))
}

distanceManhattan<-function(x,y){
  return(sum(abs(x-y)))
}

distanceMinKowski<-function(x,y){
  return((sum((x-y)^1.414))^1/1.414)
}

distanceJaccard<-function(x,y){
  return(jaccard(x,y))
}

jaccard <- function(x, y, center=FALSE, px=NULL, py=NULL) {
  if(length(x) != length(y)) {
    stop("Two fingerprints (x and y) must be of the same length.")
  }
  
  if(is.null(px) | is.null(py)){
    px <- mean(x)
    py <- mean(y)
  }
  
  sumxy <- sum(x & y)
  unionxy <- sum(x)+sum(y)-sumxy
  if(unionxy == 0) {
    j <- (px*py)/(px+py-px*py)
  } else {
    j <- sumxy/unionxy
  }
  if(center == FALSE) {
    return(j)
  } else {
    return(j - (px*py)/(px+py-px*py))
  }
}


dist_voisins<-function(vecteur,data){
  return(apply(data[,-ncol(data)],1,distanceJaccard,x=vecteur))
}

kppv<-function(vecteur, k, data){
  v<-dist_voisins(vecteur,data)
  return(order(v)[1:k])
}

classerKPPV<-function(vecteur, k, data) {
  vec<-kppv(vecteur, k, data)
  classes<-data[vec,ncol(data)]
  occ<-table(classes)
  return (names(occ)[which.max(occ)])
}

erreurKPPV<-function(k, data) {
  res<-sapply(1:nrow(data), function(i)classerKPPV(data[i,-ncol(data)],k,data[-i,]))
  return (1-sum((res==(data[,ncol(data)]))/nrow(data)))
}

corpus<-VCorpus(DirSource("training",recursive=TRUE))
corpusN<-nettoyage(corpus)
mat<-DocumentTermMatrix(corpusN)
vocab200<-findFreqTerms(mat, lowfreq = 200, highfreq = 800)
vocab200<-sort(append(vocab200, c("professor", "essay", "checklist", "hotlist", "table", "search")))
write.csv(vocab200, "vocab.csv", row.names = FALSE)
mat20<-DocumentTermMatrix(corpusN, list(dictionary=vocab200))
M<-as.matrix(mat20)

classes<-c(rep(1,150),rep(2,150),rep(3,150),rep(4,150),rep(5,150),rep(6,150),rep(7,150))
M<-cbind(M,classes)
write.csv(M, "data.csv", row.names = FALSE)
erreurKPPV(k=4, data=M)

