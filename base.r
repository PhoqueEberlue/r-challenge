#Beaucoup d'operations sont possibles en R sur les chaines, incluant des expressions régulieres. 
#Voir l'aide des fonctions: gsub, strsplit, grep, ... 
#ou http://www.duclert.org/Aide-memoire-R/Le-langage/Chaines-de-caracteres.php library(tm) 
#---------------------------------------------------------- 
#SUPPRESSION DES BALISES SCRIPT #un texte peut parfois etre considere comme un vecteur ou une liste de lignes. 
#La fonction suivante concatene toutes les lignes du texte en parametre et retourne #une unique chaine de caracteres merge<-function(t){ PlainTextDocument(paste(t,collapse="")) } 
#Suppression des script s() est supprimé vec<-sapply(1:length(sp),function(i) gsub(sp[i], pattern=".*", replace=" ")) 
#les élements du split nettoyés sont concaténés PlainTextDocument(paste(vec,collapse="")) } 
#Suppression de toutes les balises removeBalises<-function(t){ t1<-gsub("<[^>]*>", " ", t) 
#suppression des occurrences multiples d'espaces (ou de tabulations) PlainTextDocument(gsub("[ \t]+"," ",t1)) } 
#Fonction de nettoyage permettant de récupérer uniquement le contenu textuel d'une page HTML nettoyage<-function(corpus){ corpus<-tm_map(corpus,merge) corpus<-tm_map(corpus,content_transformer(tolower)) 
#on utilise content_tranformer pour forcer le traitement via tolower à renvoyer un PlainTextDocument corpus<-tm_map(corpus,removeScript) 
#corpus<-tm_map(corpus,removeBalises) } 
#Exemple d'import du corpus : ATTENTION utiliser des VCorpus plutôt que des SimpleCorpus 
# corpus<-VCorpus(DirSource("training",recursive=TRUE)) 
#---------------------------------------------------------- 
#COMPTAGE DES BALISES #Compter par exemple le nombre de balises de 'lien' (anchor ) 
#une balise de 'lien' commence par "