Pour ce troisième rendu nous nous sommes tournés vers les réseaux de neurones avec la libraire "neuralnet".
Voici les ressources que nous avons utilisées pour réaliser notre classificateur :
-   https://cran.r-project.org/web/packages/neuralnet/neuralnet.pdf
-   https://www.datacamp.com/community/tutorials/neural-network-models-r
-   https://www.r-bloggers.com/2017/02/multilabel-classification-with-neuralnet-package/
-   http://eric.univ-lyon2.fr/~ricco/tanagra/fichiers/fr_Tanagra_Packages_R_for_Deep_Learning.pdf
-   https://stats.stackexchange.com/questions/181/how-to-choose-the-number-of-hidden-layers-and-nodes-in-a-feedforward-neural-netw

Nous avons utilisé la fonction éponyme de la libraire neuralnet, et nous avons utilisé l'algorithme par défaut de la fonction qui a l'air d'être un algorithme de "resilient backpropagation".
Le réseau de neurone utilise les mêmes type données que pour nos essais avec KPPV. Nous avons fait varier les fréquences mais cela ne change pas grand-chose.
Nous avons également réalisé des essais en changeant les types d'algorithmes, de fonction d'activation et le nombre de couches cachées et de neurones.
Voici nos paramètres qui ont obtenu les meilleurs résultats :
-   lowFreq = 400
-   highFreq = 1400
-   couche cachées = 1
-   nombre de neurones dans la couche cachée = 30
-   fonction d'activation = "logistic"
-   nombre d'epochs = automatique

Nous avons compris que le nombre de couches et de neurones dépend de la répartition de nos classes sur un graphe : si la répartition est linéaire, alors il n'y a pas besoin de couches cachées,
il n'est même pas très utile d'appliquer un réseau de neurones sur cet exemple. Dans notre cas la répartition n'est pas linéaire mais étant donnée que nous ne connaissons pas sa complexité, nous avons
directement fait des essais pour tester le modèle.

Pour tester notre modèle et vérifier que les sorties étaient bonne nous avons utilisé deux méthodes :
-   découper les données en TRAIN et TEST
-   Une Cross Validation (le même principe mais pleins de fois) (nous avons mis une seed pour réduire l'aléatoire)
-   Avec les mêmes données de train et de test

Nous obtenons pour la première méthode de 99 à 100% de bonne classification et pour la deuxième cela peut varier entre 85 et 89% et pour la dernière 99.71429%.