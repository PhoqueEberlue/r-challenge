Pour ce deuxième rendu nous avons commencé par modifier le vocabulaire en tentant de ne pas supprimer les balises, cela n'a pas été très efficace.
Nous avons également essayé de laisser les chiffres mais nous n'avons pas constaté d'amélioration notable.
Puis, nous avons joué sur les valeurs de lowFreq ainsi que de highFreq où nous avons constaté de bonnes améliorations : -5% d'erreur.

Aussi en prenant en compte les valeurs de notre premier rendu, nous avons essayé d'améliorer le taux de classification des listes.
Pour cela nous avons augmenté de KPPV à 4, résultant d'une bonne augmentation pour les listes (30% -> 53%), au détriment d'une très légère perte sur l'accueil (100% -> 98%).
Cela a aussi augmenté le taux des autres classifications.

Nous obtenons 0.2038095 d'erreur sur les données d'entraînement.