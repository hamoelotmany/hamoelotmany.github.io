##################################################################
### TRAVAUX PRATIQUES CLASSIFICATION (TP6-TP7 - 4H)
#Approche superviséee: cherche à établir un modèle de prévision 
#à partir d'un ensemble de donnéees étiquetéees
#Approche non-superviséee: Analyser, regrouper, trouver des structures cachéees dans les donnéees
#sans avoir une variable cible.
#################################################################
#Classification non supérvisée : CAH & K-means
#classification ascendante hiérarchique:
#Classification : on regroupe nos individus dans des classes
#Ascendante : on part du niveau le plus fin (ie des individus) où les individus sont seuls dans une classe
#Hiérarchique : la méthode aboutit à la construction d'un arbre

#################################################################
#idée de fonctionnement de CAH: 
#1. Calculer la matrice des distances 
#2. Remplacer les deux individus de distance minimale par une classe (à 2 élements) 
#qui sera représentée par le centre de gravité des individus et affectée de la somme 
# des poids des individus
#3.Calculer la perte d'inertie intercalsse (ou gain d'inertie interclasse)
#Après cette étape, on a (n-1 classes ) (n-2 classes à 1 éléments et une classe à 2 éléments)

### DONNEES DECATHLON : hclust + HCPC 
##################################################################
#1- Utilisation de la fonction hclust de R

#1. Lecture de Data: decathlon
library(FactoMineR)
data(decathlon)
dim(decathlon) # 41 individus et 13 variables
View(decathlon)

# 2. Le principe de CAH
# On calcule puis visualise la matrice des distances euclidiennes entre tous les individus
distdeca=dist(scale(decathlon[,1:10]))
distdeca
#Le minimum non nul de ce tableau est 1.540179 et 
#correspond à la distance entr e"BERNARD et" "Nool" 
#Le premier groupe sera donc formé en réunissant ces éléments.
#Le tableau des distances mutuelles entre les objets restant 
#après regroupement "BERNARD"et  "Nool"  

# On effectue la classification en utilisant la méthode de ward
#d(A,B) = W_AW_B/(W_A+W_B)d(gA,gB) où W_A et W_B les poids des classes
#Minimiser la variance interclasse
hcadeca=hclust(distdeca,"ward.D2")
hcadeca
############################Temps de calcul d'un dendrogramme###################
#Le temps de calcul d'un dendrogramme peut être particulièrement important 
#sur un gros fichier de données. 
#L'extension fastcluster permet de réduire significativement le temps de calcul. 
#library(fastcluster)
#hcadeca=hclust(distdeca,"ward.D2")
#hcadeca

summary(hcadeca)
# détail des éléments contenu dans "hcadeca"
hcadeca$merge
# [,1] [,2]
# [1,]   -4  -21
# [2,]   -6  -23
# [3,]  -30  -31
# [4,]   -8  -22
# [5,]   -5  -19
# [6,]  -28  -29
# [7,]  -12  -37
# [8,]  -11  -38
# [9,]   -7  -20
# [10,]  -26  -35
# [11,]  -14  -15
# [12,]  -32    2
# [13,]  -40    8
# [14,]    3   10
# [15,]   -1  -24
#   .      .    .
#   .      .    .
#   .      .    .
#   .      .    .
#   .      .    .
# [38,]   32   36
# [39,]   30   37
# [40,]   38   39
#
# Explications : 
# valeur négative = singleton (1 seul individu)
# valeur positive = numéro de ligne = groupe d'individus correspondant
# Exemple : 
# 1ère ligne  = 1er regroupement individus 4 et 21 (individus les + proches selon la matrice des distances)
# 2ème ligne  = 2ème regroupement individus 6 et 23
# ................................................
# 12ème ligne  = 12ème regroupement individus 32 avec classe 2 (classe 2 = ligne 2 = [2, ] = individus 6 et 23)
# ................................................
# 40ème ligne = 40ème regroupement = regroupement des classes 38 et 39
#
# repérage des 2 1ers individus agrégés
rownames(decathlon)[c(4, 21)]
rownames(decathlon)[c(6,23)]
# distance entre les 2 individus les + proches
as.matrix(distdeca)[4, 21]
# visualisation du dendrogramme pour le mettre 
# en parallèle de "hcadeca$merge"
#plot(hcadeca, main= "Dendogramme ",hang = -1) #labels= FALSE pour ne pas afficher les labels
# La fonction hclust produit l'objet hcadeca qui contient les 6 éléments suivants :  
hcadeca$height
# hcadeca$height fournit, pour chaque regroupement, les distances entre classes selon la stratégie choisie ("ward", "saut minimum", ...)
# Remarque : lorsque le regroupement concerne uniquement 2 individus, la valeur indiquée est la distance fournie par "distdeca". 
# Exemples : 
# 1ère valeur de hcadeca$height = 1.540179 = as.matrix(distdeca)[4, 21] (distance entre les individus 4 et 21)
# 2ème valeur de hcadeca$height = 1.786728 = as.matrix(distdeca)[6, 23] (distance entre les individus 4 et 21)
hcadeca$order
hcadeca$labels
hcadeca$method
hcadeca$call
hcadeca$dist.method
# Représentation du diagramme en éboulis (2 ou 5 classes)
inertie = sort(hcadeca$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")

#On voit trois sauts assez nets à 2, 5 et 8 classes, que nous avons représentés 
#ci-dessous respectivement en vert, en rouge et en bleu.
points(c(2, 5, 8), inertie[c(2, 5, 8)], col = c("green3", "red3", "blue3"), cex = 2, lwd = 3)

# Représentation du dendogramme : le niveau de chaque noeud est choisi proportionnel à la nouvelle
# inertie intra en choisissant inertie intra/inertie totale
# Le niveau est zéro si les individus sont séparés
plot(hcadeca, hang = -1)
# représentation sur le dendogramme de 6 classes
rect.hclust(hcadeca, 6, border = "green3")
plot(hcadeca, labels = FALSE, main = "Partition en 2, 5 ou 8 classes", xlab = "", ylab = "", sub = "", axes = TRUE, hang = -1)
rect.hclust(hcadeca, 2, border = "green3")
rect.hclust(hcadeca, 5, border = "red3")
rect.hclust(hcadeca, 8, border = "blue3")

################ Package dendextend #################################
#Vous pouvez utiliser l'extension dendextend et 
#sa fonction color_branches. Cette extensionfournit une méthode 
#permettant de passer un dendrogramme à ggplot.
install.packages("dendextend")
library(dendextend)
library(ggplot2)
ggplot(color_branches(hcadeca, k = 8), labels = TRUE)
################ Package factoextra #################################
#Vous pouvez utiliser la fonction fviz_dend de factoextra pour une meilleure visualisation
install.packages("factoextra")
library(factoextra)
fviz_dend(hcadeca, k = 5, show_labels = FALSE, rect = TRUE)

# Représentation du dendogramme
plot(hcadeca,  hang = -1)
# représentation sur le dendogramme de 5 classes
rect.hclust(hcadeca, 5, border = "green3")
# Représentation du dendogramme
plot(hcadeca, hang = -1)
# représentation sur le dendogramme de 2 classes
rect.hclust(hcadeca, 2, border = "green3")
# En pratique et dès que possible, on retiendra des partitions possédant
# les caractéristiques suivantes : 
# 1) nb de classes correspondant à une perte significative d'inertie intra
#    (i.e. nb de classes correspondant à un gain significatif d'inertie inter)
# 2) nb de classes raisonnable au regard du nombre d'individus à classer
# 3) obtention de classes de tailles homogènes
###############################################################
# visualisation des 5 classes d'individus sur le plan princiapl
# récupération de la partition dans l'objet "memb5"
# L'arbre est coupé pour obtenir une partition en 5 classes
memb5 = cutree(hcadeca, k = 5)
memb5
# Représentation du graphique des pertes relatives 
#d'inertie avec graph=TRUE. La meilleure partition selon ce critère est 
#représentée par un point noir et la seconde par un point gris.
install.packages("JLutils")
library(JLutils)
source(url("https://raw.githubusercontent.com/larmarange/JLutils/master/R/clustering.R"))
#Cherche la meilleure partition entre 3 et 20 classes.
best.cutree(hcadeca,graph = TRUE, xlab = "Nombre de classes", ylab = "Inertie relative")
#il s'agirait d'une partition en 5 classes. 
#Il est possible de modifier le minimum et
#le maximum des partitions recherchées avec min et max.
best.cutree(hcadeca, min = 2, graph = TRUE, xlab = "Nombre de classes", ylab = "Inertie relative")

#################ACP des données decathlon ####################
#1. Lecture de Data: decathlon
library(FactoMineR)
data(decathlon)
decacp = PCA(decathlon, quanti.sup = 11:12, quali.sup=13)
# axe 1, 2
par(mfrow=c(1,1))
# représentation des individus sur la plan principal (1, 2)
plot(decacp$ind$coord[,1],decacp$ind$coord[,2],pch=20,col=memb5, xlab = "Axe 1", ylab ="Axe 2")
# rajout des noms des individus
text(decacp$ind$coord[,1],decacp$ind$coord[,2], labels = rownames(decathlon), col=memb5, pos=3)
# rajout de la légende indiquant le code couleur pour les classes
legend(x = 2.5, y = 0, legend = c("classe 1", "classe 2", "classe 3", "classe 4", "classe 5"), text.col = 1:5, cex = 0.75)
# représentation du cercle des corrélation
plot(decacp, choix="var", axes=c(1,2))
# axe 1, 3
##########
# représentation des individus sur la plan principal (1, 3)
plot(decacp$ind$coord[,1], decacp$ind$coord[,3],col=memb5, xlab = "Axe 1", ylab ="Axe 3")
# rajout des noms des individus
text(decacp$ind$coord[,1],decacp$ind$coord[,3], labels = rownames(decathlon), col=memb5,pos=3)
# rajout de la légende indiquant le code couleur pour les classes
legend(x = 2.5, y = 2.5, legend = c("classe 1", "classe 2", "classe 3", "classe 4", "classe 5"), text.col = 1:5, cex = 1.25)
# représentation du cercle des corrélation
plot(decacp,choix="var",axes=c(1,3))
# axe 1, 4
##########
# représentation des individus sur la plan principal (1, 4)
plot(decacp$ind$coord[,1], decacp$ind$coord[,4],col=memb5, xlab = "Axe 1", ylab ="Axe 4")
# rajout des noms des individus
text(decacp$ind$coord[,1],decacp$ind$coord[,4], labels = rownames(decathlon), col=memb5,pos=3)
# rajout de la légende indiquant le code couleur pour les classes
legend(x = 2.5, y = 2.5, legend = c("classe 1", "classe 2", "classe 3", "classe 4", "classe 5"), text.col = 1:5, cex = 1.25)

###############################################
# COMPARAISON DES STRATEGIES ("ward", "saut minimum",...)
###############################################

# LIEN MINIMAL (i.e. SAUT MINIMUM plus petite distance)
hcadeca.single=hclust(distdeca,"single")
inertie.single = sort(hcadeca.single$height, decreasing = TRUE)
par(mfrow = c(1,2))
inertie = sort(hcadeca$height, decreasing = TRUE)
plot(inertie.single[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie (single)")
#points(c(3, 5, 8), inertie.single[c(3, 5, 8)], col = c("green3", "red3", "blue3"), cex = 2, lwd = 3)
abline(v = 1:20, lwd = 0.25)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
abline(v = 1:20, lwd = 0.25)
# visualisation des 5 classes d'individus
memb5.single = cutree(hcadeca.single, k = 5)
# Représentation des ind. sur les axes 1 et 2
par(mfrow = c(1, 1))
plot(decacp$ind$coord[,1],decacp$ind$coord[,2],pch=20,col=memb5.single, xlab = "Axe 1", ylab ="Axe 2")
text(decacp$ind$coord[,1],decacp$ind$coord[,2], labels = rownames(decathlon), col=memb5.single, pos=3)
legend(x = 2.4, y = 0, legend = c("classe 1", "classe 2", "classe 3", "classe 4", "classe 5"), text.col = 1:5, cex = 0.75)

# COMPARAISON DES DENDROGRAMMES WARD/SINGLE
par(mfrow = c(2, 1), mar = c(1,4,1,1))
plot(hcadeca, hang = -1)
rect.hclust(hcadeca, 5, border = "blue") #ward
plot(hcadeca.single, hang = -1)
rect.hclust(hcadeca.single, 5, border = "green3")

# LIEN COMPLET (i.e. DIAMETRE plus grande distance)
hcadeca.complete=hclust(distdeca,"complete")
inertie = sort(hcadeca.complete$height, decreasing = TRUE)
par(mfrow = c(1,1))
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
# visualisation des 6 classes d'individus
memb6.complete = cutree(hcadeca.complete, k = 6)
K <- 6
T <- sum(hcadeca$height)
n <- length(hcadeca)
W <- sum(hcadeca$height[1:(n-K)])
#Pourcentage d'inertie expliquée
(1-W/T)*100

# Représentation des ind. sur les axes 1 et 2
plot(decacp$ind$coord[,1],decacp$ind$coord[,2],pch=20,col=memb6.complete, xlab = "Axe 1", ylab ="Axe 2")
text(decacp$ind$coord[,1],decacp$ind$coord[,2], labels = rownames(decathlon), col=memb6.complete, pos=3)
legend(x = 3, y = 0, legend = c("classe 1", "classe 2", "classe 3", "classe 4", "classe 5", "classe 6"), text.col = 1:6, cex = 0.75)

# COMPARAISON DES DENDROGRAMMES WARD/COMPLETE
par(mfrow = c(2, 1), mar = c(1,1,1,1))
plot(hcadeca, hang = -1)
rect.hclust(hcadeca, 5, border = "blue")
plot(hcadeca.complete, hang = -1)
rect.hclust(hcadeca.complete, 5, border = "green3")


# LIEN MOYEN
hcadeca.average=hclust(distdeca,"average")
inertie = sort(hcadeca.average$height, decreasing = TRUE)
par(mfrow = c(1,1), mar = c(3,3,3,3))
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
# récupération de la partition dans l'objet "memb5.average"
memb5.average = cutree(hcadeca.average, k = 5)
# Représentation des ind. sur les axes 1 et 2
plot(decacp$ind$coord[,1],decacp$ind$coord[,2],pch=20,col=memb5.average, xlab = "Axe 1", ylab ="Axe 2")
text(decacp$ind$coord[,1],decacp$ind$coord[,2], labels = rownames(decathlon), col=memb5.average, pos=3)
legend(x = 2, y = 4, legend = c("classe 1", "classe 2", "classe 3", "classe 4", "classe 5"), text.col = 1:5, cex = 1.25)

# COMPARAISON DES DENDROGRAMMES WARD/AVERAGE
par(mfrow = c(2, 1), mar = c(1,4,1,1))
plot(hcadeca, hang = -1)
rect.hclust(hcadeca, 5, border = "blue")
plot(hcadeca.average, hang = -1)
rect.hclust(hcadeca.average, 5, border = "green3")

# Récapitulatif des dendogrammes
par(mfrow=c(1,1))
plot(hclust(distdeca,"ward.D2"),hang=-1)
plot(hclust(distdeca,"single"),hang=-1)
plot(hclust(distdeca,"complete"),hang=-1)
plot(hclust(distdeca,"average"),hang=-1)

#ggplot(color_branches(hcadeca, k = 8), labels = TRUE)

# CONCLUSION : les différentes startégies produisent des classifications différentes. 
# On retiendra celle qui valide les critères de qualité d'une partition à savoir : 
# 1) nb de classes correspondant à une perte significative d'inertie intra
#    (i.e. nb de classes correspondant à un gain significatif d'inertie inter)
# 2) nb de classes raisonnable au regard du nombre d'individus à classer
# 3) obtention de classes de tailles homogènes
#
# Au regard, des différentes classifications obtenues, c'est celle utilisant la 
# stratégie "ward" avec 5 classes qui sera retenue

###############################################
###############################################
# CENTRAGE ET REDUCTION DES DONNEES
###############################################
###############################################
# On peut recommencer la classification avec les données centrées-réduites
#pour éviter que variables à forte variance pèsent indûment sur les résultats
decathlon.scale = scale(as.matrix(decathlon[,1:10])) #center =T, scale = T

# On calcule et visualise la matrice des distances euclidiennes entre tous les individus
distdeca.scale=dist(decathlon.scale)
distdeca.scale
#CAH (classification Ascendate Hiérarchique) - stratégie de Ward 
#propose une représentation intéressante mais exploitable seulement si n est petit
#method = « ward.D2 » correspond au vrai critère de Ward
#utilisant le carré de la distance
hcadeca.scale.ward=hclust(distdeca.scale,"ward.D2")
inertie = sort(hcadeca.scale.ward$height, decreasing = TRUE)
par(mfrow = c(1,1), mar = c(3,3,3,3))
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")

#Affichage des dendrogrammes
#indique l'ordre dans lequel les agrégations successives ont été faites.
#indique la valeur de l'indice d'agrégation à chaque niveau d'agrégation
par(mfrow=c(1,1))
plot(hclust(distdeca.scale,"ward.D2"),hang=-1)
######################Commentaires#########################
#Le dendrogramme « suggère » un découpage en 4 groupes
#on aurait pu envisager aussi un découpage en 2 groupes seulement
#Nous y reviendrons plus longuement lorsque 
#nous mixerons l'analyse avec une analyse en composantes principales (ACP).
###############################################
#dendrogramme avec matérialisation des groupes
rect.hclust(hcadeca.scale.ward,k=4)
#découpage en 4 groupes
groupes.cah <- cutree(hcadeca.scale.ward,k=4)
#liste des groupes
print(sort(groupes.cah))

plot(hclust(distdeca.scale,"single"),hang=-1)
#dendrogramme avec matérialisation des groupes
#rect.hclust(hcadeca.scale.ward,k=?)

plot(hclust(distdeca.scale,"complete"),hang=-1)

plot(hclust(distdeca.scale,"average"),hang=-1)

# focus sur la stratégie "ward"
par(mfrow=c(1,1))
plot(hclust(distdeca.scale,"ward.D2"),hang=-1)
rect.hclust(hcadeca.scale.ward, 5, border = "green3")

# On peut comparer les 2 dendogrammes : 
cbind(hcadeca$merge, hcadeca.scale.ward$merge)
# Ici le fait de centrer puis de réduire les données ne change pas la classification
##################################################
## HCPC: Classification Hierarchique sur la Composante Principale
#permet de combiner les 3 méthodes standards dans les analyses des données multivariées 
#Analyse par composante principale (ACP, ACF, ACFM), Classification Ascendante Hiérarchique
#Le partitionnement en K moyennes
#ACP doit être considérée comme une étape essentielle réduisant le bruit de fonds des données
#pour avoir une classification plus stable
##################################################
##################################################
par(mfrow=c(1,1))

#2- Utilisation de la commande HCPC de FactoMineR HCPC (variables réduites)
#---Le module HCPC propose de faire des classifications hiérarchiques ascendantes 
#  (via hclust) précédées d une analyse factorielle. 
#  L'option consol permet de consolider les résultats.
# Effectuer la classification en utilisant la métrique euclidienne et
# la méthode de Ward. L'option nb.clust permet soit de choisir le niveau de césure (0) 
#   sur le dendrogramme ou de conserver le choix proposé par HCPC.
##################################################
library(FactoMineR)
data(decathlon)
chdeca=HCPC(data.frame(scale(decathlon[,1:10])), nb.clust = -1, 
            metric="euclidian",consol=FALSE,method="ward", graph = TRUE)

plot(chdeca, choice = "map")
plot(chdeca, choice = "tree")
plot(chdeca, choice = "3D.map")
# contenu des différentes composantes de l'objet "chdeca" produit par HCPC
chdeca$data.clust #donne le tableau des données auquel sont ajoutées les classes
chdeca$desc.var #donne le lien (𝜂2) entre les inputs quantitatifs et les classes 
                #les plus influentes pour les classes et les variables 
                # (un v.test négatif (resp. positif) met en avant les variables   
                # influentes de par leurs fortes valeurs négatives (resp. positives)).
chdeca$desc.axes #donne le lien (𝜂2) entre les axes et les classes.
chdeca$desc.ind #donne les individus les plus représentatifs, i.e., 
                #les plus proches du centre de gravité de leur classe ($para) ou 
                #les plus éloignés des centres de gravités des autres classes ($dist)


#Visualiser les clusters,par exemple avec la fonction fviz_cluster() :
plot(decathlon[,1:3],col=rainbow(2)[factor(chdeca$data.clust$clust)],pch=16) 
library(factoextra)
library(ggplot2)
fviz_cluster(chdeca, data, ellipse.type = "norm")

# Moyennes de clusters pour chaque variable
apply(decathlon,2,function(x){by(x,chdeca$data.clust$clust,mean)})
aggregate(decathlon, by=list(Cluster=chdeca$data.clust$clust), summary) 
apply(decathlon,2,function(x){by(x,chdeca$data.clust$clust,summary)})

# ex : L'athlète "Karlivans" appartient à la classe 1 
#      et est le plus proche du centre de la classe
# --> ce sont les individus les + représentatifs de leur classe selon ce critère

#  ex : L'athlète "Korkizoglou" appartient à la classe 2 
#       et est le plus éloigné des centres des classes 1, 3 et 4
# --> ce sont les individus les + représentatifs de leur classe selon ce critère

chdeca$call
# Remarques : 
# CAH avec variables réduites : 
# chdeca.scale=HCPC(as.data.frame(decathlon.scale[,1:10]), nb.clust = 0, metric="euclidian",consol=FALSE,method="ward", graph = TRUE)
# --> variables réduites ==> chdeca.scale$call$t$inert.gain = diff(10 - chdeca.scale$call$t$within)
# Rappel : W1 + B1 = W2 + B2 ==> W1 - W2 = B1 - B2 
# (rappel valable pour des variables réduites ou non)
# $call$min = nb minimum de classes  (3 par défaut)
# $call$max = nb maximum classes (10 par défaut)
# $call$t$within : décroissance de l'inertie intra
# 1 seule classes --> inertie intra = 9.75609756
# 2 classes --> inertie intra = 8.25562785
# 3 classes --> inertie intra = 7.16925458
# 4 classes --> inertie intra = 6.21392581
# 5 classes --> inertie intra = 5.45898091
# ...................................
# Remarque : l'inertie intra décroit toujours avec le nombre de classes ; quid du comportement de l'inertie inter?
# QUESTION : Combien d'éléments devrait contenir l'objet $t$within ? Que vaut la 41ème valeur?

# $call$t$inert.gain : 
# --> quand on partitionne l'ensemble des individus en 2 classes, 
# on gagne 1.50046971 en inertie inter ou de façon équivalente, 
# on perd 1.50046971 en inertie intra
# --> quand on passe de 2 classes à 3 classes, 
# on gagne 1.08637327 en inertie inter ou de façon équivalente, 
# on perd 1.08637327 en inertie intra
# --> quand on passe de 3 classes à 4 classes, 
# on gagne 0.95532877 en inertie inter ou de façon équivalente, 
# on perd 0.95532877 en inertie intra
# --> quand on passe de 4 classes à 5 classes, 
# on gagne 0.75494490 en inertie inter ou de façon équivalente, 
# on perd 0.75494490 en inertie intra
# .............................
# Remarque : le gain en inertie inter décroit avec le nombre de classes

# $call$t$quot = ratio entre 2 inerties intra successives
# (à partir du nb minimal de classes $call$min - rappel : $call$min = 3 par défaut)
# 3 classes --> 0.8684082 = inertie intra pour 3 classes / inertie intra pour 2 classes
#                         = 7.16925458 / 8.25562785
# 4 classes --> 0.8667464 = inertie intra pour 4 classes / inertie intra pour 3 classes
#                         = 6.21392581 / 7.16925458
# 5 classes --> 0.8785076 = inertie intra pour 4 classes / inertie intra pour 3 classes
#                         = 5.45898091 / 6.21392581
# ..............................
# on s'arrête dès qu'on atteint le nb maximal de classes $call$max - rappel : $call$max = 10 par défaut)
# 10 classes --> 0.9251990 = inertie intra pour 10 classes / inertie intra pour 9 classes
#                          = 3.46832520 / 3.74873429

# Choix du nombre "optimal de classes"
# = nb de classes entre "$call$min" et "$call$max" minimisant $call$t$quot
# --> nb de classes "optimal" = 4
