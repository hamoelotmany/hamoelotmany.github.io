##################################################
### TRAVAUX PRATIQUES CLASSIFICATION (TP7 - 4H)
## RAPPEL DONNEES DECATHLON : HCPC
##################################################

par(mfrow=c(1,1))

# HCPC (variables réduites)
##################################################
library(FactoMineR)
data(decathlon)
chdeca=HCPC(data.frame(scale(decathlon[,1:10])), nb.clust = -1, metric="euclidian",consol=FALSE,method="ward", graph = TRUE)

# contenu des différentes composantes de l'objet "chdeca" produit par HCPC
chdeca$data.clust #donne le tableau des données auquel sont ajoutées les classes
chdeca$desc.var #donne le lien (𝜂2) entre les inputs quantitatifs et les classes 
#les plus influentes pour les classes et les variables 
# (un v.test négatif (resp. positif) met en avant les variables   
# influentes de par leurs fortes valeurs négatives (resp. positives)).
chdeca$desc.axes #donne le lien (𝜂2) entre les axes et les classes.
chdeca$desc.ind #donne les individus les plus représentatifs, i.e., 
#les plus proches du centre de gravité de leur classe d'appartenance ($para) ou 
#les plus éloignés des centres de gravités de leur classe et des autres classes ($dist)
chdeca$call
# Remarques : 
# CAH avec variables réduites : 
# chdeca.scale=HCPC(as.data.frame(decathlon.scale[,1:10]), nb.clust = 0, metric="euclidian",consol=FALSE,method="ward", graph = TRUE)
# --> variables réduites ==> chdeca.scale$call$t$inert.gain = diff(10 - chdeca.scale$call$t$within)
# Rappel : W1 + B1 = W2 + B2 ==> W1 - W2 = B1 - B2 
# (rappel valable pour des variables réduites ou non)
chdeca$call$min# = nb minimum de classes  (3 par défaut)
chdeca$call$max# = nb maximum classes (10 par défaut)
chdeca$call$t$within# : décroissance de l'inertie intra
# 1 seule classes --> inertie intra = 9.75609756
# 2 classes --> inertie intra = 8.25562785
# 3 classes --> inertie intra = 7.16925458
# 4 classes --> inertie intra = 6.21392581
# 5 classes --> inertie intra = 5.45898091
# ...................................
chdeca$call$t$inert.gain #: 
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
chdeca$call$t$quot #= ratio entre 2 inerties intra successives
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

chdeca= HCPC(PCA(decathlon, quanti.sup = 11:12, quali.sup =13 , graph = FALSE), nb.clust = 5, metric="euclidian", consol = FALSE, method="ward",graph= TRUE)
##################################################
# HCPC avec Factoshiny
##################################################
library(Factoshiny)
data(decathlon)
HCPCshiny(decathlon[,1:10])


##################################################################
##################################################################
### DONNEES OMF : hclust + HCPC
##################################################################
##################################################################

# cla/mod  = proba d'être dans la classe sachant qu'on prend cette modalité.
# mod/cla = proba de prendre cette modalité sachant qu'on est dans la classe
# vtest >> 0 --> sur-représentation de la modalité dans la classe
# vtest << 0 --> sous-représentation de la modalité dans la classe

##################################################################
### Classification hiérarchique  : données OMF
##################################################################
# 1) chargement des données et mise en forme
micfin=read.csv("http://www.math.univ-toulouse.fr/~ferraty/DATA/micro_finance.csv",sep=",",header=T)
View((micfin))
micfinlog = cbind(micfin[, 1:8], log(micfin[, 9:15]))
View(micfinlog)

# 2) AFCM
library(FactoMineR)
micfin.mca = MCA(micfinlog, quali.sup = 1, quanti.sup = 9:15, graph = F)
# inspecter les sorties numériques :
summary(micfin.mca)
# pourquoi 15 axes factoriels 
# Réponse 15 = m - p avec m = nb total de modalités = 22 et p = nb de variables = 7 
# inertie moyenne par axe? 
# réponse : 1/p = 0.14
# Remarque : on pourrait aller jusqu'à la dimension 7 pour le nombre d'axes à retenir pour la classification

# 3) classification hiérarchique à partir de l'AFCM (4ers axes)
# ind = coord sur les 4 1ers axes 
chmicfin=hclust(dist(micfin.mca$ind$coord[,1:4]),"ward.D2")
# scree-graph des niveaux d'aggrégation (ou inerties)
inertie = sort(chmicfin$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
#ci-dessous respectivement en vert, en rouge et en bleu.
points(c(2, 5, 8), inertie[c(2, 5, 8)], col = c("green3", "red3", "blue3"), cex = 2, lwd = 3)

# Représentation du dendrogramme et visualisation des classes
plot(chmicfin, hang = -1)
rect.hclust(chmicfin, 4, border = "blue2")
# Sauvegarde des classes affectées aux individus par la CHA
memb=cutree(chmicfin,k=4)
memb

# 4) représentation des classes et modalités dans le premier plan factoriel issu de l’AFCM
# représentation des modalités
par(mfrow = c(2,1))
plot(micfin.mca, invisible = c("ind", "quali.sup"), hab = "quali")
# Représentation des individus coloriés en fonction de la classe d'appartenance
plot(micfin.mca,choix="ind",invisible=c("var","quali"),label="none",col.ind=memb)
# Interprétation des axes factoriels
####################################
# L'axe 1 sépare bien les 2 classes des points noirs des 3 autres classes 
# (points verts, rouges et bleus)
# L'axe 2 distingue la classe des points verts et bleus des 2 autres classes 
# (points rouges et noirs)
# Interprétation des classes en fonction du position des modalités
# * la classe des OMF en noirs sont des OMF principalement : non régulées, de statut NGO, de taille 
#   petites à moyenne et à but non-lucratif
# * la classe des OMF rouges sont des OMF principalement : régulées, de statut Bank ou NBFI, 
#   de grande taille et à but lucratif
# * la classe des OMF vertes sont des OMF principalement caractérisées par une localisation en 
#   "East Asia and the Pacific" et un statut de type "Rural Bank"
# * la classe des OMF bleues sont des OMF principalement caractérisées par une localisation en 
#   "Eastern Europe and Central Asia" et un statut de type 
#   "Credit Union / Cooperative"

# Remarque : exemple de superposition des modalités et individus
#Découpage de dendrogramme en 4 clusters et transforme les résultats en un facteur
#Facteur utilisé pour attribuer les labels de clusters  aux indiv---> faciliter l'analyse et les visualisations
memb=as.factor(cutree(chmicfin,k=4))  # Convertir les numeros de cluster (entiers) en facteurs
# Les clusters seront des groupes qualitatives 
# facilité l'analyse graphique et les modèles d'anova ou de regression
#Compatibilité avec certains fonctions R ggplot2
DATA = cbind(micfinlog, memb)
DATA.MCA = MCA(DATA, quali.sup = c(1, 16), quanti.sup = 9:15, graph = F)
plot(DATA.MCA,invisible= 'quali.sup', select = c(''), hab=16,
         title="Graphe de l'ACM",label =c('var'), unselect = 0.7, col.var = "purple")


# 5) Classification avec HCPC
chamicfin=HCPC(micfin.mca, nb.clust = -1,metric="euclidian",consol=FALSE,
               method="ward", graph = TRUE)
#Comments
# cla/mod  = proba d'être dans la classe sachant qu'on prend cette modalité.
# mod/cla = proba de prendre cette modalité sachant qu'on est dans la classe
# vtest >> 0 --> sur-représentation de la modalité dans la classe
# vtest << 0 --> sous-représentation de la modalité dans la classe

chamicfin$desc.var
# Link pval>5% les variables catego ont un impact significatif sur la compo des clusters
# Classe 1 --> "rural Bank"(++) 97.92% des entreprises sont des banques rurales
# "Non-profit"(--) 23.53% ont des statuts lucratif
# Classe 3 --> "CreditUnion/Coop"(++) / "profit"(--)

# Représentation des modalités
plot(micfin.mca, invisible = c("ind", "quali.sup"), hab = "quali")
# Représentation des individus coloriés en fonction de la classe d'appartenance
membHCPC = chamicfin$data.clust$clust

plot(micfin.mca,choix="ind",invisible=c("var","quali"),label="none",col.ind=membHCPC)
# axes 1 et 3
plot(micfin.mca, axes = c(1,3), choix="ind",invisible=c("var","quali"),label="none",col.ind=membHCPC)
# axes 2 et 3
plot(micfin.mca, axes = c(2,3), choix="ind",invisible=c("var","quali"),label="none",col.ind=membHCPC)

# superposition des modalités et individus
DATA.MCA = MCA(chamicfin$data.clust, quali.sup = c(1, 16), 
               quanti.sup = 9:15, graph = F)
plot(DATA.MCA, invisible = 'quali.sup', select = c(''), 
     hab=16,title="Graphe de l'ACM",
         label =c('var'), unselect = 0.7, col.var = "purple")
# description des classes selon les axes factoriels
chamicfin$desc.axes
# pvalue---> pvalue>5% l'association entre la varible de cluster et la dimension est 
#hautement significative
# eta2: valeur très élévée --> la variable de cluster a un impact très fort sur la dim
# Les groupes formés par le clustering sont bien séparés selon cette dim

#Dim1, 2 et 3 les plus influencées par la variable de cluster avec des val. eta2>>>0
#Dans l'ensemble, on peut garder que 1 et 3
# classe 1 
# --> ind. coord élevées (pos) sur axe 2
# --> ind. coord nég. sur axe 3 et 1
# représentation des modalités
plot(micfin.mca, invisible = c("ind", "quali.sup"), hab = "quali")
# Représentation des individus coloriés en fonction de la classe d'appartenance
plot(micfin.mca,choix="ind",invisible=c("var","quali"),label="none",col.ind=membHCPC)
# superposition des modalités et individus
plot(DATA.MCA,invisible= 'quali.sup', select = c(''), 
         hab=16,title="Graphe de l'ACM",label =c('var'), unselect = 0.7, col.var = "purple")
# classe 2 
# --> ind. coord pos sur axe 3
# --> ind. coord nég. sur axe 1
# représentation des modalités
plot(micfin.mca, axes = c(1,3), invisible = c("ind", "quali.sup"), hab = "quali")
# Représentation des individus coloriés en fonction de la classe d'appartenance
plot(micfin.mca, axes = c(1,3),choix="ind",invisible=c("var","quali"),label="none",col.ind=membHCPC)
# superposition des modalités et individus
plot(DATA.MCA, axes = c(1,3),invisible= 'quali.sup', select = c(''), 
         hab=16,title="Graphe de l'ACM",cex=1.15,label =c('var'), unselect = 0.7, col.var = "purple")
# description des classes selon les individus
chamicfin$desc.ind
#para (paramètre associés aux clusters)
#@@ Les valeurs 0.31 sont répétées pour les observations dans ce cluster 87, 106,131,135 157
#Dist (distance associées aux cluster)

##@@Interprétat: les clusters 1,2,3 les valeurs sont constantes
####--> homogènes en termes de para mesurés
##@@ Pour  cluster 4, il y a une variation de valeurs
#### --> hétérogènes et les observations sont moins uniformes

# classe 4 : après lecture
# indiquer les 4 individus les plus centraux : 349 1 136 288 174 ayant la plus petite distance
# indiquer les 4 individus les plus éloignés des autres classes

# 6) Variables supplémentaires (6.1 hors programme)
# 6.1) relation entre les pays ("Country") et la partition ("chamicfin$data.clust$clust")
micfin.ca=CA(table(micfinlog[,1],chamicfin$data.clust$clust))
# commentaires du graphique
# existence d'un lien entre ces 2 variables: les ind. proches les uns des autres sur les axes 
# partagent des caractéristiques similaires en termes de ce deux var. catego.
# --> voir test du chi2 en faisant summary(micfin.ca) ou chisq.test
summary(micfin.ca)
# Khi-deux est très élévé, donc il y a une dépendance signifi. entre les deux variables
# les valeurs propres: dim 1 et dim2 expliquent ensemble 77.593% de la variance totale 
# Bengladesh ont dans dim1 avec un os2 70%
#Afghanistan dim3 avec cont neg et un cos2 élevé
# Afgh. Bangladesh et Albanie semblent avoir des associations distinctes avec les dimensiosn 1 et 2

chisq.test(micfinlog[,1], chamicfin$data.clust$clust)

# 6.2) relation entre les indices économiques et la partition ("chamicfin$data.clust$clust")
micfin.acp=PCA(chamicfin$data.clust, quali.sup = c(1:8, 16))
plot(micfin.acp, axes=c(2,3), invisible = "quali", label="none",hab=16)
micfinlogplus=cbind(chamicfin$data.clust$clust,micfinlog)
micfin.acp=PCA(micfinlogplus[,-c(2:9)],quali.sup = 1)
summary(micfin.acp)
# Dim1 explique 59,52% de la variance
#Dim2 explique 20,46% de la variance
#Dim3 explique 12.81% de la variance ---> ensemble on a 92.87% de la variance totale
#Dist : distance plus élevée indique que l'individu est éloigné de l'origine
###exemple Ind 1 a une position 0.432 sur dim1, -1.129 sur di2 et 0?083 sur dim3
#v.test plus grand, indique une projection importante sur l'axe associé, 
###donc il est bien représenté sur cet axe
plot(micfin.acp,axes=c(1,2),label="none",hab=1)
# pas de lien entre les indices économiques et la partition
##les indices (var quant) ne montrent pas de séparation nette ou de structure intéressante
##entre les clusters
library(scatterplot3d)
scatterplot3d(micfin.acp$ind$coord[,1:3],color=as.factor(micfin.acp$ind$clust), pch=16)
# 7) reprendre avec Factoshiny
library(Factoshiny)
MCAshiny(micfinlog)

##################################################################
##################################################################
### EXERCICE : "enquête sur les loisirs des français" avec HCPC
##################################################################
##################################################################
# 1) Réaliser une classification hiérarchique sur les individus qui ont 
#    participé à cette enquête à partir de leurs coordonnées sur les 1ers axes
# 2) Décrivez chaque classe à partir des résultats obtenus
# 3) recommencer avec Factoshiny

LOISIRS <- read.csv("http://www.math.univ-toulouse.fr/~ferraty/DATA/loisirs.csv", header = T, sep = ";", stringsAsFactors = T)
# Autre façon d'accéder aux données : 
# 1) télécharger le fichier "loisirs.csv" à l'aide d'un moteur de recherche et le sauvegarder dans un 
#    répertoire de sa machine
# 2) utiliser la fonctionnalité "Import Dataset" de RStudio
# Descriptif des variables
summary(LOISIRS)
# Grâce à l'option "stringAsFactors = T", la commande summary affiche 
# pour les variables catégorielles les effectifs pour chaque classe
# Pb : la variable 18 "usage TV" n'est pas identifiée comme facteur 
# (i.e. variable catégorielle) car ses modalités sont codées par des chiffres ; 
# la commande qui suit  permet de la déclarer comme facteur :
LOISIRS[, 18] = as.factor(LOISIRS[, 18])
# vérification que la variable 18 "usage TV" est bien identifiée comme une variable catégorielle
summary(LOISIRS)
# Réalisation de l'AFCM : les 4 variables signalétiques 19, 20, 21 et 22 
# ainsi que la variable quantitative 23 sont déclarées 
# comme variables supplémentaires :
loisirs.mca = MCA(LOISIRS, quali.sup = 19:22, quanti.sup = 23)
loisirs.hcpc =HCPC(loisirs.mca, nb.clust = -1, metric="euclidian",consol=FALSE,
               method="ward", graph = TRUE)
# Décrire les résultats obtenu

