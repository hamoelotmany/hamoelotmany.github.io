### TRAVAUX PRATIQUES AFCM (TP5 - 4H - SEANCES 9-10)####
##################################################################
### ENQUETES SUR LES LOISIRS - SEANCE 9
##################################################################
# chargement des données avec l'option "stringAsFactors = T"
# Grâce à l'option "stringAsFactors = T", toute variable dont 
# les modalités se présentent sous forme de texte (string) sont 
# reconnues comme variable catégorielle (facteur)
#setwd("lien de dossier/TP5")
#load("loisirs.csv")
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

library(ggplot2)
# Chargement de la librairie "FactorMineR" :
library(FactoMineR)
# Réalisation de l'AFCM : les 4 variables signalétiques 19, 20, 21 et 22 
# ainsi que la variable quantitative 23 sont déclarées 
# comme variables supplémentaires :
res.mca = MCA(LOISIRS, quali.sup = 19:22, quanti.sup = 23)
# L'objet "res.mca" contient les résultats de l'AFCM qui produit aussi différents graphiques 
# que vous pouvez observer. La commande suivante fournit un résumé numérique de l'AFCM
summary(res.mca)
# Graphe des pourcentages d'inertie associés à chaque axe
barplot(res.mca$eig[,2], main = "Pourcentage d'inertie", names.arg = 1:nrow(res.mca$eig))

# Représentation du nuage des individus sur les 2 premiers axes de l'analyse :
plot(res.mca, choix = "ind", invisible = "var", label = "none", cex = 0.5)

# Coloration des individus selon les modalités de la variable "Jardinage" (O/N) :
plot(res.mca, choix = "ind", hab = "Jardinage", label = "none", cex = 0.5) 

# Représentation du nuage des modalités sur les 2 premiers axes de l'analyse :
plot(res.mca, invisible = c("ind", "quali.sup"), hab = "quali")
# Repérage des individus extrêmes sur les axes de l'analyse.
# Axe 1. 
# Identification des 4 individus les plus à droite sur l'axe 1 puis affichage de 
# leurs réponses aux 18 questions concernant leurs activités de loisir :
indexes = order(res.mca$ind$coord[, 1])[8400:8403]
LOISIRS[indexes, 1:18]
# Identification des 4 individus les plus à gauche sur l'axe 1 puis affichage de 
# leurs réponses aux 18 questions concernant leurs activités de loisir :
indexes = order(res.mca$ind$coord[, 1])[1:4]
LOISIRS[indexes, 1:18]
# Axe 2. 
# Identification des 3 individus les plus hauts sur l'axe 2 puis affichage de 
# leurs réponses aux 18 questions concernant leurs activités de loisir :
indexes = order(res.mca$ind$coord[, 2])[8401:8403]
LOISIRS[indexes, 1:18]

###Reproduire avec la condition if (les modalites sont entre 8401 et 8403), la representation de
# des modalités proches de l'axe 1

# Identification des 3 individus les plus bas sur l'axe 2 puis affichage de 
# leurs réponses aux 18 questions concernant leurs activités de loisir :
indexes = order(res.mca$ind$coord[, 2])[1:3]
LOISIRS[indexes, 1:18]
# Représentation des variables. 
plot(res.mca, choix = "var")


# Représentation des modalités des variables supplémentaires
plot(res.mca, invisible = c("ind", "var"), hab= "quali") 
# si vous voulez imposer la couleur, il suffit de rajouter palette dans la fonction plot
plot(res.mca, invisible = c("ind", "var"), hab= "quali", palette=palette(c("red", "green", "orange", "blue"))) 
#à vérifier !!!

# Représentation de la variable quantitative supplémentaire sur le 1er plan
plot(res.mca, choix = "quanti.sup") 

# Pourcentage d'inertie représenté par chaque axe factoriel
barplot(res.mca$eig[,2], main = "Pourcentage d'inertie", names.arg = 1:nrow(res.mca$eig))

# afficher les individus dont cosinus carré est plus grand que 0.5
plot(res.mca, invisible = c("var","quali.sup"), select = "cos2 0.5", unselect = 0.8, cex = 0.9, label = "none")

# afficher les 1000 individus les mieux représentés sur le 1er plan factoriel
plot(res.mca, invisible = c("var","quali.sup"), select = "cos2 1000", unselect = 0.8, cex = 0.9, label = "none")

# afficher les 1000 individus qui contribuent le plus
plot(res.mca, invisible = c("var","quali.sup"), select = "contrib 1000", unselect = 0.8, cex = 0.5, label = "none")


# afficher les modalités dont le cosinus carré est plus grand que 0.4
plot(res.mca, invisible = c("ind","quali.sup"), selectMod = "cos2 0.4", unselect = "grey")

# afficher les 10 modalités les mieux représentées
plot(res.mca, invisible = c("ind","quali.sup"), selectMod = "cos2 10", unselect = "green")

# afficher les 10 modalités les plus contributives
plot(res.mca, invisible = c("ind","quali.sup"), selectMod = "contrib 10", unselect = "blue")
######################
# AFCM avec Factoshiny
######################
library(Factoshiny)
MCAshiny(LOISIRS)
##################################################################
##################################################################
### DONNEES MICRO-FINANCE - SEANCES 9-10
##################################################################
##################################################################
# chargement des données avec l'option "stringAsFactors = T"
# Grâce à l'option "stringAsFactors = T", toute variable dont 
# les modalités se présentent sous forme de texte (string) sont 
# reconnues comme variable catégorielle (facteur)
MICFIN = read.csv("http://www.math.univ-toulouse.fr/~ferraty/DATA/micro_finance.csv", header = T, sep = ",", dec = ".", stringsAsFactors = T)
# descriptif des variables
summary(MICFIN)
# Grâce à l'option "stringAsFactors = T", la commande summary affiche 
# pour les variables catégorielles les effectifs pour chaque classe
##################################################################
# Exploration des variables quantitatives
# Histogrammes des variables quantitatives initiales
par(mfrow = c(2, 4))
Varnames = names(MICFIN)
for(j in 9:15) hist(MICFIN[, j], main = paste(Varnames[j]), xlab = paste(Varnames[j]))
# Transformation logarithmique des variables quantitatives
MICFINLOG = cbind(MICFIN[, 1:8], log(MICFIN[, 9:15]))
# Histogrammes des variables quantitatives transformées
par(mfrow = c(2, 4))
Varnames = names(MICFINLOG)
for(j in 9:15) hist(MICFINLOG[, j], main = paste(Varnames[j]), xlab = paste(Varnames[j]))
# Chargement de la libraire "FactoMineR" (si besoin)
library(FactoMineR)
# Réalisation de l'AFCM : les 7 variables quantitatives ainsi que la variable qualitative 1 
# sont déclarées comme variables supplémentaires ; l'option "graph = F" produit aucun 
# graphique (par défaut "graph = T")
res.mca = MCA(MICFINLOG, quali.sup = 1, quanti.sup = 9:15, graph = T)
# Résumé numérique par défaut de l'AFCM
summary(res.mca)
# Résumé numérique affichant toutes les modalités actives de l'AFCM
summary(res.mca, nbelements = 23)
# Représentation du nuage des individus sur les 2 premiers axes de l'analyse :
par(mfrow = c(1,1))
plot(res.mca, invisible = c("var", "quali.sup"), label = "none", cex = 1)
# Représentation uniquement des variables actives. 
plot(res.mca, choix = "var", invisible = c("quanti.sup", "quali.sup"))

# Représentation du nuage des modalités sur les 2 premiers axes de l'analyse :
plot(res.mca, invisible = c("ind", "quali.sup"), hab = "quali", cex = 1)
# Repérage des individus extrêmes sur les axes de l'analyse.
# Axe 1. 
# Identification des 3 individus les plus à droite sur l'axe 1 puis affichage de leurs réponses aux 8 variables qualitatives : 
indexes = order(res.mca$ind$coord[, 1])[490:492]
MICFINLOG[indexes, 1:8]
# Identification des 3 individus les plus à gauche sur l'axe 1 puis affichage de leurs réponses aux 8 variables qualitatives : 
indexes = order(res.mca$ind$coord[, 1])[1:3]
MICFINLOG[indexes, 1:8]
# Axe 2. 
# Identification des 3 individus les plus hauts sur l'axe 2 puis affichage de leurs réponses aux 8 variables qualitatives : 
indexes = order(res.mca$ind$coord[, 2])[490:492]
MICFINLOG[indexes, 1:8]
# Identification des 3 individus les plus bas sur l'axe 2 puis affichage de leurs réponses aux 8 variables qualitatives : 
indexes = order(res.mca$ind$coord[, 2])[1:3]
MICFINLOG[indexes, 1:8]
# Représentation des modalités de la variable supplémentaire "Country"
plot(res.mca, invisible = c("ind", "var"), hab= "quali", cex = 1.5) 
# Représentation uniquement des 7 variables quantitatives supplémentaires sur le cercle des corrélations :
plot(res.mca, choix = "quanti.sup", cex = 1.5)
# Représentation simultanée de toutes les variables (actives et supplémentaires)
plot(res.mca, choix = "var", cex = 1.25)
# Pourcentages d'inertie
par(mfrow= c(1,1))
barplot(res.mca$eig[,2], main = "Pourcentage d'inertie", names.arg = 1:nrow(res.mca$eig))
# Transformation de variables quantitatives en variables catégorielles
# Rappel des quartiles de ces deux variables : 
summary(MICFINLOG$Assets)
summary(MICFINLOG$Personnel.expense)
# Transformation de ces deux variables selon leurs quartiles : 
Ass_cat = cut(MICFINLOG$Assets, breaks = c(10, 15, 16.1, 17.7, 22), labels = paste("Ass_cat_", 1:4, sep = ""))
Pers_exp_cat = cut(MICFINLOG$Personnel.expense, breaks = c(8, 12.3, 13.6, 14.9, 19), labels = paste("Pers_exp_cat_", 1:4, sep = ""))
# Création d'un nouveau tableau de données :
MICFINLOG_NEW = cbind(MICFIN[, 2:8], Ass_cat, Pers_exp_cat)
# Réalisation de l'AFCM avec cette nouvelle table : 
res.mca.new = MCA(MICFINLOG_NEW, quali.sup = c(8, 9), graph = F)
# Représentation simulatanée (pseudo-barycentrique) des individus et des modalités des variables actives/supplémentaires : 
plot(res.mca.new, label = c("var", "quali.sup"), cex = 1, col.ind = "gray95", hab = "quali")
# Représentation des variables actives/supplémentaires : 
plot(res.mca.new, choix = "var")
######################
# AFCM avec Factoshiny
######################
library(Factoshiny)
MCAshiny(MICFINLOG_NEW)
##################################################################
### DONNEES REVENU UNIVERSEL
### Pour celles et ceux qui souhaitent reproduire cette analyse chez eux
##################################################################
# chargement des données
REV_UNIV = read.csv("http://www.math.univ-toulouse.fr/~ferraty/DATA/revenu_universel.csv")
library(FactoMineR)
# AFCM
######
res.mca = MCA(REV_UNIV, quali.sup = c(1, 9), graph = F)
# représentation des modalités (1 couleur pour chaque variable)
plot(res.mca, label = c("var", "quali.sup"), cex = 1.1, invisible = "ind", hab = "quali")
# représentation des variables (1 couleur pour chaque variable)
plot(res.mca, choix = "var", cex.lab = 1.3, cex.main = 1.5, cex.axis = 1.3, cex = 1.3)
# représentation simultanée pseudo-barycentrique des individus et modalités (1 couleur pour chaque variable)
plot(res.mca, label = c("var", "quali.sup"), cex = 0.8, col.ind = "gray95", hab = "quali")
# AFCM avec Factoshiny
######################
library(Factoshiny)
PCAshiny(REV_UNIV)
