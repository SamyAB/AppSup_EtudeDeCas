#Ne pas oublier de changer le working directory !
setwd('/home/samy/workspaces/projets_mlds/AppSup_EtudeDeCas')

################### Pré-traitement des données ###################
#Lecture des données dans raw_data
#La lecture se fait en prennant en compte la première ligne comme noms des variables
#En remplaçant les . en NA (Valeurs manquante)
#Et en eméchant la transformation en facteur des vecteurs
raw_data = read.table('data/VisaPremier.txt',header = TRUE,na.strings = ".", stringsAsFactors = FALSE)

#On peut déjà écarter certaines variables
#Le matricule n'est qu'un identifiant, il n'est en aucun cas un facteur dans la décision de la classe
#La variable sexe peut être retirée car il existe une variable binaire sexer équivalante
#La variable nbimpaye vaut 0 quelque soit le client
#La variable cartevp exite en version codée en binaire (cartevpr)
non_used = names(raw_data) %in% c("matricul","sexe","nbimpaye","cartevp")

#On peut également séparer les variables qualitatives des quantitatives
qualitatives = names(raw_data) %in% c("departem","ptvente","sitfamil","csp","codeqlt","sexer")
#sitfamil (situation familliale) est à codifier
#csp (Catégorie socio-professionnelle) est à codifier
#codeqlt (code de qualité du client donné par la banque) est ordinal à codifier et contient des valeurs manquantes
#Et la classe est représentée par la variable cartevpr

#On commence par créer une table contenant uniquement les données quantitatives et la classe
quantitative_data = raw_data[!(non_used | qualitatives)]
#On mélange l'ordre des lignes pour pouvoir sample plus facilement plus tard
quantitative_data = quantitative_data[sample(nrow(quantitative_data)),]
#On sépare par la suite les variables de la classe
Xqu = quantitative_data[names(quantitative_data) != "cartevpr"]
Yqu = quantitative_data[names(quantitative_data) == "cartevpr"]
#On traite les variables dans Xqu
summary(Xqu)
#La variable agemvt contient 6 valeurs manquantes
#La variable nbpaiecb contient 278 valeurs manquantes

#Pour les valeurs manquantes de agemvt on remarque que pour avoir une carte premier visa
#Les client doivent avoir agemvt <= 48 
#On remarque que pour tous les clients ou la valeur de agemvt est manquante les clients n'ont pas de carte premier visa
#On remplace donc les valeurs manquante par une valeur au dessus de 48 
#Pour être sur d'être loin de 48 on prend le max de la variable càd 944
Xqu[names(Xqu) == "agemvt"][is.na(Xqu[names(Xqu) == "agemvt"])] = 944

#Pour les valeurs manquantes de nbpaiecb
#On ne peut pas suivre le même raisonement que pour agemvt
#Et que la variable est quantitative
#On remplace les valeurs manquantes par la moyenne
mean_nbpaiecb = mean(Xqu[,32], na.rm = TRUE)
#Le na.rm permet de supprimer les valeurs manquante lors du calcul de la moyenne
Xqu[names(Xqu) =="nbpaiecb"][is.na(Xqu[names(Xqu) == "nbpaiecb"])] = mean_nbpaiecb

#Normalisation des données
#On va tenter une normalisation de toutes les données quantitatives entre 0 et 1
normalized_Xqu = data.frame(Xqu)
for (i in 1:length(Xqu)) {
  tmp = ( Xqu[,i] - min(Xqu[,i]) )/( max(Xqu[,i])-min(Xqu[,i]) )
  normalized_Xqu[,i] = tmp
}

#Maintenant on va à la recherche de coolinéarité
#On utilise un critère de VIF et on essaye de supprimer les variables qui sont coolinéaires
library(usdm)
#On étudie les variables qui ont une correlation linéraire supperieure à 0.9 et on en enlève celles
#Qui ont le plus grand VIF
vifcor(normalized_Xqu,th = 0.9)
#Ces variables sont mtbon nbbon mteparmo aveparfi moycred3 engageml
coolinears = names(normalized_Xqu) %in% c("mtbon", "nbbon", "mteparmo", "aveparfi", "moycred3", "engageml")
normalized_Xqu = normalized_Xqu[ !coolinears ]

#vifstep fait une recherche des variables problèmatiques en enlenvant les variables une par une si elles ont un VIF > 10 (ici)
#puis test les VIF et enlève celles qu'il voit
vifstep(normalized_Xqu,th = 10)
#Les variables à supprimer sont aveparmo nbeparmo avtscpte
coolinears2 = names(normalized_Xqu) %in% c("aveparmo", "nbeparmo", "avtscpte")
normalized_Xqu = normalized_Xqu[ !coolinears2 ]

################### Application de technique de classification supervisée ###################

#On va séparer les donnnées en ensemble de train et de test 3/4 1/4
train_xqu = normalized_Xqu[1:804,]
train_yqu = Yqu$cartevpr[1:804]
test_xqu = normalized_Xqu[805:1073,]
test_yqu = Yqu$cartevpr[805:1073]

#Tout d'abord un KNN
library(class)
best_k = 0
best_KNN_acc = 0
#On va faire varier le K de 1 à 50
for (i in 1:50) {
  tmp_knn = knn(train_xqu, test_xqu, cl = train_yqu, k = i)
  tmp_acc = 1 - ( sum(tmp_knn != test_yqu) / length(test_yqu) )
  if(tmp_acc > best_KNN_acc){
    best_KNN_acc = tmp_acc
    best_k = i
  }
}
best_KNN_acc
best_k
#Meilleur accuracy 0.802974 pour K = 10

#SVM
library(e1071)
SVM_kernels = c("linear","polynomial","radial","sigmoid")
result_svm = tune(svm, train.x = train_xqu, train.y = train_yqu, validation.x = test_xqu, validation.y = test_yqu, ranges = list(kernel = SVM_kernels, cost = c(0.001, 0.01, 0.1, 1,5,10,100)))
#On obtient un précision de 88% kernel gaussien et cout de 1

#LDA
library(MASS)
lda_result_qu = lda(train_xqu,train_yqu)
lda_predict_qu = predict(lda_result_qu,test_xqu)
accuracy_lda_qu = 1 - ( sum(lda_predict_qu$class != test_yqu) / length(test_yqu) )
accuracy_lda_qu
#La LDA obtient une précision de 81%

