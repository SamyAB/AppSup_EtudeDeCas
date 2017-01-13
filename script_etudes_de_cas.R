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

