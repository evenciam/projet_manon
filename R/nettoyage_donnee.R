library(dplyr)
library(forcats)

data <- readxl::read_excel("Data/data1.xlsx") 

colnames(data)=gsub("\\s",".",colnames(data))
colnames(data)=gsub("./.",".",colnames(data))
colnames(data)=gsub("-",".",colnames(data))
colnames(data)=gsub("é","e",colnames(data))
colnames(data)=gsub("?","",colnames(data))
colnames(data)=gsub("à","a",colnames(data))
colnames(data)=gsub("è","e",colnames(data))
colnames(data)=gsub("[/(,/)]","",colnames(data))

# suppression des colonnes qui possede plus de 80% de valeur manquantes
data2 <- data[, colMeans(is.na(data)) < 0.95]

data2 <- data2 %>%
  rename(
    ID = ID,
    Heure_debut = Heure.de.debut,
    Heure_fin = Heure.de.fin,
    Email = Adresse.de.messagerie,
    Initiales = Premiere.lettre.de.votre.prenom.et.premiere.lettre.de.votre.nom.de.famille,
    Sexe = Sexe,
    Date_naissance = Date.de.naissance,
    Annee_debut_foot = Annee.de.debut.de.pratique.du.football,
    Difficultes_avant_match = `Avant.un.match.ressens.tu.des.difficultes.particulieres`,
    Niveau_difficultes = `Si.oui.a.quel.niveau`
  )


# modif age
data2$Date_naissance <- as.Date(data2$Date_naissance, format="%Y-%m-%d")
data2$Age <- 2025 - as.numeric(format(data2$Date_naissance, "%Y"))

#nb d'annee foot
data2$Nb_annee_foot <- 2025 - as.numeric(data2$Annee_debut_foot)


#temps du questionnaire
data2$Heure_debut <- as.POSIXct(data2$Heure_debut, format="%Y-%m-%d %H:%M:%S")
data2$Heure_fin <- as.POSIXct(data2$Heure_fin, format="%Y-%m-%d %H:%M:%S")
data2$Temps_en_min <- round(as.numeric(difftime(data2$Heure_fin, data2$Heure_debut, units = "mins")),2)

data2$Anxiete_avant_match <- as.numeric(data2$Anxiete_avant_match)

summary(data2)


write.csv(data2, "data/data_clean.csv", row.names = FALSE)




























