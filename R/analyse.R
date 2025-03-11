library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggcorrplot)
library(reshape2)
library(FactoMineR)
library(factoextra)

data_clean <- read.csv("Data/data_clean.csv")


ggplot(data_clean, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution des âges des joueurs en 2025",
       x = "Âge en 2025", y = "Nombre de joueurs") +
  theme_minimal()


ggplot(data_clean, aes(x = Nb_annee_foot)) +
  geom_histogram(binwidth = 2, fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Distribution du nombre d’années de pratique du football",
       x = "Années de pratique", y = "Nombre de joueurs") +
  theme_minimal()


ggplot(data_clean, aes(x = Temps_en_min)) +
  geom_histogram(binwidth = 0.5, fill = "purple", color = "black", alpha = 0.7) +
  labs(title = "Distribution du temps passé sur le questionnaire",
       x = "Temps en minutes", y = "Nombre de joueurs") +
  theme_minimal()


ggplot(data_clean, aes(x = Sexe, y = Temps_en_min, fill = Sexe)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Comparaison du temps passé sur le questionnaire selon le sexe",
       x = "Sexe", y = "Temps en minutes") +
  theme_minimal()


ggplot(data_clean, aes(x = Anxiete_avant_match)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Distribution du niveau d’anxiété avant un match",
       x = "Niveau d’anxiété (0 à 10)", y = "Nombre de joueurs") +
  theme_minimal()


ggplot(data_clean, aes(x = Sexe, y = Anxiete_avant_match, fill = Sexe)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Comparaison de l’anxiété avant un match selon le sexe",
       x = "Sexe", y = "Niveau d’anxiété (0-10)") +
  theme_minimal()


ggplot(data_clean, aes(x = Nb_annee_foot, y = Anxiete_avant_match)) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relation entre l'expérience et l'anxiété avant un match",
       x = "Années de pratique du football", y = "Niveau d'anxiété (0-10)") +
  theme_minimal()


ggplot(data_clean, aes(x = Difficultes_avant_match)) +
  geom_bar(fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Difficultés ressenties avant un match",
       x = "Type de difficulté", y = "Nombre de joueurs") +
  theme_minimal() +
  coord_flip()  # Pour mieux lire les labels



# Sélectionner les colonnes des affirmations
affirmations <- data_clean[, grep("Affirmation", names(data_clean))]

# Convertir en format long
affirmations_melted <- melt(affirmations, id.vars = NULL, variable.name = "Affirmation", value.name = "Réponse")

# Graphe en heatmap

# Compter les occurrences des réponses pour chaque affirmation
affirmation_counts <- affirmations_melted %>%
  count(Affirmation, Réponse)

# Afficher la heatmap correctement
ggplot(affirmation_counts, aes(x = Affirmation, y = Réponse, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Répartition des réponses aux affirmations",
       x = "Affirmations", y = "Réponses", fill = "Fréquence") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()




##### ACP

# Sélectionner uniquement les variables numériques
data_num <- data_clean %>% select_if(is.numeric)

# Supprimer les lignes avec des valeurs manquantes
data_num <- na.omit(data_num)

# Vérifier la structure des données
str(data_num)

# Réaliser l'ACP sur les données numériques
res_pca <- PCA(data_num, scale.unit = TRUE, graph = FALSE)
fviz_eig(res_pca, addlabels = TRUE, ylim = c(0, 100))
fviz_pca_ind(res_pca, col.ind = "cos2", 
             gradient.cols = c("blue", "red"),
             repel = TRUE)
fviz_pca_var(res_pca, col.var = "contrib",
             gradient.cols = c("blue", "red"),
             repel = TRUE)
fviz_pca_var(res_pca, col.var = "coS2",
             gradient.cols = c("blue", "red"),
             repel = TRUE)

fviz_pca_biplot(res_pca, repel = TRUE,
                col.var = "red", col.ind = "blue")

fviz_pca_var(res_pca, select.var = list(cos2 = 0.75))

fviz_pca_var(res_pca, col.var = "cos2")

























































