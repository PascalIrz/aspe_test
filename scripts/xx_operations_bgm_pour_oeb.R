# Recherche des opérations qui ont été menées par BGM afin de transmettre les données à l'OEB

# activation des packages
library(tidyverse)
library(aspe)

# Chargement des données
load(file = "processed_data/tables_sauf_mei_2021_09_10_14_35_58.RData")

# recherche des champs, ttes tables comprises, identifiant les intervenants et utilisateurs
expl_trouver_variable("int_id")
expl_trouver_variable("uti_id")

# en opérant des jointures sur ces champs on peut récupérer les libellés des intervenants
# ets les emails des utilisateurs. C'est ce que font les fonctions aspe mef_ajouter_utilisateurs()
# et mef_ajouter_intervenants()
ope <- mef_creer_passerelle() %>%
  mef_ajouter_utilisateurs() %>%
  mef_ajouter_intervenants()

# identification des opérations où soit on a "bgm" dans l'adresse mail de l'utilisateur
# (2 champs, createur et modificateur), soit on a "B.G.M." dans l'intervenant (3 champs)
ope_bgm <- ope %>%
  mutate(bgm = case_when(
    str_detect(operateur_peche, pattern = "B.G.M.") ~ TRUE,
    str_detect(commanditaire, pattern = "B.G.M.") ~ TRUE,
    str_detect(validation_technique, pattern = "B.G.M.") ~ TRUE,
    str_detect(createur, pattern = "bgm") ~ TRUE,
    str_detect(modificateur, pattern = "bgm") ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  filter(bgm)

## résultat : zéro opérations à partir du dump du 22/06/21


