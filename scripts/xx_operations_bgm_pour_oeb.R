# Recherche des opérations qui ont été menées par BGM afin de transmettre les données à l'OEB

# activation des packages
library(tidyverse)

# Chargement des données
load(file = "processed_data/toutes_tables_aspe_sauf_mei.RData")

# recherche des champs, ttes tables comprises, identifiant les intervenants et utilisateurs
expl_trouver_variable("int_id")
expl_trouver_variable("uti_id")

# simplification des tables pour ne conserver que le nécessaire
int_simp <- ref_intervenant %>%
  select(int_id,
         int_libelle_sandre)

uti_simp <- utilisateur %>%
  select(uti_id,
         int_id = uti_int_id,
         uti_mail)

ope_simp <- operation %>%
  select(ope_id,
         ope_int_id_operateur_peche,
         ope_int_id_commanditaire,
         ope_int_id_validation_technique,
         ope_uti_id_creation,
         ope_uti_id_derniere_modification)

# Jointure pour ajouter operateur, commanditaire etc. à la table ope_simp
ope_simp <- ope_simp %>%
  left_join(y = int_simp %>%
              rename(ope_int_id_operateur_peche = int_id,
                     operateur_peche = int_libelle_sandre)) %>%
  left_join(y = int_simp %>%
              rename(ope_int_id_commanditaire = int_id,
                     commanditaire = int_libelle_sandre)) %>%
  left_join(y = int_simp %>%
              rename(ope_int_id_validation_technique = int_id,
                     validation_technique = int_libelle_sandre)) %>%
  left_join(y = uti_simp %>%
              rename(ope_uti_id_creation = uti_id,
                     createur = uti_mail)) %>%
  left_join(y = uti_simp %>%
              rename(ope_uti_id_derniere_modification = uti_id,
                     modificateur = uti_mail))

# identification des opérations où soit on a "bgm" dans l'adresse mail de l'utilisateur
# (2 champs, createur et modificateur), soit on a "B.G.M." dans l'intervenant (3 champs)
ope_bgm <- ope_simp %>%
  mutate(bgm = case_when(
    str_detect(operateur_peche, pattern = "B.G.M.") ~ TRUE,
    str_detect(commanditaire, pattern = "B.G.M.") ~ TRUE,
    str_detect(validation_technique, pattern = "B.G.M.") ~ TRUE,
    str_detect(createur, pattern = "bgm") ~ TRUE,
    str_detect(modificateur, pattern = "bgm") ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  filter(bgm)


