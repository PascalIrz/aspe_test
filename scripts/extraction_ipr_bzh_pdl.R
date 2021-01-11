rm(list=ls())

library(aspe)
library(tidyverse)

load(file = "processed_data/tables_selectionnees.RData")


# La base Aspe comprend plus d'une centaine de tables dont la plupart contiennent des référentiels associant des codes
# à des modalités. Par exemple la table ref_protocole associe le code pro_id à la modalité
# "Pêche partielle par points (grand milieu). Ces tables sont dites "périphériques".
# Chaque table comprend une clé primaire (suffixe "_id"), identifiant unique de chacune de ses lignes.
# Les tables qui constituent la colonne vertébrale de la base sont au nombre de ... :
# - station
# - point_prelevement
# - operation
# - prelevement_elementaire
# - lot_poissons
# - mesure_individuelle
#
# Ces tables sont liées de manières hiérarchique. Ainsi, chaque mesure individuelle se rapporte à un lot, qui
# se rapporte à un prélèvement élémentaire, qui se rapporte à une opération ... jusqu'à la station.
# Pour savoir sur quelle station a été capturé ce goujon de 87mm, il faut donc remonter toute la chaîne.
# Pour simplifier cette procédure, et éviter d'avoir à la reproduire à chaque requête de sélection,
# on peut construire un tableau de correspondance des clés primaires. On n'incluera toutefois pas la
# table mesure_individuelle car elle compren des millions de lignes, ce qui alourdirait considérablement
# le tableau. Le traitement du contenu de cette table sera abordé ultérieurement.

# Construction d'une table "passerelle"

passerelle <- lot_poissons %>% select(lop_id, pre_id = lop_pre_id) %>%
  left_join(y = prelevement_elementaire %>% select(pre_id, ope_id = pre_ope_id)) %>%
  left_join(y = operation %>% select(ope_id, pop_id = ope_pop_id)) %>%
  left_join(y = point_prelevement %>% select (sta_id = pop_sta_id, pop_id, sta_id = pop_sta_id)) %>%
  select(sta_id, pop_id, ope_id, pre_id, lop_id)

# Sélection des stations sur la base administrative - exemple sur les départements
depts_bzh_pdl <- c(22, 29, 35, 56, 44, 53, 72, 49, 85)

id_stations_bzh_pdl <- station %>%
  mutate(sta_dept = str_sub(sta_com_code_insee, start = 1, end = 2)) %>%
  filter(sta_dept %in% depts_bzh_pdl) %>%
  pull(sta_id)

# On peut ensuite filtrer les tables sur ce vecteur. Par exemple :

station_bzh_pdl <- station %>%
  filter(sta_id %in% id_stations_bzh_pdl)

# Bilan des ipr par station depuis 2010

ipr <- passerelle %>%
  filter(sta_id %in% id_stations_bzh_pdl) %>%
  left_join(y = operation %>% select(ope_id, ope_date)) %>%
  mutate(ope_date = as.character(ope_date),
         annee = lubridate::ymd_hms(ope_date),
         annee = lubridate::year(annee)) %>%
  left_join(y = operation_ipr %>% select(opi_ope_id, ipr = opi_ipr),
            by = c("ope_id" = "opi_ope_id")) %>%
  filter(!is.na(ipr)) %>%
  left_join(y = point_prelevement %>% select(pop_id, pop_sta_id, pop_libelle_wama)) %>%
  left_join(y = station %>% select(sta_id, sta_libelle_sandre, sta_com_code_insee)) %>%
  mutate(dept = str_sub(sta_com_code_insee, start = 1, end = 2),
         classe_ipr = cut(ipr,
                          breaks = c(-99, 7, 16, 25, 36, 1e6),
                          labels = c("Très bon", "Bon", "Moyen", "Médiocre", "Mauvais"))) %>%
  select(libelle_station = sta_libelle_sandre,
         dept,
         sta_id,
         libelle_point = pop_libelle_wama,
         pop_id,
         ope_id,
         annee,
         date_operation = ope_date,
         ipr,
         classe_ipr) %>%
  distinct()

# points avec plus d'une pêche dans la même année ?
ipr %>%
  filter(annee > 2009) %>%
  group_by(sta_id, ope_id, pop_id, annee) %>%
  tally() %>%
  arrange(-n) %>% filter(n > 1) %>%
  View

ipr_large <- ipr %>%
  select(libelle_station, libelle_point, dept, annee, ipr, pop_id) %>%
  filter(annee > 2009) %>%
  distinct() %>%
  pivot_wider(names_from = annee, values_from = ipr, names_sort = TRUE)

write.csv2(ipr_large, file = "processed_data/ipr_bzh_pdl_large.csv", row.names = FALSE,
           na = "")

n_mini_annees <- 7

stations_suivies <- ipr_bzh_pdl %>%
  group_by(sta_id) %>%
  tally() %>%
  filter(n >= n_mini_annees) %>%
  pull(sta_id)

data <- ipr_bzh_pdl %>%
  filter(sta_id %in% stations_suivies) %>%
  mutate(nom = paste(dept, libelle_station) %>% str_wrap(width = 20))

ggplot(data = data, aes(x = annee, y = ipr)) +
    geom_point() +
  facet_wrap(~nom)

# -------------------------------------------------------------------------------
# Mise en forme pour entrée dans macro Excel de calcul de l'ipr

library(lubridate)

date_debut <- '01-01-2020'
date_fin <- '31-12-2020'

data <- passerelle %>%
  select(sta_id, pop_id, ope_id, pre_id) %>%
  filter(sta_id %in% id_stations_bzh_pdl) %>%
  distinct() %>%
  left_join(y = operation_ipr %>% select(ope_id = opi_ope_id, opi_ipr, starts_with("opi_param"))) %>%
  filter(!is.na(opi_ipr)) %>%
  left_join(y = operation %>% select(ope_id, ope_date)) %>%
  left_join(y = point_prelevement %>% select(pop_id, pop_enh_id, pop_libelle_wama)) %>%
  left_join(y = station %>% select(sta_id, sta_libelle_sandre)) %>%
  mutate(ope_date = ymd_hms(ope_date)) %>%
  filter(ope_date <= dmy(date_fin) & ope_date >= dmy(date_debut)) %>%
  left_join(y = lot_poissons %>% select(pre_id = lop_pre_id, esp_id = lop_esp_id, lop_effectif)) %>%
  left_join(y = ref_espece %>% select(esp_id, esp_code_alternatif)) %>%
  left_join(ref_unite_hydrographique %>% select(unh_code_sandre, unh_libelle),
            by = c("opi_param_bassin" = "unh_code_sandre")) %>%
  mutate(coursdo = NA) %>%
  select(ope_id, coursdo, sta_libelle_sandre, ope_date, opi_param_surf, opi_param_sbv, opi_param_ds,
         opi_param_lar, opi_param_pent, opi_param_prof, opi_param_alt, opi_param_tjuillet, opi_param_tjanvier,
         opi_param_bassin, esp_code_alternatif, lop_effectif)

data <- data %>%
  group_by_at(setdiff(names(.), "lop_effectif")) %>%
        summarise(effectif = sum(lop_effectif, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = esp_code_alternatif, values_from = effectif)


# remarque pas trouvé comment rajouter le nom du cours d'eau - ne fonctionne pas avec jointure sur enh_id
# à partir de la table ref_entite_hydrographique
  # left_join(y = ref_entite_hydrographique %>% select(enh_id, enh_libelle_sandre),
  #           by = c("pop_enh_id" = "enh_id"))

noms_colonnes <- readxl::read_xlsx("raw_data/MacroIPR_Sortie.xlsx", skip = 1)

noms_especes <- noms_colonnes %>%
  select(ABLab:VANab) %>%
  names() %>%
  str_sub(start = 1, end = 3)

# il manque les espèces suivantes :
noms_especes_manquantes <- setdiff(noms_especes, names(data))

# il faut les rajouter
data <- data %>%
  `is.na<-`(noms_especes_manquantes)

ref_operation <- data %>%
  select(ope_id:ope_date) %>%
  mutate(ope_date = format(ope_date, format = "%d/%m/%Y"))

names(ref_operation) <- noms_colonnes %>%
  select(1:4) %>%
  names()

var_env <- data %>%
  select(opi_param_surf:opi_param_bassin)

names(var_env) <- noms_colonnes %>%
  select(6:15) %>%
  names()

captures <- data %>%
  select(noms_especes)

final <- bind_cols(ref_operation, var_env, captures)

write.csv2(final, "processed_data/aspe_format_macro.csv",
           row.names = FALSE,
           na = "")

