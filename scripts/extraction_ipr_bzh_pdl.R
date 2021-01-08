rm(list=ls())

library(aspe)
library(tidyverse)

load(file = "processed_data/tables_selectionnees.RData")

# Sélection des stations sur la base administrative - exemple sur les départements
depts_bzh_pdl <- c(22, 29, 35, 56, 44, 53, 72, 49, 85)

id_stations_bzh_pdl <- station %>%
  mutate(sta_dept = str_sub(sta_com_code_insee, start = 1, end = 2)) %>%
  filter(sta_dept %in% depts_bzh_pdl) %>%
  pull(sta_id)

# On peut ensuite filtrer les tables sur ce vecteur

station_bzh_pdl <- station %>%
  filter(sta_id %in% id_stations_bzh_pdl)

point_prelevement_bzh_pdl <- point_prelevement %>%
  filter(pop_sta_id %in% id_stations_bzh_pdl)

id_points_bzh_pdl <- point_prelevement_bzh_pdl %>%
  pull(pop_id)

operation_bzh_pdl <- operation %>%
  filter(ope_pop_id %in% id_points_bzh_pdl)

id_ope_bzh_pdl <- operation_bzh_pdl %>%
  pull(ope_id)

ipr_bzh_pdl <- operation_ipr %>%
  filter(opi_ope_id %in% id_ope_bzh_pdl) %>%
  select(opi_ope_id, opi_ipr) %>%
  left_join(y = operation %>% select(ope_id, ope_pop_id, ope_date),
            by = c("opi_ope_id" = "ope_id")) %>%
  mutate(ope_date = as.character(ope_date),
         annee = lubridate::ymd_hms(ope_date),
         annee = lubridate::year(annee)) %>%
  left_join(y = point_prelevement %>% select(pop_id, pop_sta_id, pop_libelle_wama),
            by = c("ope_pop_id" = "pop_id")) %>%
  left_join(y = station %>% select(sta_id, sta_libelle_sandre, sta_com_code_insee),
            by = c("pop_sta_id" = "sta_id")) %>%
  mutate(dept = str_sub(sta_com_code_insee, start = 1, end = 2),
         classe_ipr = cut(opi_ipr,
                          breaks = c(-99, 7, 16, 25, 36, 1e6),
                          labels = c("Très bon", "Bon", "Moyen", "Médiocre", "Mauvais"))) %>%
  select(libelle_station = sta_libelle_sandre,
         dept,
         sta_id = pop_sta_id,
         libelle_point = pop_libelle_wama,
         pop_id = ope_pop_id,
         ope_id = opi_ope_id,
         annee,
         date_operation = ope_date,
         ipr = opi_ipr,
         classe_ipr)

# points avec plus d'une pêche dans la même année
ipr_bzh_pdl %>%
  filter(annee > 2009) %>%
  group_by(libelle_station, libelle_point, dept, annee) %>%
  tally() %>%
  arrange(-n) %>% filter(n > 1) %>%
  View

ipr_large <- ipr_bzh_pdl %>%
  select(libelle_station, libelle_point, dept, annee, ipr) %>%
  filter(annee > 2009) %>%
  group_by(libelle_station, libelle_point, dept, annee) %>%
      summarise(ipr = mean(ipr, na.rm = TRUE)) %>%
  ungroup() %>%
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

ipr <- operation_ipr %>%
  filter(opi_ope_id %in% id_ope_bzh_pdl) %>%
  select(opi_ope_id, opi_ipr, starts_with("opi_param")) %>%
  left_join(y = operation %>% select(ope_id, ope_pop_id, ope_date),
            by = c("opi_ope_id" = "ope_id")) %>%
  left_join(y = point_prelevement %>% select(pop_id, pop_sta_id, pop_enh_id, pop_libelle_wama),
            by = c("ope_pop_id" = "pop_id")) %>%
  left_join(y = station %>% select(sta_id, sta_libelle_sandre, sta_com_code_insee),
            by = c("pop_sta_id" = "sta_id")) %>%
  mutate(dept = str_sub(sta_com_code_insee, start = 1, end = 2),
         classe_ipr = cut(opi_ipr,
                          breaks = c(-99, 7, 16, 25, 36, 1e6),
                          labels = c("Très bon", "Bon", "Moyen", "Médiocre", "Mauvais")),
         ope_date = ymd_hms(ope_date)) %>%
  filter(ope_date <= dmy(date_fin) & ope_date >= dmy(date_debut))


ope_select <- ipr %>%
  pull(opi_ope_id)

prel_elem_select <- prelevement_elementaire %>%
  filter(pre_ope_id %in% ope_select) %>%
  pull(pre_id)

lot_select <- lot_poissons %>%
  filter(lop_pre_id %in% prel_elem_select)

captures <- lot_select %>%
  select(lop_pre_id, lop_esp_id, lop_effectif) %>%
  left_join(y = prelevement_elementaire %>% select(pre_id, pre_ope_id),
            by = c("lop_pre_id" = "pre_id")) %>%
  left_join(y = ipr, by = c("pre_ope_id" = "opi_ope_id")) %>%
  left_join(y = ref_espece %>% select(esp_id, esp_code_alternatif),
            by = c("lop_esp_id" = "esp_id")) %>%
  left_join(ref_unite_hydrographique %>% select(unh_code_sandre, unh_libelle),
            by = c("opi_param_bassin" = "unh_code_sandre"))

captures_large <- captures %>%
  mutate(coursdo = NA) %>%
  select(pre_ope_id, coursdo, sta_libelle_sandre, ope_date, opi_param_surf, opi_param_sbv, opi_param_ds,
         opi_param_lar, opi_param_pent, opi_param_prof, opi_param_alt, opi_param_tjuillet, opi_param_tjanvier,
         opi_param_bassin, esp_code_alternatif, lop_effectif)

captures_large <- captures_large %>%
  group_by_at(setdiff(names(.), "lop_effectif")) %>%
        summarise(effectif = sum(lop_effectif, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = esp_code_alternatif, values_from = effectif)


# remarque pas trouvé comment rajouter le nom du cours d'eau - ne fonctionne pas avec jointure sur enh_id
# à partir de la table ref_entite_hydrographique
  # left_join(y = ref_entite_hydrographique %>% select(enh_id, enh_libelle_sandre),
  #           by = c("pop_enh_id" = "enh_id"))

noms_colonnes <- readxl::read_xlsx("raw_data/MacroIPR_Sortie.xlsx", skip = 1)

especes <- noms_colonnes %>%
  select(ABLab:VANab) %>%
  names() %>%
  str_sub(start = 1, end = 3)

# il manque les espèces suivantes :
especes_manquantes <- setdiff(especes, names(captures_large))

# il faut les rajouter

captures_large <- captures_large %>%
  `is.na<-`(especes_manquantes)

ref_operation <- captures_large %>%
  select(pre_ope_id:ope_date) %>%
  mutate(ope_date = format(ope_date, format = "%d/%m/%Y"))

names(ref_operation) <- noms_colonnes %>%
  select(1:4) %>%
  names()

var_env <- captures_large %>%
  select(opi_param_surf:opi_param_bassin)

names(var_env) <- noms_colonnes %>%
  select(6:15) %>%
  names()

captures <- captures_large %>%
  select(especes)

final <- bind_cols(ref_operation, var_env, captures)

write.csv2(final, "processed_data/aspe_format_macro.csv",
           row.names = FALSE,
           na = "")

