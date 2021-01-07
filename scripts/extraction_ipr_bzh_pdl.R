rm(list=ls())

library(aspe)
library(tidyverse)

load(file  = "processed_data/tables_selectionnees.RData")

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
