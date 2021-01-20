rm(list=ls())
detach("package:aspe", unload = TRUE)

# Chargement des packages
library(aspe)
library(tidyverse)

mon_fichier_dump <- "raw_data/2020_01_12_aspe.sql.gz"
scinder_dump(fichier_dump = mon_fichier_dump)
lignes_autres_tables <- lire_lignes_dump_sql(fichier_dump = mon_fichier_dump)
importer_tables_a_partir_des_lignes(lignes_dump = lignes_autres_tables,
                                    tables_a_extraire = c("point_prelevement"))


# Chargement des tables transmises par Thierry
repo <- 'raw_data/verif_2021_01_12/'
file_names <- list.files(repo) %>% str_subset(pattern = '.csv')
files <- paste0(repo, file_names)

df_names <- file_names %>%
  str_sub(1, -5) %>%
  str_sub(6, -1) %>%
  paste0('_1')

# marche mais lent (pas réussi avec fread)
# for(i in 1:14) {
#   assign(df_names[i], read.table(files[i], encoding = "UTF-8", fill = TRUE,
#                                  header = TRUE))
# }

# Suite au retour de Thierry, vérifications sur point_prelevement.
point_prelevement_strquote_1 <- data.table::fread("raw_data/verif_2021_01_12/aspe.point_prelevement_strquote.csv",
                                                  encoding  ="UTF-8")


identical(point_prelevement_strquote_1, point_prelevement)
class(point_prelevement_strquote_1)
class(point_prelevement)

pp <- point_prelevement %>% arrange(pop_id)
pp1 <- point_prelevement_strquote_1 %>% arrange(pop_id) %>%
  as.data.frame()

compare_id <- function(df1, df2, i) {
  identical(df1[,i], df2[,1])
}
compare_id(df1 = pp, df2 = pp1, i = 2)

i <- 3
comp <- cbind(pp = pp[,i], pp1 = pp1[,i]) %>%
  as.data.frame() %>%
  mutate(egal = (pp == pp1)) %>%
  # filter(egal == TRUE) %>%
  # nrow()


identical(pp[,1], pp1[,1])


identical(names(point_prelevement_strquote_1), names(point_prelevement))

identical(point_prelevement_strquote_1$pop_id, point_prelevement$pop_id)
setdiff(point_prelevement$pop_id, point_prelevement_strquote_1$pop_id)

i <- 1
pp = pp[,i]
pp_strquote = pp1[,i]

comp <- cbind(pp = pp[,!!as.name(i)], ) %>%
  mutate(egal = (pp == pp_strquote))








# Si l'on connaît les noms des tables dont on aura besoin, on les charge directement.
importer_tables_a_partir_des_lignes(lignes_dump = lignes_autres_tables,
                                    tables_a_extraire = c("station", "point_prelevement", "operation", "prelevement_elementaire",
                                                          "lot_poissons", "ambiance", "groupe_points", "operation_description",
                                                          "operation_donnees_environnementales", "operation_ipr_plus", "operation_ipr",
                                                          "operation_objectif", "passage", "pathologie_lot", "pathologie_poisson"))


rm(lignes_autres_tables, df_names, file_names, files, i, mon_fichier_dump, repo)
save.image(file = "processed_data/tables_verif_2021_02_11.RData")


load(file = "processed_data/tables_verif_2021_02_11.RData")

ambiance_diff <- setdiff(ambiance_verif$amb_id, ambiance$amb_id)

operation_objectif_verif %>%
  filter(opo_ope_id %in% diff) %>%
  View

devtools::install_github("pascalirz/aspe")



# ----------------------------------------------------------------------------------
# envoi des tables à Thierry.
# commencer par tout effacer
# rm(list = ls())
mon_fichier_dump <- "raw_data/2020_01_12_aspe.sql.gz"
importer_dump_sql(fichier_dump = mon_fichier_dump)
rm(mon_fichier_dump)

# dfs <- Filter(function(x) is.data.frame(get(x)) , ls()) # noms des dataframes
dfs <- ls()
file_names <- paste0('processed_data/exports_pour_thierry/', dfs, '.csv')

my_write <- function(df_name, csv_name) {
  df_name %>% get() %>% write_csv(path = csv_name, na = "")
}

my_write(df_name = dfs[3],
         csv_name = file_names[3])

map2(.x = dfs,
     .y = file_names,
     .f = my_write)

files2zip <- dir('processed_data/exports_pour_thierry/', full.names = TRUE)
zip(zipfile = 'processed_data/exports_pour_thierry/export_r',
    files = files2zip)


# ----------------------------------------------------------------------------------
# Recherche d'un champ parmi les tables

st_id; st_nom; st_long;
ope_id; ope_date; ope_methode; ope_moyen; ope_surf; ope_nb point; ope_nb zone; ope_long; ope_larg; ope_prof;
numero_passage;
code_esp; effectif; biomasse

noms_tables <- Filter(function(x) is.data.frame(get(x)), ls())

noms_champs <- map(.x = noms_tables,
                   .f = function(x) names(get(x))) %>%
               unlist() %>%
               str_replace_all(pattern = '\"', replacement = "")

tous_id <- noms_champs %>%
  str_subset(pattern = '_id')

mon_pattern <- 'den'
mes_champs <- noms_champs  %>%
  str_subset(pattern = mon_pattern)


# ----------------------------------------------------------------------------------
# vérificatiosn opérations en 2020
# au 19/01/2021, il y a en ligne sur la base 209 opérations enregistrées

depts_bzh <- c(22)

id_stations_bzh <- station %>%
  mutate(sta_dept = str_sub(sta_com_code_insee, start = 1, end = 2)) %>% # création du champ sta_dept
  filter(sta_dept %in% depts_bzh) %>% # sélection des observations sur les numéros de dept
  pull(sta_id) # extraction des identifiants des stations

bzh_2020 <- passerelle %>%
  filter(sta_id %in% id_stations_bzh) %>%
  left_join(y = operation %>% select(ope_id, ope_date)) %>%
  mutate(ope_date  = as.character(ope_date),
         ope_date = lubridate::ymd_hms(ope_date),
         annee = lubridate::year(ope_date)) %>%
  filter(annee == 2020) %>%
  pull(ope_id) %>%
  unique()






