detach("package:aspe", unload = TRUE)

library(aspe)
library(tidyverse)

rm(list=ls())

mon_fichier_dump <- "raw_data/aspe.sql.gz"

importer_dump_sql(fichier_dump = mon_fichier_dump)

scinder_dump(fichier_dump = mon_fichier_dump)

load(file = "raw_data/lignes_autres_tables.RData")

# importer_tables_a_partir_des_lignes(lignes_dump = lignes_autres_tables,
#                                     tables_a_extraire = c("ambiance", "facies"))
# environ 5'
tictoc::tic()
importer_tables_a_partir_des_lignes(lignes_dump = lignes_autres_tables)
tictoc::toc()

# On ne garde que les tables dont on a besoin
gdata::keep(operation, operation_description_peche, operation_donnees_environnementales, operation_ipr,
            station, point_prelevement, libelles, lot_poissons, parametres, parametres_globaux,
            ref_type_projection, prelevement_elementaire, ref_espece, ref_methode_estimation_poids, ref_protocole,
            ref_type_lot, ref_unite_hydrographique, ref_objectif,
            sure = TRUE)

save.image(file  = "processed_data/tables_selectionnees.RData")



# -------------------------------------------------------------------------------------------

# A ce stade on a chargé toutes les tables nécessaires à la suite. C'est un peu lourd donc si l'on ne travaille
# pas France entière il est préférable de restreindre au périmètre de l'étude

# load(file  = "processed_data/tables_selectionnees.RData")

# Sélection des stations sur la base administrative - exemple sur les départements
depts_bzh_pdl <- c(22, 29, 35, 56, 44, 53, 72, 49, 85)

id_stations_bzh_pdl <- station %>%
  mutate(sta_dept = str_sub(sta_com_code_insee, start = 1, end = 2)) %>%
  filter(sta_dept %in% depts_bzh_pdl) %>%
  pull(sta_id)

# On peut ensuite filtrer les tables sur ce vecteur

station_bzh_pdl <- station %>%
  filter(sta_id %in% id_stations_bzh_pdl)


# Sélection sur la base géographique
# On ne peut pas toujours filtrer sur base administrative. Par ex les contours des bassins versants ...
# On utilise donc un polygone pour faire la sélection (ne sont conservées que les observations à l'intérieur).
# Petit problème, plusieurs systèmes de coordonnées coexistent dans la base
# => besoin d'homogénéiser pour que tous les objets soient dans le même système de coordonnées (CRS).
# Dans cet exemple on bascule tout en WGS84, le système utilisé par le GPS.

# Récupération des systèmes de coordonnées
# Il manque le code epsg pour le lambert II étendu donc on le complète.
ref_type_projection <- ref_type_projection %>%
  mutate(typ_code_epsg = ifelse((is.na(typ_code_epsg) & typ_libelle_sandre == "Lambert II Etendu"),
                                yes = 27572,
                                no = typ_code_epsg))

station <- station %>%
  left_join(y = ref_type_projection,
            by = c("sta_typ_id" = "typ_id"))

# Homogénéisation des systèmes de coordonnées et suppression de colonnes inutiles
coords_wgs84 <- convertir_coords_df(df = station,
                                    var_x = "sta_coordonnees_x",
                                    var_y = "sta_coordonnees_y",
                                    var_crs_initial = "typ_code_epsg",
                                    crs_sortie = 4326) %>%
  rename(x_wgs84 = X, y_wgs84 = Y)

station <- station %>%
  bind_cols(coords_wgs84) %>%
  select(-(sta_geometrie:typ_code_epsg))


save(station, file = "processed_data/station.RData")

# téléchargement du shapefile des contours des départements en WGS84
url_depts <- "https://www.data.gouv.fr/fr/datasets/r/3096e551-c68d-40ce-8972-a228c94c0ad1"
telecharger_depts(url = url_depts,
                  repertoire = "raw_data")

# Création d'un polygone englobant les départements choisis entouré d'un buffer.
#
region <- creer_polygone_region (couche_shp_departements = "raw_data/departements-20140306-100m.shp",
                                 departements_selectionnes = c("22", "29", "35", "56"),
                                 distance_buffer = 0.01,
                                 intitule_region = "Bretagne")

mapview::mapview(region)

# Sélection des stations qui sont dans ma région
station_geo <- station %>%
  sf::st_as_sf(coords = c("x_wgs84", "y_wgs84"), crs = 4326)

mapview::mapview(station_geo)

station_bzh <- sf::st_join(station_geo, region) %>%
  filter(!is.na(region))

mapview::mapview(region, alpha.regions = 0.1,
                 map.types = c("OpenStreetMap", "CartoDB.Positron", "CartoDB.DarkMatter",
                               "Esri.WorldImagery", "OpenTopoMap")) +
  mapview::mapview(station_bzh, alpha = 0.9, col.regions = 'blue')

coords <- station_bzh %>%
  sf::st_coordinates() %>%
  as.data.frame() %>%
  purrr::set_names(c("sta_x_wgs84", "sta_y_wgs84"))

station <- station_bzh %>%
  sf::st_drop_geometry() %>%
  bind_cols(coords) %>%
  select(-region)

save(station, file = "processed_data/station.RData")

# On peut procéder de même pour d'autres dataframes contenant des coordonnées, comme celui des points.

point_prelevement <- point_prelevement %>%
  left_join(y = ref_type_projection,
            by = c("pop_typ_id" = "typ_id"))

tictoc::tic()
coords_wgs84 <- convertir_coords_df(df = point_prelevement,
                                    var_x = "pop_coordonnees_x",
                                    var_y = "pop_coordonnees_y",
                                    var_crs_initial = "typ_code_epsg",
                                    crs_sortie = 4326) %>%
  rename(pop_x_wgs84 = X, pop_y_wgs84 = Y)
tictoc::toc()

point_prelevement <- point_prelevement %>%
  bind_cols(coords_wgs84) %>%
  select(-(pop_com_code_insee_wama:pop_fog_id_cerema), -(pop_uti_id:typ_code_epsg))

save(point_prelevement, file = "processed_data/point_prelevement.RData")

# Sélection des stations qui sont dans ma région
point_prelevement_geo <- point_prelevement %>%
  sf::st_as_sf(coords = c("pop_x_wgs84", "pop_y_wgs84"), crs = 4326)

mapview::mapview(point_prelevement_geo)

point_prelevement_bzh <- sf::st_join(point_prelevement_geo, region) %>%
  filter(!is.na(region))

mapview::mapview(region, alpha.regions = 0.1,
                 map.types = c("OpenStreetMap", "CartoDB.Positron", "CartoDB.DarkMatter",
                               "Esri.WorldImagery", "OpenTopoMap")) +
  mapview::mapview(point_prelevement_bzh, alpha = 0.9, col.regions = 'blue')

coords <- point_prelevement_bzh %>%
  sf::st_coordinates() %>%
  as.data.frame() %>%
  purrr::set_names(c("pop_x_wgs84", "pop_y_wgs84"))

point_prelevement <- point_prelevement_bzh %>%
  sf::st_drop_geometry() %>%
  bind_cols(coords) %>%
  select(-region)

save(point_prelevement, file = "processed_data/point_prelevement.RData")


# ------------------------------------------------------------------------------------
rm(list=ls())
load(file = "processed_data/tables_selectionnees.RData")
load(file = "processed_data/point_prelevement.RData")
load(file = "processed_data/station.RData")
# A ce stade, on a donc la possibilité de filtrer toutes la base aspe de façon à n'en conserver que les
# éléments localisés dans un périmètre géographique. A cette fin, on constitue des listes d'identifiants
# pour la région dans chaque table.

liste_sta_id <- station %>%
  pull(sta_id)

liste_pop_id <- point_prelevement %>%
  pull(pop_id)

operation <- operation %>%
  filter(ope_pop_id %in% liste_pop_id)

liste_ope_id <- operation %>%
  pull(ope_id)

operation_description_peche <- operation_description_peche %>%
  filter(odp_ope_id %in% liste_ope_id)

operation_donnees_environnementales <- operation_donnees_environnementales %>%
  filter(ode_ope_id %in% liste_ope_id)

operation_ipr <- operation_ipr %>%
  filter(opi_ope_id %in% liste_ope_id)

prelevement_elementaire <- prelevement_elementaire %>%
  filter(pre_ope_id %in% liste_ope_id)

liste_pre_id <- prelevement_elementaire %>%
  pull(pre_id)

lot_poissons <- lot_poissons %>%
  filter(lop_pre_id %in% liste_pre_id)

save.image(file = "processed_data/data.RData")


# -------------------------------------------------------------------------------------------

# Calcul des abondances sur chaque opération de pêche






# Valeur IPR par opération (station, année)

ipr <- operation_ipr %>%
  mutate(opi_date_execution = as.character(opi_date_execution),
         annee = lubridate::ymd(opi_date_execution),
         annee = lubridate::year(annee)) %>%
  select(opi_ope_id, opi_date_execution, annee, opi_ipr) %>%
  left_join(y = operation %>% select(ope_id, ope_pop_id),
            by = c("opi_ope_id" = "ope_id")) %>%
  left_join(y = point_prelevement %>% select(pop_id, pop_sta_id, pop_libelle_wama),
            by = c("ope_pop_id" = "pop_id")) %>%
  left_join(y = station %>% select(sta_id, sta_libelle_sandre),
            by = c("pop_sta_id" = "sta_id"))

write.csv2(ipr, file = "processed_data/ipr_bzh.csv", row.names = FALSE)

















