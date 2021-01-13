rm(list=ls())
detach("package:aspe", unload = TRUE)

# Chargement des packages
library(aspe)
library(tidyverse)

#C Chargement des tables transmises par Thierry
repo <- 'raw_data/verif_2021_01_12/'
file_names <- list.files(repo) %>% str_subset(pattern = '.csv')
files <- paste0(repo, file_names)

df_names <- file_names %>%
  str_sub(1, -5) %>%
  str_sub(6, -1) %>%
  paste0('_1')

# marche mais lent (pas réussi avec fread)
for(i in 1:14) {
  assign(df_names[i], read.table(files[i], encoding = "UTF-8", fill = TRUE,
                                 header = TRUE))
}

i <- 13
assign(x = df_names[i],
       value = read.table(files[i], encoding = "UTF-8", fill = TRUE, header = TRUE))

# Ok jusqu'à 12 mais bloque à i = 13'
rm(point_prelevement_1)
point_prelevement_2 <- read.table(files[i], fileEncoding  = "UTF-8", fill = TRUE, header = TRUE, row.names = NULL)
point_prelevement_2 <- data.table::fread(files[i], encoding  = "UTF-8")
mon_fichier_dump <- "raw_data/2020_01_12_aspe.sql.gz"







scinder_dump(fichier_dump = mon_fichier_dump)

# Ensuite, on peut charger la partie du dump correspondant à toutes les tables sauf la plus volumineuse.
load(file = "raw_data/lignes_autres_tables.RData")

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
