detach("package:aspe", unload = TRUE)
library(aspe)
library(tidyverse)

load("processed_data/toutes_tables_aspe_sauf_mei.RData")

# prov <- rtaxref::rt_taxa_search(rank = "ES", habitats = 7, size = 50000, page = 2)

page_nb <- 1
nb_lignes <- 2

referentiel <- list()

while(nb_lignes > 1)

{

  prov <- rtaxref::rt_taxa_search(rank = "ES", habitats = 2, size = 10000, page = page_nb)

  nb_lignes <- nrow(prov)

  referentiel[[page_nb]] <- prov

  page_nb <- 1 + page_nb


}

referentiel <- referentiel %>%
  .[1:(length(referentiel) - 1)] %>%
  reduce(rbind)










library(jsonlite)
URL <- "https://api.sandre.eaufrance.fr/referentiels/v1/apt/5231.json?outputSchema=SANDREv3.1"
dtaBrut <- fromJSON(URL)

dta<-dtaBrut[['REFERENTIELS']][['Referentiel']][['AppelTaxon']]







