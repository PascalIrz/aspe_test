sta_plus_5a <- ipr %>%
  select(libelle_station, annee) %>%
  unique() %>%
  group_by(libelle_station) %>%
  summarise(n_annee = n_distinct(annee)) %>%
  filter(n_annee > 4) %>%
  pull(libelle_station) %>%
  as.character()

mon_dept <- '22'

data <- ipr %>%
  filter(dept == mon_dept & libelle_station %in% sta_plus_5a)

grapher_ipr_plusieurs_stations <- function(ipr_df, stations_id = NA, premiere_annee = NA,
                                           derniere_annee = NA, titre = NA, palette = NA,
                                           nb_colonnes = 3)

      {

  if (is.na(palette)) {

  palette <- c("Très bon" = "darkblue", "Bon" = "darkgreen", "Moyen" = "yellow",
                  "Médiocre" = "orange", "Mauvais" = "red")
                      }

  if (!is.na(stations_id)) ipr_df <- ipr_df %>% filter(sta_id %in% stations_id)

  if (!is.na(premiere_annee))
                {
            ipr_df <- ipr_df %>% filter(annee >= premiere_annee)
                } else{
            premiere_annee <- min(ipr_df$annee, na.rm = T)
                      }

  if (!is.na(derniere_annee))
                {
            ipr_df <- ipr_df %>% filter(annee <= derniere_annee)
                } else{
            derniere_annee <- max(ipr_df$annee, na.rm = T)
                      }

  ggplot(data = ipr_df,
         aes(x = annee, y = ipr, fill = classe_ipr)) +
  geom_bar(stat = "identity") +
  labs(x = "Année", y = "IPR",
       title = titre,
       fill = "Classe IPR") +
  scale_fill_manual(values = palette) +
  facet_wrap(~libelle_station,
             ncol = nb_colonnes,
             scales = 'free') +
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''),
                     breaks = seq(premiere_annee, derniere_annee, 2),
                     limits = c(premiere_annee, derniere_annee)) +
  scale_y_continuous(limits = c(0, 40)) +
  theme(legend.position = "bottom")

}

grapher_ipr_plusieurs_stations (ipr_df = data,
                                stations_id = NA,
                                premiere_annee = 2015,
                                derniere_annee = NA,
                                titre = "Côtes d'Armor",
                                palette = NA)

