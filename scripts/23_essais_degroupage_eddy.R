mon_lot <- top_lots_data %>%
  filter(lop_id == 926674) %>%
  mutate(type_taille = ifelse(mei_mesure_reelle == "t",
                              "mesure_reelle",
                              "degr_aspe"))

vect_mesures_reelles <- mon_lot %>%
  filter(mei_mesure_reelle == "t") %>%
  pull(mei_taille)

# comptages <- mon_lot %>%
#   group_by(mei_mesure_reelle, mei_taille) %>%
#   count() %>%
#   ungroup() %>%
#   pivot_wider(names_from = mei_mesure_reelle,
#               values_from = n,
#               values_fill  = 0)
#
# summarise_all(comptages, sum)

# mon_lot %>%
#   ggplot(aes(x = mei_taille,
#              fill = mei_mesure_reelle,
#              col = mei_mesure_reelle)) +
#   geom_density(alpha = 0.2)

mon_lot_alt <- mon_lot %>%
  aspeQual::qta_ajouter_mei_taille_alt() %>%
  filter(mei_mesure_reelle == "f") %>%
  mutate(mei_taille = mei_taille_alt) %>%
  select(-mei_taille_alt) %>%
  mutate(type_taille = "degr_pi")

mon_lot_alt2 <- mon_lot %>%
  filter(mei_mesure_reelle == "f")

mon_lot_alt2$mei_taille_alt_ec <- rep(vect_mesures_reelles,
                                      length.out = nrow(mon_lot_alt2))
mon_lot_alt2 <- mon_lot_alt2 %>%
  mutate(mei_taille = mei_taille_alt_ec) %>%
  select(-mei_taille_alt_ec) %>%
  mutate(type_taille = "degr_ec")

aggr <- rbind(mon_lot,
              mon_lot_alt,
              mon_lot_alt2)

ggplot(data = aggr) +
  geom_density(aes(x = mei_taille,
                   fill = type_taille,
                   col = type_taille
                   ),
               alpha = 0.2)

mes_quantiles <- aggr %>%
  group_by(type_taille) %>%
  summarise(q = list(quantile(mei_taille, probs = seq(0, 1, 0.01)))) %>%
  unnest_wider(q) %>%
  pivot_longer(cols = `0%`:`100%`) %>%
  mutate(name = str_replace(name, "%", ""),
         pourcentile = as.integer(name) / 100) %>%
  select(-name) %>%
  pivot_wider(names_from = type_taille,
              values_from = value)


gg_degr_aspe <- ggplot(data = mes_quantiles,
       aes(x = mesure_reelle,
           y = degroupage_aspe)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_abline(intercept = 0,
              slope = 1,
              col = "red",
              size = 0.5)

gg_degr_pi <- ggplot(data = mes_quantiles,
       aes(x = mesure_reelle,
           y = degr_alt_pi)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_abline(intercept = 0,
              slope = 1,
              col = "red",
              size = 0.5)

gg_degr_ec <- ggplot(data = mes_quantiles,
       aes(x = mesure_reelle,
           y = degr_alt_ec)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_abline(intercept = 0,
              slope = 1,
              col = "red",
              size = 0.5)

qq_mod_aspe <- lm(degroupage_aspe ~ mesure_reelle, data = mes_quantiles)
confint(qq_mod_aspe)

qq_mod_pi <- lm(degr_ ~ mesure_reelle, data = mes_quantiles)
confint(qq_mod_aspe)


qq_mod_aspe <- lm(degroupage_aspe ~ mesure_reelle, data = mes_quantiles)
confint(qq_mod_aspe)


