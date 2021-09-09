load(file = "processed_data/toutes_tables_aspe_sauf_mei.RData")

expl_trouver_variable("int_id")
expl_trouver_variable("uti_id")

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
         ope_uti_id_derniere_modification) %>%
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


