library(tidyverse)
elections <- read_csv("data/Presidentielle_2017_Resultats_Communes_T1_clean.csv", 
                      col_types = cols(
                        .default = col_double(),
                        CodeInsee = col_character(),
                        CodeDepartement = col_character(),
                        Département = col_character(),
                        Commune = col_character(),
                        Inscrits = col_integer(),
                        Abstentions = col_integer(),
                        Votants = col_integer(),
                        Blancs = col_integer(),
                        Nuls = col_integer(),
                        Exprimés = col_integer(),
                        `LE PEN` = col_integer(),
                        MÉLENCHON = col_integer(),
                        MACRON = col_integer(),
                        FILLON = col_integer(),
                        LASSALLE = col_integer(),
                        `DUPONT-AIGNAN` = col_integer(),
                        HAMON = col_integer(),
                        ASSELINEAU = col_integer(),
                        POUTOU = col_integer(),
                        ARTHAUD = col_integer()
                        # ... with 1 more columns
                      ))


pres2017T1_communes_long <- elections %>% 
  select(- contains("_ins")) %>%
  select(- contains("_exp")) %>%   
  gather(candidat, voix, `LE PEN`:CHEMINADE)

pres2017T1_communes_long <- pres2017T1_communes_long %>%
  group_by(CodeInsee) %>% 
  arrange(desc(voix)) %>% 
  slice(1) %>% 
  mutate(ins = voix / Inscrits * 100) %>% 
  mutate(exp = voix / Exprimés * 100) %>%
  glimpse

pres2017T1_communes_tres_long <- pres2017T1_communes_long %>% 
  gather(metrique, valeur, voix:exp) %>% 
  glimpse

pres2017T1_communes_long %>% 
  gather(metrique, valeur, voix:exp) %>% 
  unite(variable, candidat, metrique, sep = "_") %>% 
  glimpse


