suppressPackageStartupMessages(library(tidyverse))
elections <- read_csv("data/Presidentielle_2017_Resultats_Communes_T1_clean.csv")

elections %>% 
  mutate(region = case_when(CodeDepartement %in% c("75", "77", "78", "91", "92", "93", "94", "95") ~ "Ile-de-France",
                            TRUE ~ "Province")) %>% 
  glimpse

library(readxl)
ZE2010 <- read_xls("./data/ZE2010 au 01-01-2017.xls", sheet = "Composition_communale", skip = 5)
elections <- elections %>% 
  left_join(ZE2010, by = c("CodeInsee" = "CODGEO")) %>% 
  glimpse()

elections %>% 
  group_by(ZE2010) %>% 
  summarise_at(vars(Inscrits, Abstentions, Votants, Blancs, Nuls, Exprimés, `LE PEN`:CHEMINADE), 
               funs(sum(.)))

elections %>% 
  group_by(ZE2010) %>% 
  summarise_at(vars(Inscrits, Abstentions, Votants, Blancs, Nuls, Exprimés, `LE PEN`:CHEMINADE), funs(sum(.))) %>% 
  mutate_at(vars(`LE PEN`:CHEMINADE), 
            funs(ins = . / Inscrits * 100, exp = . / Exprimés * 100)
            ) %>% 
  View

elections %>% 
  select(`LE PEN`:CHEMINADE) %>% 
  summarise_all(funs(sum(.))) %>% 
  gather(candidat, voix) %>% 
  ggplot(aes(x = candidat, y = voix)) +
  geom_bar(stat = "identity")

elections %>% 
  select(`LE PEN`:CHEMINADE) %>% 
  summarise_all(funs(sum(.))) %>% 
  gather(candidat, voix) %>% 
  arrange(desc(voix)) %>% 
  ggplot(aes(x = candidat, y = voix)) +
  geom_bar(stat = "identity")


elections %>% 
  select(`LE PEN`:CHEMINADE) %>% 
  summarise_all(funs(sum(.))) %>% 
  gather(candidat, voix) %>% 
  arrange(desc(voix)) %>% 
  mutate(candidat = forcats::fct_inorder(candidat)) %>% 
  ggplot(aes(x = candidat, y = voix)) +
  geom_bar(stat = "identity")

elections %>% 
  select(`LE PEN`:CHEMINADE) %>% 
  summarise_all(funs(sum(.))) %>% 
  gather(candidat, voix) %>% 
  arrange(voix) %>% 
  mutate(candidat = forcats::fct_inorder(candidat)) %>% 
  ggplot(aes(x = candidat, y = voix)) +
  geom_bar(stat = "identity") +
  coord_flip()

library(hrbrthemes)
elections %>% 
  select(`LE PEN`:CHEMINADE) %>% 
  summarise_all(funs(sum(.))) %>% 
  gather(candidat, voix) %>% 
  arrange(voix) %>% 
  mutate(candidat = forcats::fct_inorder(candidat)) %>% 
  ggplot(aes(x = candidat, y = voix)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_ipsum(grid = c("X"))

elections %>% 
  select(`LE PEN`:CHEMINADE) %>% 
  summarise_all(funs(sum(.))) %>% 
  gather(candidat, voix) %>% 
  arrange(voix) %>% 
  mutate(candidat = forcats::fct_inorder(candidat)) %>% 
  ggplot(aes(x = candidat, y = voix)) +
  geom_bar(stat = "identity", fill = c("#0000CD")) +
  coord_flip() +
  theme_ipsum(grid = c("X"))


library(scales)
point <- format_format(big.mark = " ", decimal.mark = ".", scientific = FALSE)

elections %>% 
  select(`LE PEN`:CHEMINADE) %>% 
  summarise_all(funs(sum(.))) %>% 
  gather(candidat, voix) %>% 
  arrange(voix) %>% 
  mutate(candidat = forcats::fct_inorder(candidat)) %>% 
  ggplot(aes(x = candidat, y = voix)) +
  geom_bar(stat = "identity", fill = c("#0000CD")) +
  coord_flip() +
  theme_ipsum(grid = c("X")) +
  labs(y = "Nombre de voix", x = "") +
  scale_y_continuous(labels = point)

elections %>% 
  select(`LE PEN`:CHEMINADE) %>% 
  summarise_all(funs(sum(.))) %>% 
  gather(candidat, voix) %>% 
  arrange(voix) %>% 
  mutate(candidat = forcats::fct_inorder(candidat)) %>% 
  ggplot(aes(x = candidat, y = voix)) +
  geom_bar(stat = "identity", fill = c("#0000CD")) +
  geom_text(aes(label = candidat)) +
  coord_flip() +
  theme_ipsum(grid = c("X")) +
  labs(y = "Nombre de voix", x = "") +
  scale_y_continuous(labels = point)

elections %>% 
  select(`LE PEN`:CHEMINADE) %>% 
  summarise_all(funs(sum(.))) %>% 
  gather(candidat, voix) %>% 
  arrange(voix) %>% 
  mutate(candidat = forcats::fct_inorder(candidat)) %>% 
  ggplot(aes(x = candidat, y = voix)) +
  geom_bar(stat = "identity", fill = c("#0000CD")) +
  geom_text(aes(label = candidat), nudge_y = -100000, hjust = "right", color = "white") +
  coord_flip() +
  scale_x_discrete(labels = NULL) +
  theme_ipsum(grid = c("X")) +
  labs(y = "Nombre de voix", x = "") +
  scale_y_continuous(labels = point)

elections %>% 
  select(`LE PEN`:CHEMINADE) %>% 
  summarise_all(funs(sum(.))) %>% 
  gather(candidat, voix) %>% 
  arrange(voix) %>% 
  mutate(candidat = forcats::fct_inorder(candidat)) %>%
  mutate(justification = if_else(voix < 1000000, "left", "right")) %>% 
  mutate(couleur = if_else(voix < 1000000, "black", "white")) %>% 
  ggplot(aes(x = candidat, y = voix)) +
  geom_bar(stat = "identity", fill = c("#0000CD")) +
  geom_text(aes(label = candidat, colour = couleur, hjust = justification), nudge_y = -100000) +
  coord_flip() +
  scale_x_discrete(labels = NULL) +
  theme_ipsum(grid = c("X")) +
  labs(y = "Nombre de voix", x = "") +
  scale_y_continuous(labels = point)

elections %>% 
  select(`LE PEN`:CHEMINADE) %>% 
  summarise_all(funs(sum(.))) %>% 
  gather(candidat, voix) %>% 
  arrange(voix) %>% 
  mutate(candidat = forcats::fct_inorder(candidat)) %>%
  mutate(justification = if_else(voix < 1000000, "left", "right")) %>% 
  mutate(couleur = if_else(voix < 1000000, "black", "white")) %>% 
  mutate(offset = if_else(voix < 1000000, 100000, -100000)) %>% 
  mutate(ybis = if_else(voix < 1000000, voix + 100000, voix - 100000)) %>% 
  ggplot(aes(x = candidat, y = voix)) +
  geom_bar(stat = "identity", fill = c("#0000CD")) +
  geom_text(aes(y = ybis, label = candidat, colour = couleur, hjust = justification)) +
  scale_color_manual(breaks = c("black", "white"), values = c("black", "white"), guide = "none") +
  coord_flip() +
  scale_x_discrete(labels = NULL) +
  theme_ipsum(grid = c("X")) +
  labs(y = "Nombre de voix", x = "") +
  scale_y_continuous(labels = point)

# camembert
elections %>% 
  select(`LE PEN`:CHEMINADE) %>% 
  summarise_all(funs(sum(.))) %>% 
  gather(candidat, voix) %>% 
  ggplot(aes(x = "", y = voix)) +
  geom_col(aes(fill = candidat), width = 1) +
  coord_polar("y", start = pi / 3)

elections %>% 
  ggplot(aes(x = `LE PEN_ins`, y = MACRON_ins)) +
  geom_point(aes(size = Inscrits), alpha = 0.2) +
  scale_size_area(max_size = 10) +
  coord_fixed()

elections %>% 
  select(Inscrits, MACRON_ins, `LE PEN_ins`) %>% 
  gather(candidat, valeur, MACRON_ins:`LE PEN_ins`) %>% 
  ggplot(aes(x = Inscrits, y = valeur)) +
  geom_point(aes(colour = candidat), alpha = 0.1) +
  scale_x_log10()

elections %>% 
  select(Inscrits, MACRON_ins, `LE PEN_ins`) %>% 
  gather(candidat, valeur, MACRON_ins:`LE PEN_ins`) %>% 
  ggplot(aes(x = Inscrits, y = valeur)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  scale_x_log10() +
  facet_wrap(~ candidat)

cairo_pdf("./img/mongraphique.pdf")
elections %>% 
  select(Inscrits, MACRON_ins, `LE PEN_ins`) %>% 
  gather(candidat, valeur, MACRON_ins:`LE PEN_ins`) %>% 
  ggplot(aes(x = Inscrits, y = valeur, group = candidat)) +
  geom_smooth(aes(colour = candidat)) +
  scale_x_log10()
dev.off()
