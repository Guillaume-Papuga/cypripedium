# Charger les packages nécessaires

library(tidyverse)

# Lire le fichier (adapter le chemin et le séparateur si besoin)

data <- read.delim("detections_individuelles_verifsTC+LD_18dec.csv", sep = ";", header = TRUE)

# Nettoyage de base : on garde 1 ligne par quadrat unique dans chaque session et site

data_clean <- data %>% distinct(site, session, numero_quadrat)

# Créer un tableau récapitulatif : liste des quadrats par site et par session

table_quadrats <- data_clean %>% arrange(site, session, numero_quadrat) %>% group_by(site, session) %>% summarise(quadrats_presents = paste(sort(unique(numero_quadrat)), collapse = ", "), n_quadrats = n(), .groups = "drop")

# 1️⃣ Récupérer la liste complète des quadrats existants

tous_quadrats <- data_clean %>% distinct(numero_quadrat) %>% arrange(numero_quadrat) %>% pull(numero_quadrat)

# 2️⃣ Transformer la colonne "quadrats_presents" en lignes

presence_long <- table_quadrats %>% mutate(quadrats_presents = strsplit(quadrats_presents, ",s*")) %>% unnest(quadrats_presents) %>% mutate(numero_quadrat = as.numeric(quadrats_presents), present = 1) %>% select(site, session, numero_quadrat, present)

# 3️⃣ Créer toutes les combinaisons possibles site × session × quadrat

complet <- expand_grid( site = unique(presence_long$site),
  session = unique(presence_long$session), numero_quadrat = tous_quadrats )

# 4️⃣ Joindre pour marquer les absents

presence_complet <- complet %>% left_join(presence_long, by = c("site", "session", "numero_quadrat")) %>% mutate(present = ifelse(is.na(present), 0, present))

# 5️⃣ Visualisation

ggplot(presence_complet, aes(x = as.factor(numero_quadrat), y = as.factor(session), fill = as.factor(present))) + geom_tile(color = "white") + scale_fill_manual(values = c("0" = "grey85", "1" = "steelblue"), labels = c("Absent", "Présent"), name = "Présence du quadrat") + facet_wrap(~ site, scales = "free_x") + labs(title = "Présence / absence des quadrats par session et site", x = "Numéro de quadrat", y = "Session") + theme_minimal() + theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), panel.grid = element_blank() )


##Tablea session x site x quadrat x observateur

# 1️⃣ Nettoyage de base : garder 1 ligne par quadrat unique dans chaque session, site et observateur
data_clean <- data %>% distinct(site, session, numero_quadrat, observateur)

# 2️⃣ Créer un tableau récapitulatif par site, session et observateur
table_quadrats <- data_clean %>%
  arrange(site, session, numero_quadrat) %>%
  group_by(site, session, observateur) %>%
  summarise(quadrats_presents = paste(sort(unique(numero_quadrat)), collapse = ", "),
            n_quadrats = n(), .groups = "drop")

# 3️⃣ Transformer la colonne "quadrats_presents" en lignes et garder observateur
presence_long <- table_quadrats %>%
  mutate(quadrats_presents = strsplit(quadrats_presents, ",\\s*")) %>%
  unnest(quadrats_presents) %>%
  mutate(numero_quadrat = as.numeric(quadrats_presents),
         present = 1) %>%
  select(site, session, observateur, numero_quadrat, present)

# 4️⃣ Créer toutes les combinaisons possibles site × session × observateur × quadrat
complet <- expand_grid(
  site = unique(presence_long$site),
  session = unique(presence_long$session),
  observateur = unique(presence_long$observateur),
  numero_quadrat = tous_quadrats
)

# 5️⃣ Joindre pour marquer les absents
presence_complet <- complet %>%
  left_join(presence_long, by = c("site", "session", "observateur", "numero_quadrat")) %>%
  mutate(present = ifelse(is.na(present), 0, present))

ggplot(presence_complet, aes(x = as.factor(numero_quadrat), y = as.factor(session), fill = as.factor(present))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "grey85", "1" = "steelblue"),
                    labels = c("Absent", "Présent"),
                    name = "Présence du quadrat") +
  facet_grid(observateur ~ site, scales = "free_x") +  # Observateur en lignes, site en colonnes
  labs(title = "Présence / absence des quadrats par session, site et observateur",
       x = "Numéro de quadrat",
       y = "Session") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid = element_blank()
  )


library(ggplot2)
library(plotly)

# Graphique ggplot de base
p <- ggplot(presence_complet, aes(x = as.factor(numero_quadrat), 
                                  y = as.factor(session), 
                                  fill = as.factor(present),
                                  text = paste0("Site: ", site,
                                                "<br>Session: ", session,
                                                "<br>Observateur: ", observateur,
                                                "<br>Quadrat: ", numero_quadrat,
                                                "<br>Présent: ", ifelse(present==1,"Oui","Non")))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "grey85", "1" = "steelblue"),
                    labels = c("Absent", "Présent"),
                    name = "Présence du quadrat") +
  facet_wrap(~ site + observateur, scales = "free_x") +
  labs(title = "Présence / absence des quadrats par session, site et observateur",
       x = "Numéro de quadrat",
       y = "Session") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid = element_blank()
  )

# Transformer en plot interactif
ggplotly(p, tooltip = "text")





library(tidyverse)
library(plotly)

# Lire les données
data <- read.delim("Taille et phéno verif LD.csv", sep = ";", header = TRUE)

# Nettoyage : 1 ligne par site et quadrat
data_clean <- data %>% distinct(site, quadrat)

# Liste complète des quadrats
tous_quadrats <- sort(unique(data_clean$quadrat))

# Créer toutes les combinaisons site × quadrat
complet <- expand_grid(
  site = unique(data_clean$site),
  quadrat = tous_quadrats
)

# Marquer les quadrats présents
presence_complet <- complet %>%
  left_join(data_clean %>% mutate(present = 1), by = c("site", "quadrat")) %>%
  mutate(present = ifelse(is.na(present), 0, present))

# Graphique interactif
p <- ggplot(presence_complet, aes(x = as.factor(quadrat),
                                  y = site,
                                  fill = as.factor(present),
                                  text = paste0("Site: ", site,
                                                "<br>Quadrat: ", quadrat,
                                                "<br>Présent: ", ifelse(present==1,"Oui","Non")))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "grey85", "1" = "steelblue"),
                    labels = c("Absent", "Présent"),
                    name = "Présence du quadrat") +
  labs(title = "Présence / absence des quadrats par site",
       x = "Numéro de quadrat",
       y = "Site") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid = element_blank()
  )

# Convertir en plot interactif
ggplotly(p, tooltip = "text")







## Lire les données
data <- read.delim("Recouvrement VF.csv", sep = ";", header = TRUE)

# Nettoyage : 1 ligne par site et quadrat
data_clean <- data %>% distinct(Site, Quadrat)

# Liste complète des quadrats
tous_quadrats <- sort(unique(data_clean$Quadrat))

# Créer toutes les combinaisons site × quadrat
complet <- expand_grid(
  Site = unique(data_clean$Site),
  Quadrat = tous_quadrats
)

# Marquer les quadrats présents
presence_complet <- complet %>%
  left_join(data_clean %>% mutate(present = 1), by = c("Site", "Quadrat")) %>%
  mutate(present = ifelse(is.na(present), 0, present))

# Graphique interactif
p <- ggplot(presence_complet, aes(x = as.factor(Quadrat),
                                  y =Site,
                                  fill = as.factor(present),
                                  text = paste0("Site: ", Site,
                                                "<br>Quadrat: ", Quadrat,
                                                "<br>Présent: ", ifelse(present==1,"Oui","Non")))) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "grey85", "1" = "steelblue"),
                    labels = c("Absent", "Présent"),
                    name = "Présence du quadrat") +
  labs(title = "Présence / absence des quadrats par site",
       x = "Numéro de quadrat",
       y = "Site") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid = element_blank()
  )

# Convertir en plot interactif
ggplotly(p, tooltip = "text")

