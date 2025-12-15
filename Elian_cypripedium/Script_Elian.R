# Charger les packages nécessaires

library(tidyverse)

# Lire le fichier (adapter le chemin et le séparateur si besoin)
data <- read.delim("detections_individuelles_verifsTC+LD_18dec.csv", sep = ";", header = TRUE)

unique(data$site)
data$site <- trimws(data$site)
unique(data$site)

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


## AFD Site avec cercle corr. ---- 

data <- read.delim("Recouvrement VF.csv", sep = ";", header = TRUE)

## --- Packages nécessaires ---
library(ade4)
library(tidyverse)
## --- Vérification ---
head(data)

## --- Séparation des variables explicatives et du facteur ---
# Variables quantitatives (strates)
vars <- data %>%
  select(S_muscinale, S_herbacee, S_arbustive_basse, S_arbustive_haute, S_arborescente)

# Facteur (ici le site)
fac <- as.factor(data$Site)

## --- Réalisation d'une ACP préalable ---
acp <- dudi.pca(vars, center = TRUE, scale = TRUE, scannf = FALSE, nf = 3)
plot(acp$li[,1], acp$li[,2], xlab="PC1", ylab="PC2", pch=19)

## --- Analyse Factorielle Discriminante (AFD) ---
afd <- discrimin(acp, fac, scannf = FALSE, nf = 3)

## --- Résumé ---
summary(afd)

## --- Graphiques ---
par(mfrow = c(1,2))
s.class(afd$li, fac, col = rainbow(length(unique(fac))),
        grid = TRUE, axesell = TRUE, cstar = 1.2,
        sub = "AFD - projection des quadrats")
s.arrow(afd$va, sub = "Corrélation des variables")

## Sans les groupes : 
# Projection sur les deux premiers axes discriminants
plot(afd$li[,1], afd$li[,2],
     xlab = "Axis 1", ylab = "Axis 2",
     main = "Projection AFD - sans groupes",
     pch = 19, col = "black")
grid()


## --- Contribution des axes ---
afd$eig / sum(afd$eig) * 100

## Jackniffe :
 
## --- Validation Jackknife (leave-one-out) -----------------------

library(MASS)

# On reprend les mêmes variables et le même facteur
data_lda <- cbind(vars, fac)
colnames(data_lda)[ncol(data_lda)] <- "groupe"

# LDA avec validation croisée
lda_jack <- lda(groupe ~ ., data = data_lda, CV = TRUE)

# Matrice de confusion
cat("\n=== Validation jackknife ===\n")
table_obs_pred <- table(Observé = data_lda$groupe, Prédit = lda_jack$class)
print(table_obs_pred)

# Taux global de bonne classification
taux_global <- mean(lda_jack$class == data_lda$groupe)
cat("\nTaux global de bonne classification :", round(taux_global * 100, 1), "%\n")

# Taux de bonne classification par groupe
taux_par_groupe <- tapply(lda_jack$class == data_lda$groupe, data_lda$groupe, mean)
cat("\nTaux de bonne classification par groupe (%) :\n")
print(round(taux_par_groupe * 100, 1))


#Croissance ----

library(dplyr)
library(tidyr)

data_long <- data %>%
  pivot_longer(
    cols = c(phenoS1, phenoS2, phenoS3, tailleS1, tailleS2, tailleS3),
    names_to = c(".value", "time"),
    names_pattern = "([a-zA-Z]+)(S[1-3])"
  ) %>%
  mutate(
    time = recode(time, S1 = 1, S2 = 2, S3 = 3),
    taille = as.numeric(as.character(taille))
  )

data_long %>%
  group_by(pheno) %>%
  summarise(
    taille_min = min(taille, na.rm = TRUE),
    taille_max = max(taille, na.rm = TRUE),
    taille_moy = mean(taille, na.rm = TRUE),
    n = n()
  )
library(ggplot2)

ggplot(data_long, aes(x = taille, fill = pheno)) +
  geom_histogram(position = "dodge", binwidth = 1) +
  labs(title = "Distribution de la taille selon le phénotype", x = "Taille", y = "Nombre d'individus")

#Tester l'effet de la taille sur la phéno. 
# Modèle logistique : pheno (j/nf) ~ taille
data_long$pheno <- factor(data_long$pheno, levels = c("nf","j"))
model_pheno <- glm(pheno ~ taille, family = binomial, data = data_long)
summary(model_pheno)


library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(MuMIn)

# Passer en format long pour les tailles
data_long <- data %>%
  pivot_longer(
    cols = c(tailleS1, tailleS2, tailleS3),
    names_to = "time",
    values_to = "taille"
  ) %>%
  mutate(
    time = recode(time, tailleS1 = 1, tailleS2 = 2, tailleS3 = 3),
    taille = as.numeric(as.character(taille))
  ) %>%
  arrange(ID_ind, time)

data_long$site <- as.factor(data_long$site)
data_long$ID_ind <- as.factor(data_long$ID_ind)

# Calcul de la croissance relative entre temps consécutifs
data_long <- data_long %>%
  group_by(ID_ind) %>%
  mutate(croissance = (taille - lag(taille)) / lag(taille)) %>%
  ungroup()

# Calcul de la survie entre temps
data_long <- data_long %>%
  group_by(ID_ind) %>%
  mutate(survie = ifelse(!is.na(taille) & lead(taille, 1) == 0, 0, 1)) %>%
  ungroup()

####Analyse de la survie ----
####Modèle logistique simple
# survie du temps t vers t+1
model_survie <- glm(survie ~ site + taille, family = binomial, data = data_long, na.action = na.omit)
summary(model_survie)
####Modèle mixte avec effet aléatoire sur l’individu
model_survie_lmer <- glmer(survie ~ site + taille + (1|ID_ind), family = binomial, data = data_long, na.action = na.omit)
summary(model_survie_lmer)

#####Analyse de la croissance ----
####Modèle linéaire simple
model_croissance_lm <- lm(croissance ~ site + taille, data = data_long, na.action = na.omit)
summary(model_croissance_lm)
####Modèle linéaire mixte (avec effet aléatoire individuel)
model_croissance_lmer <- lmer(croissance ~ site + taille + (1|ID_ind), data = data_long, na.action = na.omit)
summary(model_croissance_lmer)
r.squaredGLMM(model_croissance_lmer) # R2 marginal et conditionnel

# Visualisation de la croissance par site
ggplot(data_long, aes(x = site, y = croissance)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(title = "Croissance relative par site", y = "Croissance", x = "Site")

# Visualisation de l'évolution de la taille par site
ggplot(data_long, aes(x = time, y = taille, color = site)) +
  geom_line(aes(group = ID_ind), alpha = 0.3) +
  geom_point(stat="summary", fun=mean, size=3) +
  labs(title = "Évolution de la taille par site", x = "Temps", y = "Taille")

# Visualisation du taux de survie moyen par site
ggplot(data_long, aes(x = site, y = survie)) +
  geom_bar(stat="summary", fun="mean") +
  labs(title = "Taux de survie moyen par site", y = "Proportion survivante", x = "Site")


#Effet du site : certains sites ont une croissance relative plus faible.
#Effet de la taille initiale : légèrement positif.
#Effet individuel : chaque plante a sa propre croissance moyenne, ce qui justifie l’effet aléatoire.
#Variance expliquée :
#20 % par les effets fixes
#29 % si on tient compte de l’individu → les différences individuelles sont importantes.
#Modèle mixte vs linéaire simple : très similaires pour les coefficients fixes, mais le mixte permet de prendre en compte la non-indépendance des mesures répétées par individu.

data <- read.delim("Taille et phéno verif LD.csv", sep = ";", header = TRUE)

library(tidyr)
library(lme4)
library(ggplot2)
library(MuMIn)

# Passer en format long pour les tailles
data_long <- data %>%
  pivot_longer(
    cols = c(tailleS1, tailleS2, tailleS3),
    names_to = "time",
    values_to = "taille"
  ) %>%
  mutate(
    time = recode(time, tailleS1 = 1, tailleS2 = 2, tailleS3 = 3),
    taille = as.numeric(as.character(taille))
  ) %>%
  arrange(ID_ind, time)

data_long$site <- as.factor(data_long$site)
data_long$ID_ind <- as.factor(data_long$ID_ind)

# Calcul de la croissance relative entre temps consécutifs
data_long <- data_long %>%
  group_by(ID_ind) %>%
  mutate(croissance = (taille - lag(taille)) / lag(taille)) %>%
  ungroup()

# Calcul de la survie entre temps
data_long <- data_long %>%
  group_by(ID_ind) %>%
  mutate(survie = ifelse(!is.na(taille) & lead(taille, 1) == 0, 0, 1)) %>%
  ungroup()


# Observateurs ----
#### Graphique femme/homme risque d'erreur observateur ----
##Observateurs : 
data <- read.delim("Experience_des_observateurs_VF.csv", sep = ";", header = TRUE)

data <- data %>%
  mutate(score_moyen = rowMeans(across(A:E)))

library(dplyr)

data <- data %>%
  mutate(score_moyen = rowMeans(across(A:E)))
data <- data %>%
  mutate(score_moyen = rowMeans(across(A:E), na.rm = TRUE))

# Tri du plus compétent au moins compétent
data %>%
  arrange(desc(score_moyen))


# Poids de fiabilité (normalisé entre 0 et 1)
data <- data %>%
  mutate(fiabilite = score_moyen / 10,
         risque_erreur = 1 - fiabilite)

data <- data %>%
  rowwise() %>%
  mutate(ecart_type = sd(c_across(A:E))) %>%
  ungroup()

data <- data %>%
  arrange(desc(risque_erreur)) %>%
  mutate(ID.observateur = factor(ID.observateur, levels = ID.observateur))


ggplot(data, aes(x = ID.observateur, y = risque_erreur, fill = sexe)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("f" = "red", "m" = "blue")) +
  theme_minimal() +
  labs(title = "Risque d’erreur estimé par observateur",
       x = "Observateur", y = "Risque d’erreur (1 - fiabilité)")+geom_errorbar(
         aes(
           ymin = risque_erreur - (ecart_type/10),
           ymax = risque_erreur + (ecart_type/10)
         ),
         width = 0.2,
         linewidth = 0.6
       )

#


