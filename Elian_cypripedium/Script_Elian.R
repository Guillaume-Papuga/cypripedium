# Regression multinomiale

#modèle statistique utilisé quand la variable réponse est catégorielle avec 
#plus de deux catégories, sans ordre naturel entre elles.
#phénologie est un état discret, non ordonné, avec plusieurs modalités.

#On fait pas RDA car suggère que la variable réponse est continue, elle cherche des relation linéaire

library(nnet)

mod_pheno <- multinom(
  phenologie_verif ~ S_muscinale + S_herbacee + S_arbustive_basse + S_arbustive_haute + S_arborescente,
  data = tab_complet
)
summary(mod_pheno)


# Extraction
coef_mat <- summary(mod_pheno)$coefficients
se_mat   <- summary(mod_pheno)$standard.errors
z_mat    <- coef_mat / se_mat
p_mat    <- 2 * (1 - pnorm(abs(z_mat)))

# Fonction pour étoiles
etoiles <- function(p){
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*",
                       ifelse(p < 0.1, ".", ""))))
}

# Construction du tableau
tableau <- data.frame(
  Etat = rep(rownames(coef_mat), each = ncol(coef_mat)),
  Parametre = rep(colnames(coef_mat), times = nrow(coef_mat)),
  Coefficient = as.vector(coef_mat),
  SE = as.vector(se_mat),
  z = as.vector(z_mat),
  p_value = as.vector(p_mat)
) %>%
  mutate(
    Coefficient = round(Coefficient, 4),
    SE = round(SE, 4),
    z = round(z, 3),
    p_value = signif(p_value, 3),
    Signif = etoiles(p_value)
  )

library(knitr)
library(kableExtra)

kable(tableau, format = "html", booktabs = TRUE,
      caption = "Régression multinomiale : effets des strates de recouvrement sur la phénologie") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

#Les résultats montrent un gradient phénologique structuré par l’ouverture du milieu :
#Milieux ouverts (herbacées) → stades avancés (fleurs fanées, fruits), peu de juvéniles.
#Milieux semi‑fermés (arbustif bas) → forte probabilité de fleurs fanées et de fruits.
#Milieux très fermés (arbustif haut, arborescent) → stades précoces (non fleuri, juvénile), peu de stades avancés.
#Mousse → effet faible ou non significatif, sauf absence quasi totale de fleurs fanées dans ces micro‑habitats.
#La structure verticale du recouvrement influence donc fortement la progression phénologique, avec un cycle plus avancé dans les milieux ouverts ou semi‑ouverts, et plus précoce dans les milieux fermés.


# Sauvegarde 

# pas charger encore (à enlever ?)
```{r}
#Faire le tableau propre
verification %>%
  mutate(site=as.factor(site),
         date=date(date),
         ID_quadrat=as.factor(ID_quadrat),
         ID_ind=as.factor(ID_ind),
         observateur=as.factor(observateur)) %>%
  rowwise() %>%
  mutate(detecte=sum(juv, non_fleuri, fleur, fleurs_fanees, fruit)) -> data_imp


data_original %>%
  mutate(site=as.factor(site),
         ID_quadrat=as.factor(ID_quadrat),
         ID_ind=as.factor(ID_ind),
         phenoS1=as.factor(phenoS1),
         phenoS2=as.factor(phenoS2),
         phenoS3=as.factor(phenoS3)) -> taille_pheno

data_original %>%
  pivot_longer(cols=c(phenoS1, phenoS2, phenoS3), names_prefix = "phenoS", names_to = "session", values_to = "stade_pheno") %>%
  mutate(session=as.numeric(session)) %>%
  select(site, ID_quadrat, ID_ind, session, stade_pheno) -> pheno

data_original %>%
  pivot_longer(cols=c(tailleS1, tailleS2, tailleS3), names_prefix = "tailleS", names_to = "session", values_to = "taille") %>%
  mutate(session=as.numeric(session)) %>%
  select(site, ID_quadrat, ID_ind, session, taille) -> taille


# visualisation des changements phéno
pheno %>%
  pivot_wider(names_from=session, id_cols = "ID_ind", values_from = "stade_pheno", names_prefix = "session") -> evo_pheno

evo_pheno %>%
  filter(!ID_ind%in%"CM.Q5.10") %>%
  group_by(session1, session2, session3) %>%
  summarise(n=n()) -> recap_pheno

# jointure avec données détectins individuelles et infos pheno/taille
data_imp %>%
  select(-site) %>%
  left_join(pheno) %>%
  left_join(taille) -> data

# on importe les infos observateurs / numéros de sessions
obs_session <- readr::read_csv2("data/raw/obs_session.csv")
obs_session %>%
  dplyr::mutate(ID_quadrat = as.factor(ID_quadrat),
                observateur = as.factor(observateur),
                num_obs=as.factor(num_obs)) -> obs_session

obs_session %>%
  mutate(val=1) %>%
  pivot_wider(names_from = c(session,num_obs), values_from = val, id_cols=ID_quadrat) -> recap_sessions_obs


# on indique l'effort pour les observateurs manquants sur des quadrats, on reporte ensuite cette info aux individus
recap_sessions_obs %>%
  pivot_longer(cols=2:19,  names_to = "session_obs", values_to = "prospecte") %>%
  mutate(session=str_sub(session_obs, 1,1),
         session=as.numeric(session),
         num_obs=str_sub(session_obs, 3,6),
         num_obs=as.factor(num_obs)) %>%
  left_join(obs_session) -> effort


data %>%
  select(ID_quadrat, ID_ind) %>%
  group_by(ID_ind) %>%
  slice(1) -> ind_quadrat


ind_quadrat %>%
  left_join(effort) -> effort_inds


# histoires de détection individuelles avec les stades phénos pour multistate.
effort_inds %>%
  left_join(data) %>%
  select(session, ID_ind, num_obs, stade_pheno, prospecte) %>%
  mutate(pheno_lettres = case_when(stade_pheno%in% "j" ~ "A",
                                   stade_pheno%in% "nf" ~ "B",
                                   stade_pheno%in% "f" ~ "C",
                                   stade_pheno%in% "ff" ~ "D",
                                   stade_pheno%in% "F"~ "E"),
         etat_prosp= case_when(prospecte==1~pheno_lettres,
                               is.na(prospecte)~".")) %>%
  arrange(session, num_obs) %>%
  pivot_wider(names_from = c(session,num_obs), values_from = etat_prosp, id_cols=ID_ind) %>%
  replace(is.na(.),"0") -> hist_det

hist_long <- hist_det %>%
  pivot_longer(
    cols = -ID_ind,
    names_to = "sess_obs",
    values_to = "etat"
  ) %>%
  mutate(
    session = as.integer(str_sub(sess_obs, 1, 1)),
    obs = str_extract(sess_obs, "obs[0-9]+")
  )

histories_for_mark <- hist_long %>%
  arrange(ID_ind, obs, session) %>%
  group_by(ID_ind, obs) %>%
  summarise(
    ch = paste0(etat, collapse = ""),
    .groups = "drop"
  )

histories_clean <- histories_for_mark %>%
  mutate(ID_obs = paste(ID_ind, obs, sep = "_")) %>%
  select(ID_obs, ch)

histories_valid <- histories_clean %>%
  # garder seulement les lignes qui ont au moins un A ou B
  filter(grepl("[A-Z]", ch))

histories_valid2 <- histories_valid %>%
  filter(!grepl("obs6", ID_obs))

# 2. Définir time.intervals : longueur = T - 1 = 2

# Si tu n'as pas de covariables individuelles prêtes, on utilise juste ID et ch
histories_for_mark <- histories_valid2 %>% select(ID_obs, ch)

# vérifier les chaînes
table(nchar(histories_for_mark$ch))
unique(head(histories_for_mark$ch, 20))

# définir time.intervals (T = longueur des ch ; ici T = 3 => length = 2)
T <- unique(nchar(histories_for_mark$ch))
time.intervals <- rep(1, as.integer(T) - 1)

# Process data en Multistrata : indiquer les labels d'états présents
# RMark attend : lettres pour états et "0" pour non-observé — c'est ton cas
states <- c("A","B","C","D","E")   # adapte si tu as d'autres états
```

# Multistrata

```{r}
# processed_ms <- process.data(histories_for_mark, model = "Multistrata",
#                              time.intervals = time.intervals,
#                              strata.labels = states)
# 
# ddl_ms <- make.design.data(processed_ms)
# 
# # Exemple de modèles simples
# S.const <- list(formula = ~1)                       # survie constante
# p.bystratum <- list(formula = ~ stratum)            # p dépend de l'état observé
# Psi.bystratum <- list(formula = ~ stratum)          # transitions dépendant de l'état de départ
# 
# # Lancer un modèle multistate simple
# model_ms <- mark(processed_ms, ddl_ms,
#                  model.parameters = list(S = S.const, p = p.bystratum, Psi = Psi.bystratum))
# 
# summary(model_ms)

```

Résultat correcte pour A et B et pour D et E résultats completement aberrants

#Autres modèles

```{r}

# # 5. lancer modèles parcimonieux (Multistrata)
# mA <- mark(processed_ms, ddl_ms, model.parameters = list(
#   S   = list(formula = ~1),       # survie constante
#   p   = list(formula = ~stratum), # détection selon état
#   Psi = list(formula = ~1)        # transitions constantes
# ))
# 
# mB <- mark(processed_ms, ddl_ms, model.parameters = list(
#   S   = list(formula = ~1),
#   p   = list(formula = ~1),
#   Psi = list(formula = ~stratum)
# ))
# 
# mC <- mark(processed_ms, ddl_ms, model.parameters = list(
#   S   = list(formula = ~1),
#   p   = list(formula = ~stratum),
#   Psi = list(formula = ~stratum)
# ))
# 
# # 6. comparer
# models <- collect.models()
# model.table(models)
# 

```


#RMARK multistrate 

#Prendre le recouvrement en compte

# ```{r}
# 1. Sécuriser noms et types
histories_for_mark <- histories_for_mark %>%
  rename_with(~ as.character(.), everything())

tab_complet <- tab_complet %>%
  rename_with(~ as.character(.), everything())

# 2. Extraire ID_ind depuis ID_obs (tout avant le premier "_")
histories_tmp <- histories_for_mark %>%
  mutate(
    ID_ind = sub("_.*$", "", as.character(ID_obs)),
    ID_ind = str_trim(ID_ind)
  )

# 3. Préparer tab_complet : ID_ind + covariables à joindre
tab_tmp <- tab_complet %>%
  mutate(
    ID_ind = as.character(ID_ind),
    ID_ind = str_trim(ID_ind)
  ) %>%
  select(
    ID_ind,
    recouvrement_PC1,
    site,
    session,
    taille,
    quadrat
  )

# 4. Jointure des covariables
histories_with_recov <- histories_tmp %>%
  left_join(tab_tmp, by = "ID_ind")

# 5. Conserver la structure d'origine + nouvelles colonnes
histories_for_mark_final <- histories_with_recov %>%
  select(
    all_of(names(histories_for_mark)),
    recouvrement_PC1,
    site,
    session,
    taille,
    quadrat
  )

# 6. Contrôles rapides
n_total <- nrow(histories_for_mark_final)
n_missing <- sum(is.na(histories_for_mark_final$recouvrement_PC1))
message(
  "Total lignes : ", n_total,
  " — lignes sans recouvrement_PC1 après jointure : ", n_missing
)

# 7. Préparation finale : ID_ind + centrage de la covariable continue
histories_for_mark_final <- histories_for_mark_final %>%
  mutate(
    ID_ind = sub("_.*$", "", as.character(ID_obs)),
    recouvrement_PC1 = as.numeric(recouvrement_PC1),
    taille = as.numeric(taille),
    recov_c = recouvrement_PC1 - mean(recouvrement_PC1, na.rm = TRUE)
  )

# 8. Vérifier longueurs des chaînes ch
len_tab <- table(nchar(histories_for_mark_final$ch))
if (length(len_tab) != 1)
  stop("Les chaînes ch n'ont pas toutes la même longueur")

T <- as.integer(names(len_tab)[1])
time.intervals <- rep(1, T - 1)

# 9. Définir états présents (exclure '0' et '.')
states <- sort(
  setdiff(
    unique(
      unlist(
        strsplit(
          paste(histories_for_mark_final$ch, collapse = ""),
          ""
        )
      )
    ),
    c("0", ".")
  )
)
message("États détectés : ", paste(states, collapse = ", "))

# 10. Table pour process.data : ID, ch + covariables individuelles
histories_rmark <- histories_for_mark_final %>%
  transmute(
    ID = ID_obs,
    ch = ch,
    recov_c,
    site,
    session,
    taille,
    quadrat
  )

# 11. process.data Multistrata
processed_ms <- process.data(
  histories_rmark,
  model = "Multistrata",
  time.intervals = time.intervals,
  strata.labels = states
)

# 12. design data
ddl_ms <- make.design.data(processed_ms)

# Modèle 1 : base parcimonieuse (S constant, p selon état, Psi constant)
m1 <- mark(
  processed_ms,
  ddl_ms,
  model.parameters = list(
    S   = list(formula = ~1),
    p   = list(formula = ~stratum),
    Psi = list(formula = ~1)
  )
)

# Modèle 2 : p constant, Psi selon état (test de structure)
m2 <- mark(
  processed_ms,
  ddl_ms,
  model.parameters = list(
    S   = list(formula = ~1),
    p   = list(formula = ~1),
    Psi = list(formula = ~stratum)
  )
)

# Modèle 3 : modèle complet sans covariable
m3 <- mark(
  processed_ms,
  ddl_ms,
  model.parameters = list(
    S   = list(formula = ~1),
    p   = list(formula = ~stratum),
    Psi = list(formula = ~stratum)
  )
)

# Modèle 4 : inclure recouvrement_PC1 sur S
m4 <- mark(
  processed_ms,
  ddl_ms,
  model.parameters = list(
    S   = list(formula = ~recov_c),
    p   = list(formula = ~stratum),
    Psi = list(formula = ~stratum)
  )
)

# Modèle 5
m5 <- mark(
  processed_ms,
  ddl_ms,
  model.parameters = list(
    S   = list(formula = ~1),
    p   = list(formula = ~stratum + recov_c),
    Psi = list(formula = ~stratum)
  )
)

# Rassembler et comparer
models_list <- list(m1 = m1, m2 = m2, m3 = m3, m4 = m4, m5 = m5)
model.table(models_list)

# Modèle avec groupes (site)
processed_ms <- process.data(
  histories_rmark,
  model = "Multistrata",
  time.intervals = time.intervals,
  strata.labels = states,
  groups = "site"
)

ddl_ms <- make.design.data(processed_ms)

m6 <- mark(
  processed_ms,
  ddl_ms,
  model.parameters = list(
    S   = list(formula = ~group),
    p   = list(formula = ~stratum + recov_c),
    Psi = list(formula = ~stratum)
  )
)

models_list <- list(m6 = m6)
model.table(models_list)
