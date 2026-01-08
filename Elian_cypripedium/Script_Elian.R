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
