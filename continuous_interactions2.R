# ==============================================================================
# Graphique d'une interaction entre deux variables continues
# Plot showcasing an interaction between two continuous variables
# ==============================================================================



# Le second graphique que nous allons créer sera en deux dimensions
# et présentera l'interaction en montrant les courbes prédites
# par l'équation du modèle pour le 1er et le 3e quartile de la 
# covariable x2. Nous utiliserons la librarie ggplot2.




# 1. Importer le jeu de données et charger les libraries
#    Import the dataset and load libraries
# ==============================================================================

# We execute the simulate_data.R script by sourcing the raw file's URL from the LMM-GLMM repository
# Nous exécutons le script simulate_data.R en sourçant l'URL du fichier brut dans le répertoire LMM-GLMM
source("https://raw.githubusercontent.com/GRECA-UQAM/LMM-GLMM/main/simulate_data.R")

# Télécharger les librairies nécessaires pour produire le graphique
# Load the libraries needed to produce the plot
packages <- c("lme4", "plotly", "htmlwidgets")
lapply(packages, require, character.only = TRUE)

# Transformer la variable "environment" en facteur
# Transform the "environment" variable as a factor
data$environnement <- as.factor(data$environnement)

# ==============================================================================




# 2. Produire un modèle mixte simple
#    Produce a simple mixed model
# ==============================================================================

model <- lmer(y ~ x1 + x2 + x1:x2 + environnement + (1|id), data = data)

# ==============================================================================




# 3. Générer les matrices d'effets modèle pour faire le graphique
#    Generate effects model matrixes for plotting
# ==============================================================================

# Décomposer la covariable x2 en 3 variables discrètes basées sur les quantiles (low, medium, high)
# Decompose the x2 covariate in 3 discreete categories based on quantiles (low, medium, high)
x2_quantiles <- quantile(data$x2)


# Créer un nouveau jeu de données
# Create a new dataset
newdat <- expand.grid(x1 = seq(min(data$x1), max(data$x1), length = 100),
                      x2 = c(as.numeric(x2_quantiles[2]), 
                             as.numeric(x2_quantiles[3]), 
                             as.numeric(x2_quantiles[4])
                             )
                     )

# Créer la matrice du modèle
# Create the model matrix
mm <- model.matrix(~ x1 + x2 + x1:x2, newdat)

# Calculer les valeurs prédites à partir des coefficients du modèle
# Calculate predicted values from the model coefficients
y <- mm%*%fixef(model)[c(1, 2, 3, 7)]

# Intervalle de confiance et de prédiction
# Confidence and prediction intervals
x1_p <- diag(mm %*% tcrossprod(vcov(model)[c(1, 2, 3, 7), c(1, 2, 3, 7)], mm))
x1_t <- x1_p + 
        VarCorr(model)$id[1] + # variance de id - id variance
        attr(VarCorr(model), "sc") # variance résiduelle - residual variance

# Compute final dataframe
newdat <- data.frame(
                     x1 = newdat$x1,
                     x2 = newdat$x2,
                     y = y,
                     plo = y - 1.96 * sqrt(x1_p),
                     phi = y + 1.96 * sqrt(x1_p),
                     tlo = y - 1.96 * sqrt(x1_t),
                     thi = y + 1.96 * sqrt(x1_t)
                    )

newdat$quantile <- rep(c("25% quantile", "50% quantile", "75% quantile"), each = 100)

# ==============================================================================




# 4. Créer le graphique
#    Create the graph
# ==============================================================================

# Palette de couleurs pour les gens avec déficiences visuelles
# Color palette for visually impared people
cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# use scale_fill_manuel(value = cbp2) and scale_colour_manual(value = cbp2) to call palette


# Le graphique
# The graph
plot <- ggplot(data = newdat,
            aes(x = x1, y = y, color = quantile, fill = quantile)) +
        scale_color_viridis(discrete = TRUE, option = "D") +
        scale_fill_viridis(discrete = TRUE) +
        geom_point(data = data,
                   aes(x = x1, y = y),
                   shape = 20, alpha = 0.05, color = "gray28", 
                   inherit.aes = FALSE) + # or point shape 20 
        geom_line(size = 1.5) +
      # geom_ribbon(aes(ymin = plo, ymax = phi), alpha = 0.3, colour = NA) +
        geom_line(data = newdat,
                  aes(x = x1, y = plo),
                  linetype = "dashed", size = 1, colour = "black") +
        geom_line(data = newdat,
                  aes(x = x1, y = phi),
                  linetype = "dashed", size = 1, colour = "black") +
       #scale_y_continuous(breaks = seq(), 
       #                   expand = c(0, 0),
       #                   limits = c()) +
       #scale_x_continuous(breaks = seq(), 
       #                   expand = c(0, 0),
       #                   limits = c()) +
        ylab("y\n") +
        labs(color = "x2 quantiles") +
        xlab("\nx1") +
        theme(axis.text.x = element_text(face = "plain", size = 14,
                                   color = "black"), # axis tick numbers size
              axis.text.y = element_text(face = "plain", size = 14,
                                   color = "black"),
              axis.ticks.length = unit(.15, "cm"), # axis ticks lenght
              axis.ticks = element_line(size = 0.90, color = "black"), # axis ticks width
              axis.title = element_text(size = 15, face = "plain"), # axis titles size
              axis.line = element_line(size = 0.95),
              plot.margin = unit(c(2, 0.5, 2, 0.5), "lines"),
              panel.grid = element_blank(),
              panel.background = element_blank())


















library(data.table)
library(lme4)
library(arm)
library(minqa)
library(lattice)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(plotly)


selection_data <- fread("01_selection_xp-data.csv",
                        select = c("timestamp.x", "mirrors_id", "match_id", "map_name",
                                   "sacrificed_count","prop_bloodpoints", "weight",
                                   "Zspeed", "Zprox_mid_guard", "Zspace_covered_rate",
                                   "speed", "proportion_prox_mid",
                                   "space_covered_rate", "xp_total",
                                   "xp_map", "xp_both"))

# Add an observation-level random effect (OLRE) (in case of overdispersion)
selection_data$obs <- 1:nrow(selection_data)

# Character variables to factor variables
char_as_factor <- names(selection_data)[sapply(selection_data, is.character)] # extract columns that are characters
selection_data[, (char_as_factor) := lapply(.SD, as.factor), .SDcols = char_as_factor] # columns as factors

# Standardize experience variables
selection_data[, c("Zxp_total", "Zxp_map", "Zxp_both") :=
            lapply(.SD, scale), .SDcols = c(14:16)]

# Load the model
load("02_model1.rda")





# 2. Generate model matrixes for plotting ------------------------------
# ----------------------------------------------------------------------
# Decompose each behavioural variable by 3 discreete experience categories (low, medium, high)
# categories should be based on quantiles
xp_quantiles <- quantile(selection_data$Zxp_total)


# Z speed
speed_newdat <- expand.grid(speed_x = seq(min(selection_data$Zspeed), max(selection_data$Zspeed), length = 100),
Zxp_total = c(as.numeric(xp_quantiles[2]), as.numeric(xp_quantiles[3]), as.numeric(xp_quantiles[4])))

speed_mm <- model.matrix(~ speed_x + Zxp_total + speed_x:Zxp_total, speed_newdat)
speed_y <- speed_mm%*%fixef(model1)[c(1, 3, 2, 6)] # I'm extracting only the coefficients from the original model related to the model matrix

speed_pvar1 <- diag(speed_mm %*% tcrossprod(vcov(model1)[c(1, 3, 2, 6), c(1, 3, 2, 6)], speed_mm)) # 4x4 matrix of intercept and Zspeed
speed_tvar1 <- speed_pvar1 + 
VarCorr(model1)$obs[1] + 
VarCorr(model1)$mirrors_id[1] + 
VarCorr(model1)$mirrors_id[2] + 
VarCorr(model1)$mirrors_id[3] + 
VarCorr(model1)$mirrors_id[4] + 
VarCorr(model1)$map_name[1] +
VarCorr(model1)$map_name[2] +
VarCorr(model1)$map_name[3] +
VarCorr(model1)$map_name[4]

speed_newdat <- data.frame(
  speed_x = speed_newdat$speed_x,
  Zxp_total = speed_newdat$Zxp_total,
  speed_y = invlogit(speed_y),
  speed_plo = invlogit(speed_y - 1.96 * sqrt(speed_pvar1))
  , speed_phi = invlogit(speed_y + 1.96 * sqrt(speed_pvar1))
  , speed_tlo = invlogit(speed_y - 1.96 * sqrt(speed_tvar1))
  , speed_thi = invlogit(speed_y + 1.96 * sqrt(speed_tvar1))
)

speed_newdat$code <- rep(c("25% quantile", "50% quantile", "75% quantile"), each = 100) # a coding of the level of the other covariate



# Z space covered
space_newdat <- expand.grid(space_x = seq(min(selection_data$Zspace_covered_rate), max(selection_data$Zspace_covered_rate), length = 100),
Zxp_total = c(as.numeric(xp_quantiles[2]), as.numeric(xp_quantiles[3]), as.numeric(xp_quantiles[4])))

space_mm <- model.matrix(~ space_x + Zxp_total + space_x:Zxp_total, space_newdat)
space_y <- space_mm%*%fixef(model1)[c(1, 5, 2, 8)] # I'm extracting only the coefficients from the original model related to the model matrix

space_pvar1 <- diag(space_mm %*% tcrossprod(vcov(model1)[c(1, 5, 2, 8), c(1, 5, 2, 8)], space_mm)) # 4x4 matrix
space_tvar1 <- space_pvar1 + 
VarCorr(model1)$obs[1] + 
VarCorr(model1)$mirrors_id[1] + 
VarCorr(model1)$mirrors_id[2] + 
VarCorr(model1)$mirrors_id[3] + 
VarCorr(model1)$mirrors_id[4] + 
VarCorr(model1)$map_name[1] +
VarCorr(model1)$map_name[2] +
VarCorr(model1)$map_name[3] +
VarCorr(model1)$map_name[4]

space_newdat <- data.frame(
  space_x = space_newdat$space_x,
  Zxp_total = space_newdat$Zxp_total,
  space_y = invlogit(space_y),
  space_plo = invlogit(space_y - 1.96 * sqrt(space_pvar1))
  , space_phi = invlogit(space_y + 1.96 * sqrt(space_pvar1))
  , space_tlo = invlogit(space_y - 1.96 * sqrt(space_tvar1))
  , space_thi = invlogit(space_y + 1.96 * sqrt(space_tvar1))
)

space_newdat$code <- rep(c("25% quantile", "50% quantile", "75% quantile"), each = 100) # a coding of the level of the other covariate



# Z Time spent guarding
guard_newdat <- expand.grid(guard_x = seq(min(selection_data$Zprox_mid_guard), max(selection_data$Zprox_mid_guard), length = 100),
Zxp_total = c(as.numeric(xp_quantiles[2]), as.numeric(xp_quantiles[3]), as.numeric(xp_quantiles[4])))

guard_mm <- model.matrix(~guard_x + Zxp_total + guard_x:Zxp_total, guard_newdat)
guard_y <- guard_mm%*%fixef(model1)[c(1, 4, 2, 7)] # I'm extracting only Z space covered and the intercept

guard_pvar1 <- diag(guard_mm %*% tcrossprod(vcov(model1)[c(1, 4, 2, 7), c(1, 4, 2, 7)], guard_mm)) # 4x4 matrix 
guard_tvar1 <- guard_pvar1 + 
VarCorr(model1)$obs[1] + 
VarCorr(model1)$mirrors_id[1] + 
VarCorr(model1)$mirrors_id[2] + 
VarCorr(model1)$mirrors_id[3] + 
VarCorr(model1)$mirrors_id[4] + 
VarCorr(model1)$map_name[1] +
VarCorr(model1)$map_name[2] +
VarCorr(model1)$map_name[3] +
VarCorr(model1)$map_name[4]

guard_newdat <- data.frame(
  guard_x = guard_newdat$guard_x,
  Zxp_total = guard_newdat$Zxp_total,
  guard_y = invlogit(guard_y),
  guard_plo = invlogit(guard_y - 1.96 * sqrt(guard_pvar1))
  , guard_phi = invlogit(guard_y + 1.96 * sqrt(guard_pvar1))
  , guard_tlo = invlogit(guard_y - 1.96 * sqrt(guard_tvar1))
  , guard_thi = invlogit(guard_y + 1.96 * sqrt(guard_tvar1))
)

guard_newdat$code <- rep(c("25% quantile", "50% quantile", "75% quantile"), each = 100) # a coding of the level of the other covariate
# ----------------------------------------------------------------------





# 3. Generate plots ----------------------------------------------------
# ----------------------------------------------------------------------
# Color palette for visually impared people
cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# use scale_fill_manuel(value = cbp2) and scale_colour_manual(value = cbp2) to call palette


# Z speed plot
speed_plot <- ggplot(data = speed_newdat,
            aes(x = speed_x, y = speed_y, color = code, fill = code)) +
scale_color_viridis(discrete = TRUE, option = "D") +
scale_fill_viridis(discrete = TRUE) +
  geom_point(data = selection_data,
             aes(x = Zspeed, y = prop_bloodpoints),
             shape = 20, alpha = 0.05, color = "gray28", inherit.aes = FALSE) + # or point shape 20 
  geom_line(size = 1.5) +
#geom_ribbon(aes(ymin = speed_plo, ymax = speed_phi), alpha = 0.3, colour = NA) +
geom_line(data = speed_newdat,
            aes(x = speed_x, y = speed_plo),
            linetype = "dashed", size = 1, colour = "black") +
geom_line(data = speed_newdat,
            aes(x = speed_x, y = speed_phi),
            linetype = "dashed", size = 1, colour = "black") +
scale_y_continuous(breaks = seq(0, 1, .25), expand = c(0, 0),
                     limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(-8, 6, 2), expand = c(0, 0),
                     limits = c(-8, 6)) +
  # ylab("Probability of gaining bloodpoints\n") +
  ylab("") +
  labs(color = "Cumulative XP.") +
  xlab("\nAverage speed") +
  theme(axis.text.x = element_text(face = "plain", size = 14,
                                   color = "black"), # axis tick numbers size
        axis.text.y = element_text(face = "plain", size = 14,
                                   color = "black"),
        axis.ticks.length = unit(.15, "cm"), # axis ticks lenght
        axis.ticks = element_line(size = 0.90, color = "black"), # axis ticks width
        axis.title = element_text(size = 15, face = "plain"), # axis titles size
        axis.line = element_line(size = 0.95),
        plot.margin = unit(c(2, 0.5, 2, 0.5), "lines"),
        panel.grid = element_blank(),
        panel.background = element_blank())