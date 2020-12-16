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
packages <- c("lme4", "viridis")
lapply(packages, require, character.only = TRUE)

# ==============================================================================




# 2. Produire un modèle mixte simple
#    Produce a simple mixed model
# ==============================================================================

model <- lmer(y ~ x1 + x2 + x1:x2 + (1|id), data = data)

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
y <- mm%*%fixef(model)[c(1, 2, 3)]

# Intervalle de confiance et de prédiction
# Confidence and prediction intervals
x1_p <- diag(mm %*% tcrossprod(vcov(model)[c(1, 2, 3), c(1, 2, 3)], mm))
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
# ==============================================================================
