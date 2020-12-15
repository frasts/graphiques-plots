# ==============================================================================
# Graphique 3D d'une interaction entre deux variables continues
# 3D plot showcasing an interaction between two continuous variables
# ==============================================================================


# Le premier type de graphique que nous allons créer est une surface 3D 
# que nous ferons à l'aide de la librarie plotly. C'est l'une des façons 
# de représenter l'interaction entre deux variables continues.
# ------------------------------------------------------------------------------




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



# 2. Calculer les valeurs de l'axe z (3D)
#    Calculate values of the z axis (3D)
# ==============================================================================

# a) Éxécuter un modèle mixte simple
#    Execute a simple mixed model
model <- lmer(y ~ x1 + x2 + x1:x2 + environnement + (1|id), data = data)

# b) Extraire les vecteurs (n=50) contenant l'étendue des variables x1 et x2 
#    Extract vectors (n=50) within the range of the x1 and x2 variables 
x1 <- seq(min(data$x1), max(data$x1), length = 50)
x2 <- seq(min(data$x2), max(data$x2), length = 50)

# c) Nous calculons les valeurs de z à partir des paramètres du modèle
#    On inclus tous les paramètres de l'équation du modèle
#    Calculate the z axis values from the model parameters
#    You should include all the model parameters in the equation 
z1 <- outer(x1, x2, FUN = function(x, y) { fixef(model)[1] + 
                                          (fixef(model)[2] * x) +
                                          (fixef(model)[3] * y) + 
                                          (fixef(model)[5] * x * y)
                                          })

# ==============================================================================



# 3. On produit la surface 3D
#    Produre the 3D surface plot
# ==============================================================================

# a) mise en forme des valeurs sur les axes
#    axis tick font
tickfont <- list(
  ticklen = 3,
  tickwidth = 3,
  size = 14,
  color =  "black" # color of numbers
)

# b) mise en forme du titre des axes
#    axis title font
titlefont <- list(
  size = 15,
  color = "black"
)


# c) Produire les paramètres de fond
#    Compute font parameters
# axe z - z axis
xz <- list(
    tickfont = tickfont,
    ticks = "outside",
    tickcolor = "black",
    title = "y", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# x axis
xx <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    title = "x2", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par.
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# y axis
xy <- list(
    tickfont = tickfont, # tick par.
    ticks = "outside",
    tickangle = 3,
    title = "x1", # titles par.
    titlefont = titlefont,
    backgroundcolor = "white", # cube par.
    gridcolor = "darkgray",
    gridwidth = 3,
    showbackground = TRUE,
    zerolinecolor = "darkgray", # lines at 0
    linecolor = "black", # black axis line
    linewidth = 4
  )

# d) On créé le graphique. On spécifie les paramètres de fond en utlisant layout(scene = list(...)) 
#    Create the plot. Specify axis font parameters using layout(scene = list(...))
surface_plot <- plot_ly() %>%
                      add_trace(x = ~x2, 
                                y = ~x1, 
                                z = ~y, 
                                data = data, 
                                type = "scatter3d",
                                mode = "markers",
                                marker = list(size = 2, 
                                              color = "#571A44",
                                              opacity = 0.3),
                                showlegend = FALSE) %>% 
                                # Dans la partie ci-haut, on a créé le nuage de points 3D
                                # In the above part, we added the 3D scatterplot
                      add_surface(z = z1,
                                  x = x2, 
                                  y = x1,
                                  opacity = 0.8) %>% 
                                  # Maintenant, nous avons ajouté la surface 3D
                                  # Now we added the 3D surface
                      layout(scene = list(zaxis = xz, 
                                          xaxis = xx, 
                                          yaxis = xy,
                                          showscale = FALSE, 
                                          showlegend = FALSE)) %>% 
                                  # Finalement, on a spécifié les paramètres graphiques
                                  # Here, we specify layout parameters
                      hide_colorbar()
# ==============================================================================


# Si vous avez installé la librarie htmlwidgets, le graphique 
# apparaitra dans la fenètre graphique lorsque vous l'exécutez. 
# Sinon, cela ouvrira un url avec le graphique que vous pourrez
# modifier à votre guise. Vous pourrez ensuite l'enregistrer 
# en format .png.

# If you have htmlwidgets installed, the plot should appear in
# the plot window when you executed it. If not, it should open 
# an url with the plot. You can change it in there and then save
# it in your computer as a .png file.