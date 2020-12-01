# ==============================================================================
# Produce plots showcasing a two-way continuous interaction from a mixed model
# ==============================================================================



# 1. Import the dataset and load libraries
# ==============================================================================
# Source the dataset from the LMM-GLMM github repository (GRECA-UQAM)
source("https://raw.githubusercontent.com/GRECA-UQAM/LMM-GLMM/main/simulate_data.R")

# Load the libraries needed to produce the plot
packages <- c("lme4", "plotly", "htmlwidgets")
lapply(packages, require, character.only = TRUE) # this loads all the packages at once

# Make sure that characters are coded as factors in the dataset
char_as_factor <- names(data)[sapply(data, is.character)] # extract columns that are characters
data[, (char_as_factor) := lapply(.SD, as.factor), .SDcols = char_as_factor] # columns as factors
# ==============================================================================



# 2. Compute a simple mixed model to extract the parameters and produce the plot
# ==============================================================================
model <- lmer(y ~ x1 + x2 + x1:x2 + environnement + (1|id), data = data)
# ==============================================================================



# 3. 3d surface plot using plotly
# ==============================================================================
# tick font
tickfont <- list(
  ticklen = 3,
  tickwidth = 3,
  size = 14,
  color =  "black" # color of numbers
)

# title font
titlefont <- list(
  size = 15,
  color = "black"
)

# Calculate z-values from model equation
x1 <- seq(min(data$x1), max(data$x1), length = 50) # select values for the surface
x2 <- seq(min(data$x2), max(data$x2), length = 50)

z1 = outer(x1, x2, FUN = function(x, y) {fixef(model)[1] + 
                                         (fixef(model)[2] * x) +
                                         (fixef(model)[3] * y) + 
                                         (x * y * fixef(model)[5])
                                          }) # this calculates the z axis

# Compute font parameters
# z axis parameters
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

# x axis parameters
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

# y axis parameters
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

# Create the plot. Specify axis parameters (the ones we created above) using layout(scene = list(...))
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
                                showlegend = FALSE) %>% # In this part, we added the scatterplot
                      add_surface(z = z1,
                                  x = x2, 
                                  y = x1,
                                  opacity = 0.8) %>% # Here, we add the surface
                      layout(scene = list(zaxis = xz, 
                                          xaxis = xx, 
                                          yaxis = xy,
                                          showscale = FALSE, 
                                          showlegend = FALSE)) %>% # Here, we specify layout parameters
                      hide_colorbar() # Hide the color gradient legend

# If you have htmlwidgets installed, the plot should appear in the plot window (in Rstudio)
# If not, running "surface_plot" in your console will open an url with the plot. You can change it in there and then save it in your computer as a .png file
