
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)

eruptions <- read_csv("tidytuesday:2020:2020-05-12:volcano/eruptions.csv")
events <- read_csv("tidytuesday:2020:2020-05-12:volcano/events.csv")
sulfur <- read_csv("tidytuesday:2020:2020-05-12:volcano/sulfur.csv")
tree_ring <- read_csv("tidytuesday:2020:2020-05-12:volcano/tree_rings.csv")
volcanos <- read_csv("tidytuesday:2020:2020-05-12:volcano/volcano.csv")


# Basic map of eruptions

ggplot(data = eruptions %>% filter(!is.na(vei)), aes(x = longitude, y = latitude, color = vei)) +
  borders("world", colour = "gray80", fill = "gray95") +  # Add a world map with gray borders and light background
  geom_point(size = 2, alpha = 0.5) +  # Plot eruption points after creating the border
  scale_color_gradient(low = "blue", high = "red") +  # Color scale: Blue for low VEI, red for high VEI
  theme_minimal() +  # Minimalist theme for a clean look
  labs(title = "Map of Volcano Eruptions with VEI Scale",  # Title of the plot
       x = "Longitude",  # Label for the x-axis
       y = "Latitude",  # Label for the y-axis
       color = "VEI") +  # Label for the color scale
  theme(plot.title = element_text(size = 30, face = "bold")) # Increase title size for better visibility


# Map of eruptions 1800s onwards

ggplot(data = eruptions %>% 
         filter(!is.na(vei), start_year >= 1800), aes(x = longitude, y = latitude, color = vei)) +
  borders("world", colour = "gray80", fill = "gray95") +  # Add a world map with gray borders and light background
  geom_point(size = 2, alpha = 0.5) +  # Plot eruption points after creating the border
  scale_color_gradient(low = "blue", high = "red") +  # Color scale: Blue for low VEI, red for high VEI
  theme_minimal() +  # Minimalist theme for a clean look
  labs(title = "Map of Volcano Eruptions with VEI Scale",  # Title of the plot
       x = "Longitude",  # Label for the x-axis
       y = "Latitude",  # Label for the y-axis
       color = "VEI") +  # Label for the color scale
  theme(plot.title = element_text(size = 30, face = "bold")) # Increase title size for better visibility


