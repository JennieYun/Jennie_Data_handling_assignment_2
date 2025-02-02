
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(ggpubr)

eruptions <- read_csv("tidytuesday:2020:2020-05-12:volcano/eruptions.csv")
events <- read_csv("tidytuesday:2020:2020-05-12:volcano/events.csv")
sulfur <- read_csv("tidytuesday:2020:2020-05-12:volcano/sulfur.csv")
tree_ring <- read_csv("tidytuesday:2020:2020-05-12:volcano/tree_rings.csv")
volcanos <- read_csv("tidytuesday:2020:2020-05-12:volcano/volcano.csv")


# Scatter plot between the number of trees (n_tree) and Europe Temperature Index

ggplot(tree_ring, aes(x = n_tree, y = europe_temp_index)) + 
  # Add scatter points with grey color and defined size and stroke
  geom_point(size = 1, stroke = 0.8, color = "grey40") + 

  # Add a linear regression line (lm method) in red
  geom_smooth(method = "lm", color = "red") + 
  
  # Add correlation coefficient to the plot
  stat_cor(aes(label = ..r.label..), method = "pearson", color = "black", size = 4)

  # Set the title of the plot
  ggtitle("Correlation Between Number of Trees and Europe Temperature Index") +
  
  # Apply minimal theme for clean appearance
  theme_minimal() +
  
  # Label the x-axis as "Number of Trees"
  xlab("n_tree") + 
  
  # Label the y-axis as "Europe Temperature Index"
  ylab("Europe Temperature Index")

