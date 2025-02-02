
# Install revealjs package if not installed
install.packages("revealjs")

# Load packages

library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)


# Load relevant files
eruptions <- read_csv("tidytuesday:2020:2020-05-12:volcano/eruptions.csv")
events <- read_csv("tidytuesday:2020:2020-05-12:volcano/events.csv")
sulfur <- read_csv("tidytuesday:2020:2020-05-12:volcano/sulfur.csv")
tree_ring <- read_csv("tidytuesday:2020:2020-05-12:volcano/tree_rings.csv")
volcanos <- read_csv("tidytuesday:2020:2020-05-12:volcano/volcano.csv")


# Check the datasets
View(eruptions) # volcano name & vei & coordinates
View(events) # volcano name & volcano type -> explosion type might effect weather/temp during that time
View(sulfur) # link sulfur and eruptions
View(tree_ring) # plot the correlation between eruption & n_tree + europe_temp_index



#Use the function: facet_wrap(), facet_grid()

######## Ideas #########

# 1. Did volcanic activity in Europe after the 1800s affect the n_tree or Europe temperature index in the corresponding year?
# 2. The correlation between VEI magnitude and frequency of eruptions with the Europe temperature index in the same year.
# 3. Mapping volcanic eruptions after the 1800s, colored by VEI magnitude.



###### Clean dataset #######


# 1. Check the correlation between the sulfur and tree_ring datasets. Since the sulfur data is in decimal years and the tree_ring data is yearly, the sulfur values should first be averaged by year to enable proper comparison.

sulfur_year <- sulfur %>%
  filter(year >= 500 & year <= 706) %>% 
  mutate(year = floor(year)) %>%          # Round down the year 
  group_by(year) %>%                     # Group by year
  summarise(mean_neem = mean(neem), mean_wdc = mean(wdc))

sulfur_tree_ring <- sulfur_year %>%
  inner_join(tree_ring, by = "year") 

View(sulfur_tree_ring)


# 2. The tree_ring data can be converted to a long format using the pivot_longer() function, merging n_tree and europe_temp_index into a single measurement variable.

measurement <- sulfur_tree_ring %>%
pivot_longer(cols = c(n_tree, europe_temp_index, mean_neem, mean_wdc), 
             names_to = "measurement", values_to = "value")

View(measurement)


