
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)

eruptions <- read_csv("tidytuesday:2020:2020-05-12:volcano/eruptions.csv")
events <- read_csv("tidytuesday:2020:2020-05-12:volcano/events.csv")
sulfur <- read_csv("tidytuesday:2020:2020-05-12:volcano/sulfur.csv")
tree_ring <- read_csv("tidytuesday:2020:2020-05-12:volcano/tree_rings.csv")
volcanos <- read_csv("tidytuesday:2020:2020-05-12:volcano/volcano.csv")



# Check eruption events that could affect temperature indicators
str(events$event_type)
weather_affect_event <- unique(events$event_type)
print(weather_affect_event)

# Set latitude and longitude range for the European region
european_volcanoes <- eruptions %>%
  filter(latitude >= 35 & latitude <= 71,   
         longitude >= -31 & longitude <= 40) 

# Extract volcano numbers in the European region
european_volcano_numbers <- european_volcanoes$volcano_number

# Extract event_type and eruptions after 1900 for European volcano numbers
events_european <- events %>%
  filter(volcano_number %in% european_volcano_numbers) %>%
  filter(event_type %in% c("Explosion", "Ash", "Eruption cloud", "Ash Plume", "Tephra", "Volcanic smoke")) #%>%
# filter(eruption_start_year >= 1900)  # Filter for eruptions after 1900

# Check the results
View(events_european)

# Extract eruption years from events_european based on event_date_year
eruption_years <- events_european %>%
  select(volcano_number, eruption_start_year)

# Extract European temperature indices for corresponding years from tree_ring
european_temp_impact <- tree_ring %>%
  filter(year %in% eruption_years$eruption_start_year) %>%
  select(year, europe_temp_index)

# Check temperature indices during eruption years
View(european_temp_impact)

# Average temperature index during eruption years
avg_temp_impact_eruption <- mean(european_temp_impact$europe_temp_index, na.rm = TRUE)

# Average temperature index during general years (overall average of temperature indices)
european_temp_impact_all <- tree_ring #%>%
# filter(year >= 1900)

avg_temp_impact_all <- mean(european_temp_impact_all$europe_temp_index, na.rm = TRUE)

# Compare average values
avg_temp_impact_eruption
avg_temp_impact_all


###### Calculate the average temperature change by VEI values and compare volcanic activity impacts

# 1. Extract eruption data containing VEI values from all regions
# Use the eruptions dataset for VEI data extraction
events_vei <- eruptions  # Using the eruptions dataset

# 2. Calculate temperature changes in tree_ring data (current year - previous year)
tree_ring <- tree_ring %>%
  arrange(year) %>%
  mutate(temp_change = europe_temp_index - lag(europe_temp_index))  # Calculate temperature change

# 3. Combine tree_ring and eruptions datasets (based on year and eruption_start_year)
tree_ring_vei <- tree_ring %>%
  inner_join(events_vei, by = c("year" = "start_year")) %>%
  filter(!is.na(vei))  # Include only non-NA VEI values

# Check the combined dataset
View(tree_ring_vei)

# 4. Calculate temperature change by VEI values
# Classify into VEI categories
tree_ring_vei <- tree_ring_vei %>%
  mutate(vei_category = case_when(
    vei >= 5 ~ "VEI >= 5",
    vei >= 3 ~ "VEI 3-4",
    vei >= 1 ~ "VEI 1-2",
    TRUE ~ "No VEI"  # Handle cases with no VEI values
  ))

# 5. Calculate the average temperature change for each VEI category
vei_temp_change <- tree_ring_vei %>%
  group_by(vei_category) %>%
  summarise(avg_temp_change = mean(temp_change, na.rm = TRUE))

# 6. Visualize temperature changes by VEI category
library(ggplot2)
ggplot(vei_temp_change, aes(x = vei_category, y = avg_temp_change, fill = vei_category)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Average Temperature Change by VEI Category", x = "VEI Category", y = "Average Temperature Change (°C)") +
  theme_minimal()


################################################################################

# 1. Filter data from 1800 onward
tree_ring_1800 <- tree_ring %>%
  filter(year >= 1800)

events_vei_1800 <- eruptions %>%
  filter(start_year >= 1800)

# 2. Expand volcanic activity periods to each year (handle only valid end_year values)
events_vei_long <- events_vei_1800 %>%
  filter(!is.na(end_year)) %>%  # Exclude rows with NA in end_year
  mutate(years_active = map2(start_year, end_year, ~seq(.x, .y))) %>%
  unnest(cols = years_active) %>%
  rename(year = years_active) %>%
  select(year, vei)  # Select only necessary columns

# 3. Join tree_ring data with volcanic activity data (inner_join)
tree_ring_vei_1800 <- tree_ring_1800 %>%
  inner_join(events_vei_long, by = "year")

# 4. Calculate temperature changes based on VEI
tree_ring_vei_1800 <- tree_ring_vei_1800 %>%
  arrange(year) %>%
  mutate(temp_change = europe_temp_index - lag(europe_temp_index))

# Categorize VEI values and compute temperature changes per category
tree_ring_vei_1800 <- tree_ring_vei_1800 %>%
  mutate(vei_category = case_when(
    vei >= 5 ~ "VEI >= 5",
    vei >= 3 ~ "VEI 3-4",
    vei >= 1 ~ "VEI 1-2",
    TRUE ~ "No VEI"
  ))

# Remove duplicate observations by averaging temp_change per year and VEI category
tree_ring_vei_1800_avg <- tree_ring_vei_1800 %>%
  group_by(year, vei_category) %>%
  summarise(temp_change = mean(temp_change, na.rm = TRUE), .groups = "drop")

# Remove "No VEI" category
tree_ring_vei_1800_avg <- tree_ring_vei_1800_avg %>%
  filter(vei_category != "No VEI")

# Visualize temperature changes by VEI category using an area plot
ggplot(tree_ring_vei, aes(x = year, y = temp_change, fill = vei_category)) +
  geom_area(alpha = 0.6) +  # Area plot
  scale_fill_manual(values = c("VEI >= 5" = "red", "VEI 3-4" = "yellow", "VEI 1-2" = "green")) +
  labs(
    title = "Temperature Change by VEI Intensity (1800s Onward)",
    x = "Year",
    y = "Temperature Change (°C)"
  ) +
  theme_minimal()



################################################################################


# Box plot으로 VEI 카테고리별 기온 변화 분포 시각화
# ggplot(tree_ring_vei, aes(x = vei_category, y = temp_change, fill = vei_category)) +
#   geom_boxplot() +  # 박스 플롯 생성
#   labs(
#     title = "Temperature Change by VEI Category (Box Plot)",
#     x = "VEI Category",
#     y = "Temperature Change (°C)"
#   ) +
#   scale_fill_manual(values = c("VEI >= 5" = "red", "VEI 3-4" = "yellow", "VEI 1-2" = "green")) +
#   theme_minimal()


# # VEI 카테고리별 기온 변화량 시각화 (선 그래프)
# ggplot(tree_ring_vei %>% filter(vei_category != "No VEI"), aes(x = year, y = temp_change, color = vei_category)) +
#   geom_line(size = 1) +  # 선 굵기 조정
#   geom_point(data = tree_ring_vei %>% filter(vei_category == "VEI >= 5"), 
#              aes(x = year, y = temp_change), size = 3, color = "red") +  # VEI >= 5 값 강조
#   labs(
#     title = "Temperature Change by Year and VEI Category",
#     x = "Year", 
#     y = "Temperature Change (°C)",
#     color = "VEI Category"
#   ) +
#   theme_minimal() +
#   scale_color_manual(values = c("VEI >= 5" = "red", "VEI 3-4" = "yellow", "VEI 1-2" = "green")) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))  # x축 텍스트 회전

# # VEI 카테고리별 기온 변화량 시각화 (VEI 5 이상 강조)
# ggplot(tree_ring_vei_1800_avg, aes(x = year, y = temp_change, color = factor(vei_category))) +
#   geom_line(aes(size = ifelse(vei_category == "VEI >= 5", 1.5, 1)), show.legend = FALSE) +  # VEI >= 5 강조
#   labs(
#     title = "Temperature Change by Year and VEI Category (VEI >= 5 Emphasized)",
#     x = "Year", 
#     y = "Temperature Change (°C)",
#     color = "VEI Category"
#   ) +
#   theme_minimal() +
#   scale_color_manual(values = c("VEI >= 5" = "red", "VEI 3-4" = "orange", "VEI 1-2" = "green")) +  # 색상 강조
#   scale_size_continuous(range = c(0.5, 1.5))  # 선 두께 조정



# # VEI 카테고리별 기온 변화량 시각화 (VEI 카테고리로 그룹화)
# ggplot(tree_ring_vei_1800, aes(x = year, y = temp_change, color = factor(vei_category))) +
#   geom_line() +
#   labs(
#     title = "Temperature Change by Year and VEI Category",
#     x = "Year",
#     y = "Temperature Change (°C)",
#     color = "VEI Category"
#   ) +
#   theme_minimal() +
#   scale_color_manual(values = c("VEI >= 5" = "red", "VEI 3-4" = "yellow", "VEI 1-2" = "green", "No VEI" = "gray"))  # 각 카테고리에 색상 지정

