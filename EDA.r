#### Required Libraries for the EDA
library(viridis)
library(ggplot2)
library(GGally)
####

#### Custom theme for our visualizations
custom_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
##############

####################### 1 A box plot to compare body mass across species:
ggplot(PenguinData, aes(x = species, y = body_mass, fill = species)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Body mass across species",
       x = "Species",
       y = "Body Mass (g)",
       fill = "Species") +
  custom_theme
########################


################ 2 Pair Plot: Relationships Between Numeric Variables
ggpairs(PenguinData[, c("bill_length", "bill_depth", "flipper_length", "body_mass")],
        aes(color = PenguinData$species, alpha = 0.8)) +
  labs(title = "Pair Plot: Relationships Between Numeric Variables") +
  custom_theme
###########################

#################### 3  Faceted Scatter Plot: Bill Length vs Bill Depth by Island

ggplot(PenguinData, aes(x = bill_length, y = bill_depth, color = species)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_viridis(discrete = TRUE) +
  facet_wrap(~island) +
  labs(title = "Faceted Scatter Plot: Bill Length vs Bill Depth by Island",
       x = "Bill Length (mm)",
       y = "Bill Depth (mm)",
       color = "Species") +
  custom_theme
######################


####################### 4 Violin Plot: Body Mass by Species and Sex
ggplot(PenguinData, aes(x = species, y = body_mass, fill = sex)) +
  geom_violin(alpha = 0.7) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Violin Plot: Body Mass by Species and Sex",
       x = "Species",
       y = "Body Mass (g)",
       fill = "Sex") +
  custom_theme
#########################


############### 5 Density Plot: Flipper Length by Species
ggplot(PenguinData, aes(x = flipper_length, fill = species)) +
  geom_density(alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Density Plot: Flipper Length by Species",
       x = "Flipper Length (mm)",
       y = "Density",
       fill = "Species") +
        custom_theme
##################


########## 6 Bar Plot: Count of Penguins by Species and Island
ggplot(PenguinData, aes(x = species, fill = island)) +
  geom_bar(position = "dodge", alpha = 0.8) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Bar Plot: Count of Penguins by Species and Island",
       x = "Species",
       y = "Count",
       fill = "Island") +
  custom_theme
###############


################## 7 Number of Penguins Captured Each Year by Island and Species
penguin_island_species_counts <- PenguinData %>%
  group_by(year, island, species) %>%
  summarise(count = n(), .groups = 'drop')

ggplot(penguin_island_species_counts, aes(x = year, y = count, color = species)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Adelie" = "#1f77b4", "Chinstrap" = "#ff7f0e", "Gentoo" = "#2ca02c")) +
  facet_wrap(~island, ncol = 1) +
  labs(title = "Number of Penguins Captured Each Year by Island and Species",
       x = "Year",
       y = "Number of Penguins Captured",
       color = "Species") +
  theme_minimal()
#####################

################### 8 Number of Penguins Captured Each Year by Island
penguin_counts_island <- PenguinData %>%
  group_by(year,island) %>%
  summarise(count = n(), .groups = 'drop')

ggplot(penguin_counts_island, aes(x = year, y = count, color = island)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Biscoe" = "#1f77b4", "Dream" = "#ff7f0e", "Torgensen" = "#2ca02c")) +
  labs(title = "Number of Penguins Captured Each Year by Island",
       x = "Year",
       y = "Number of Penguins Captured",
       color = "Island") +
  custom_theme
######################



###########  9 Number of Penguins Captured Each Year by Species
penguin_counts_year <- PenguinData %>%
  group_by(year, species) %>%
  summarise(count = n(), .groups = 'drop')

ggplot(penguin_counts_year, aes(x = year, y = count, color = species)) +
  geom_line(size = 1.2) +  # Draw lines
  geom_point(size = 3) +   # Add points for each year
  scale_color_viridis(discrete = TRUE) +  # Use a pretty color scale
  labs(title = "Number of Penguins Captured Each Year by Species",
       x = "Year",
       y = "Number of Penguins Captured",
       color = "Species") +
  custom_theme
###############
