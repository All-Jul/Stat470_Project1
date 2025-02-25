#### Required Libraries for the EDA
library(viridis)
library(ggplot2)
library(GGally)
library(ggridges)
library(ggExtra)

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

################ 3  Faceted Scatter Plot: Bill Length vs Bill Depth by Island

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


##### 10 Box Plot: Body Mass Over Years
ggplot(PenguinData, aes(x = factor(year), y = body_mass, fill = factor(year))) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Box Plot: Body Mass Over Years",
       x = "Year",
       y = "Body Mass (g)",
       fill = "Year") +
  custom_theme
#########


###### 11 Species Distribution Over Years
ggplot(PenguinData, aes(x = factor(year), fill = species)) +
  geom_bar(position = "stack") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Species Distribution Over Years",
       x = "Year",
       y = "Count",
       fill = "Species") +
  theme_minimal()
########

############### 12 Pie Chart: Proportion of Species
species_counts <- PenguinData %>%
  group_by(species) %>%
  summarise(count = n())

ggplot(species_counts, aes(x = "", y = count, fill = species)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +  # Convert to pie chart
  scale_fill_viridis(discrete = TRUE) +  # Pretty color scale
  labs(title = "Pie Chart: Proportion of Species",
       fill = "Species") +
  theme_void()  # Remove background and axes
################



########### 13 Ridgeline Plot: Body Mass Distribution by Species
ggplot(PenguinData, aes(x = body_mass, y = species, fill = species)) +
  geom_density_ridges(alpha = 0.7) +
  scale_fill_viridis(discrete = TRUE) +  # Pretty color scale
  labs(title = "Ridgeline Plot: Body Mass Distribution by Species",
       x = "Body Mass (g)",
       y = "Species",
       fill = "Species") +
  theme_minimal()
###########


############### 14 2D Density Plot with Contour Lines
ggplot(PenguinData, aes(x = flipper_length, y = body_mass)) +
  geom_density_2d_filled(aes(fill = after_stat(level)), alpha = 0.8) +  # Density fill
  geom_density_2d(color = "black", size = 0.5) +  # Contour lines
  scale_fill_viridis(discrete = TRUE, option = "plasma") +  # Pretty color scale
  labs(title = "2D Density Plot: Body Mass vs Flipper Length",
       subtitle = "Density of Penguins by Body Mass and Flipper Length",
       x = "Flipper Length (mm)",
       y = "Body Mass (g)",
       fill = "Density") +
  theme_minimal() 
###################


################## 15 Stacked Bar Plot: Sex Distribution by Island
ggplot(PenguinData, aes(x = island, fill = sex)) +
  geom_bar(position = "stack", alpha = 0.8) +
  scale_fill_viridis(discrete = TRUE) +  # Pretty color scale
  labs(title = "Stacked Bar Plot: Sex Distribution by Island",
       x = "Island",
       y = "Count",
       fill = "Sex") +
  theme_minimal()

###################



#################### 16 Scatter plot with marginal histograms
p <- ggplot(PenguinData, aes(x = flipper_length, y = body_mass, color = species)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_viridis(discrete = TRUE) +  # Pretty color scale
  labs(title = "Scatter Plot with Marginal Histograms: Body Mass vs Flipper Length",
       x = "Flipper Length (mm)",
       y = "Body Mass (g)",
       color = "Species") +
  theme_minimal()

ggMarginal(p, type = "histogram", fill = "slateblue", alpha = 0.7)  # Add marginal histograms
#######################

################ 17 Bill Length by Species
ggplot(PenguinData, aes(x = species, y = bill_length, fill = species)) +
  geom_violin(alpha = 0.7) +  # Violin plot
  geom_boxplot(width = 0.2, alpha = 0.8) +  # Box plot overlay
  scale_fill_viridis(discrete = TRUE) +  # Pretty color scale
  labs(title = "Violin Plot with Box Plot: Bill Length by Species",
       x = "Species",
       y = "Bill Length (mm)",
       fill = "Species") +
  theme_minimal()
##################

############## 18 Stacked area chart

species_year_counts <- PenguinData %>%
  group_by(year, species) %>%
  summarise(count = n(), .groups = 'drop')

# Create the stacked area chart
ggplot(species_year_counts, aes(x = year, y = count, fill = species)) +
  geom_area(alpha = 0.8) +  # Stacked area
  scale_fill_viridis(discrete = TRUE) +  # Pretty color scale
  labs(title = "Stacked Area Chart: Species Distribution Over Time",
       x = "Year",
       y = "Number of Penguins",
       fill = "Species") +
  theme_minimal()
##################