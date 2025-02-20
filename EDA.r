library(viridis)
library(ggplot2)
custom_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Apply the custom theme to any plot
ggplot(PenguinData, aes(x = species, y = body_mass, fill = species)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Box Plot with Custom Theme",
       x = "Species",
       y = "Body Mass (g)",
       fill = "Species") +
  custom_theme