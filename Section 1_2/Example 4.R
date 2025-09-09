###############################################################################
# EXAMPLE 4: Boxplot for skewed data describing the duration of hospital stay for two diseases.
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
library(ggplot2) 
library(Cairo)
data_hs_diagnosis1 = c(15.3, 15.9, 20.5, 19.9, 16.2, 27.1, 21.6, 17.5, 16.09)
data_hs_diagnosis2 = c(16.4, 12.1, 7.6, 6.8, 6.8, 17.2, 6.8, 6.9, 8)

# Combine the data into a data frame
data_diagnoses <- data.frame(
  Value = c(data_hs_diagnosis1, data_hs_diagnosis2),
  Group = rep(c("Diagnosis 1", "Diagnosis 2"), each = 9)
)

# Create a boxplot with overlaid points
plt <- ggplot(data_diagnoses, aes(x = Group, y = Value, color = Group)) +
  geom_boxplot(width = 0.5) +
  geom_jitter(position = position_jitter(width = 0.1), alpha = 0.5) +
  labs(x = "Group",  y = "Hospital stay (days)") + 
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24)
  )

CairoPNG(file = "boxplot.png", width = 600, height = 400)
print(plt)
dev.off()