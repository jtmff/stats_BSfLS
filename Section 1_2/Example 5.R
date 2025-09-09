###############################################################################
# EXAMPLE 5: Bimodal data (cell sizes) with a single violin plot.
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
library(ggplot2) 
library(Cairo)
library(ggforce)
# Define data
data_cell_size = c(56.4, 35.3, 53.8, 61.1, 49.9, 47.4, 38.3, 55.8,
                   42.5, 42.6, 56.9, 70.9, 37.5, 50.6, 56.2, 51.0,
                   91.1, 90.3, 95.9, 87.3, 84.8, 87.2, 87.3, 97.6,
                   83.5, 91.3, 90.5, 92.8, 83.7, 98.3, 88.2, 95.3)

# Convert into data frame.
df_cell_size = data.frame(Value = unlist(data_cell_size), xGroup = 1)

# Create a violin plot
plt <- ggplot(df_cell_size, aes(x = xGroup, y = Value)) +
  geom_violin(fill = "lightblue") + 
  geom_boxplot(width = .05) + 
  geom_sina() + # like geom_jitter(), but restricts  points within the violin plot 
  labs(title="", y = "Cell size (uM)")+
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_blank(),# Not showing the x-tick
    axis.title.x = element_blank() 
  )

CairoPNG(file = "violin.png", width = 600, height = 400)
print(plt)
dev.off()
