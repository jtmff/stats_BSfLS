###############################################################################
## EXAMPLE 2: Same as the previous example, but reading the data from a file.
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################

# This shows how to read the data either from a csv file (csv = comma-separated values), or an Excel spreadsheet.

# When running this, do make sure that your working directory contains the 
# data file, otherwise you will get an error stating the file could not be found.
set.seed(111) # We use the same seed as the previous example to have the same
              # jitter of data points shown, to help verify identity.
# Load required libraries
library(ggplot2)
library(Cairo)
library(readxl)  # Load the readxl package

# Read the data from a file - either from a csv, or Excel spreadsheet, depending
# on which you leave uncommented (the files' contents are identical)
df_wide <- read.csv("dataGroupsAB.csv") 
#df_wide <- read_excel("dataGroupsAB.xlsx") 

# Convert from the "wide" to "long" format, as the latter is what many
# R functions require to run. For explanation of the two formats, see,
# e.g., https://libguides.princeton.edu/R-reshape
groups <- names(df_wide)  # Extract column names (group labels)
df_long <- data.frame(
  Value = unlist(df_wide, use.names = FALSE),  # Flatten the data
  Group = rep(groups, each = nrow(df_wide))   # Repeat group names for each value
)

# Create a plot with points and mean (as diamonds) + sd whiskers overlay
plt <- ggplot(df_long, aes(x = Group, y = Value, color = Group)) +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.5, size = 3) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, 
               position = position_dodge(width = 0.75)) +
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1),
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.75),
               mapping = aes(group = Group),
               linewidth = 1) +
  labs(x = "Group", y = "Weight (grams)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24)
  )

# Save the plot using Cairo
CairoPNG(file = "meanSD_read_from_file.png", width = 600, height = 400)
print(plt)
dev.off()