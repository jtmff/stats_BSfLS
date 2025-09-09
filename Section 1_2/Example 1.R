###############################################################################
## EXAMPLE 1: Bell-shaped data with a mean-and-whiskers summary overlaid.
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
# See also comments describing whether to show plots in RStudio or save images to 
# hard drive, and an explanation of how to set the working directory.

# Load the library of functions ggplot2. Before running this
# the first time, you may need to install it using the following command:
# install.packages("ggplot2"). 
library(ggplot2) 

# We use the Cairo library for plotting figures and saving them to a hard drive
# See the end of example for how to plot so that output images are shown in
# RStudio, rather than saved to a hard drive.
library(Cairo)

set.seed(111)  # Fixed “seed” of the random number generator (for reproducibility)

dataA <- c(171.57, 191.66, 167.33, 201.35, 174.83, 178.04, 181.84, 175.55)
dataB <- c(145.71, 148.38, 169.11, 169.76, 144.23, 162.04, 141.11, 167.39)

# Combine the data into a data frame
df <- data.frame(
  Value = c(dataA, dataB),
  Group = rep(c("A", "B"), each = 8)
)

# Create a plot with points and mean (as diamonds) + sd whiskers overlay
plt <- ggplot(df, aes(x = Group, y = Value, color = Group)) +
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


# Save the plot in a file (in your working directory).We do this using the library
# Cairo, which produces nicer and more publication-ready plots than default R.
# This stores the image in your 'Working directory'. To make sure your working
# directory is the same as this script (if you want to store the image in the
# same folder), right-click the name of this script at the tab name in the 
# script editor, and hit 'Set Working Directory'. Alternatively, you may choose
# the working directory through the 'File' tab in Rstudio. Finally, you can use
# the console command 'setwd'
CairoPNG(file = "meanSD.png", width = 600, height = 400)
print(plt)
dev.off()

print(plt)

# If you prefer to have the plot shown in RStudio (‘Plots’ pane), just omit the 
# CairoPNG and dev.off() calls. Alternatively, you can just copy print(plt) outside
# the # CairoPNG-dev.off lines, also keeping it there, in which case the plot gets
# shown in RStudio and is also stored on the hard drive.
# If your script plots multiple images in RStudio, you can switch between them using
# the small arrows in the top left corner of the ‘Plots’ pane.
