###############################################################################
# EXAMPLE 6: Paired data with an overlaid boxplot.
## From Tomek & Eisner: Basic Statistics for Life Scientists. https://github.com/jtmff/stats_BSfLS
###############################################################################
library(ggpubr) 
library(Cairo)
data_pre <- c(2, 3, 2.6, 1.5, 2.9)
data_post <- c(2.5, 3.4, 2.7, 1.1, 3.3)
df <- data.frame(pre = data_pre, post = data_post)

lineSize <- 1.2

p1t <- ggpaired(df, cond1 = "pre", cond2 = "post", 
               color = "condition", palette = "npg", point.size = 5,
               line.color = "gray", line.size = 1.5) + 
  scale_x_discrete(labels = c("Before treatment", "After treatment")) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 24),
        axis.title.y = element_text(size = 24))  

CairoPNG(file = "paired.png", width = 600, height = 400)
print(p1t)
dev.off()
