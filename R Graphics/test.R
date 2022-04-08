# Library
library(fmsb)
library(dplyr)

# Create data: note in High school for several students
set.seed(99)
df1 <- read.csv("D:\\Codefield\\R\\R Graphics\\data.csv")
mat1 <- as.matrix(df1)
z <- as.vector(scale(mat1, center = FALSE, scale = TRUE))
dim(z) <- c(3, 6)
df2 <- as.data.frame(z)
colnames(df2) <- c("伤害值", "开火速率", "后坐力控制", "精准射程", "机动性", "护甲穿透")
rownames(df2) <- c("AK-47", "M4A1-S", "M4A4")

# Set graphic colors
library(RColorBrewer)
coul <- brewer.pal(3, "BuPu")
colors_border <- coul
library(scales)
colors_in <- alpha(coul, 0.3)

# If you remove the 2 first lines, the function compute the max and min of each variable with the available data:
radarchart(df2,
    axistype = 0, maxmin = F,
    # custom polygon
    pcol = colors_border, pfcol = colors_in, plwd = 4, plty = 1,
    # custom the grid
    cglcol = "grey", cglty = 1, axislabcol = "black", cglwd = 0.8,
    # custom labels
    vlcex = 0.8
)

# Add a legend
legend(x = 0.7, y = 1, legend = rownames(df2), bty = "n", pch = 20, col = colors_in, text.col = "grey", cex = 1.2, pt.cex = 3)