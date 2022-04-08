df1 <- read.csv("R\\R Graphics\\面积.csv", header = F)
vec1 <- as.vector(df1[, "V1"])
# hist(vec1,
#     breaks = c(30, 60, 90, 120, 150, 180, 210, 240, 270, 300),
#     freq = TRUE, col = "#9dc3e6", main = "", xlab = "面积/m2",
#     ylab = "频数", xlim = c(30, 300),
#     ylim = c(0, 6500)
# )
library(ggplot2)
library(latex2exp)
ggplot(data = df1, aes(V1)) +
    geom_histogram(fill = "#9dc3e6", color = "black", alpha = 1, binwidth = 30) +
    scale_fill_manual(values = "#FB882C") +
    theme(strip.background = element_blank(), legend.position = "none") +
    labs(x = TeX("面积/$m^2$"), y = "频数") +
    scale_x_continuous(breaks = c(30, 60, 90, 120, 150, 180, 210, 240, 270, 300)) +
    theme_bw() +
    theme(panel.grid = element_blank())