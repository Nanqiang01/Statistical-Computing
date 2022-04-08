options(warnPartialMatchArgs = TRUE)
rm(list = ls())
library(ggplot2)
library(cowplot)
library(showtext)

# 定义寻根函数
findroot <- function(g, dg, x0, plot = TRUE) {
    MaxIter <- 500
    x <- rep(NA, MaxIter + 1)
    x[1] <- x0
    for (n in 1:MaxIter) {
        x[n + 1] <- x[n] - (g(x[n]) / dg(x[n]))
        if (abs(g(x[n + 1])) <= 1E-10) {
            break
        }
    }
    if (n == MaxIter) {
        warning("Maximum number of iterations reached without convergence")
    }
    return(list(x[1:(n + 1)], n))
}
# 定义待求根函数、导数及初值
g1 <- function(x) {
    y <- sqrt(abs(x))
    return(y)
}
dg1 <- function(x) {
    y <- ifelse(x > 0, 1 / sqrt(abs(x)), -1 / sqrt(abs(x)))
    return(y)
}
# 求根
root1 <- findroot(g1, dg1, 0.25, plot = TRUE)
# ----------root1结果----------
gridx <- seq(-2, 2, 0.01)
gridy <- g1(gridx)
plot(gridx, gridy, "l",
    lwd = 3,
    main = "Function", xlab = "x", ylab = expression(y = sqrt(abs(x)))
)
lines(c(-2, 2), c(0, 0))

gridx <- seq(0, root1[[2]], 1)
gridy <- root1[[1]]
plot(gridx, gridy, "l",
    lwd = 3, main = "Function", xlab = "n", ylab = "value of x"
)
points(gridx, gridy, pch = 18, col = "black")

gridx <- seq(0, root1[[2]], 1)
gridy <- g1(root1[[1]])
plot(gridx, gridy, "l",
    lwd = 3, main = "Function", xlab = "n", ylab = "value of g(x)"
)
points(gridx, gridy, pch = 18, col = "black")

gridx <- seq(0, root1[[2]], 1)
gridy <- dg1(root1[[1]])
plot(gridx, gridy, "l",
    lwd = 3, main = "Function", xlab = "n", ylab = "value of dg(x)"
)
points(gridx, gridy, pch = 18, col = "black")
# ----------
g2 <- function(x) {
    y <- x * exp(-x^2) - 0.4 * (exp(x) + 1)^(-1) + 0.2
    return(y)
}
dg2 <- function(x) {
    y <- exp(-x^2) - x * (exp(-x^2) * (2 * x)) -
        0.4 * ((exp(x) + 1)^(-2) * ((-1) * exp(x)))
    return(y)
}
# 求根
root2 <- findroot(g2, dg2, 0.5, plot = TRUE)
# root3 <- findroot(g2, dg2, 0.6, plot = TRUE)
# root3会报错,波动非常大
# ----------root2结果----------
gridx <- seq(-2, 2, 0.01)
gridy <- g2(gridx)
plot(gridx, gridy, "l",
    lwd = 3,
    main = "Function", xlab = "x", ylab = expression(y = sqrt(abs(x)))
)
lines(c(-2, 2), c(0, 0))
gridx <- seq(0, root2[[2]], 1)
gridy <- root2[[1]]
plot(gridx, gridy, "l",
    lwd = 3, main = "Function", xlab = "n", ylab = "value of x"
)
points(gridx, gridy, pch = 18, col = "black")

gridx <- seq(0, root2[[2]], 1)
gridy <- g2(root2[[1]])
plot(gridx, gridy, "l",
    lwd = 3, main = "Function", xlab = "n", ylab = "value of g(x)"
)
points(gridx, gridy, pch = 18, col = "black")

gridx <- seq(0, root2[[2]], 1)
gridy <- dg2(root2[[1]])
plot(gridx, gridy, "l",
    lwd = 3, main = "Function", xlab = "n", ylab = "value of dg(x)"
)
points(gridx, gridy, pch = 18, col = "black")
#--------------------
g3 <- function(x) {
    y <- x^3 - 2 * x^2 - 11 * x + 12
    return(y)
}
dg3 <- function(x) {
    y <- 3 * x^2 - 4 * x - 11
    return(y)
}
root4 <- findroot(g3, dg3, 2.35287527, plot = TRUE)
root5 <- findroot(g3, dg3, 2.35284172, plot = TRUE)
# ----------root4结果----------
gridx <- seq(-1, 3, 0.01)
gridy <- g3(gridx)
plot(gridx, gridy, "l",
    lwd = 3,
    main = "Function", xlab = "x", ylab = expression(y = sqrt(abs(x)))
)
lines(c(-1, 3), c(0, 0))
gridx <- seq(0, root4[[2]], 1)
gridy <- root4[[1]]
plot(gridx, gridy, "l",
    lwd = 3, main = "Function", xlab = "n", ylab = "value of x"
)
points(gridx, gridy, pch = 18, col = "black")

gridx <- seq(0, root4[[2]], 1)
gridy <- g3(root4[[1]])
plot(gridx, gridy, "l",
    lwd = 3, main = "Function", xlab = "n", ylab = "value of g(x)"
)
points(gridx, gridy, pch = 18, col = "black")

gridx <- seq(0, root4[[2]], 1)
gridy <- dg3(root4[[1]])
plot(gridx, gridy, "l",
    lwd = 3, main = "Function", xlab = "n", ylab = "value of dg(x)"
)
points(gridx, gridy, pch = 18, col = "black")
# --------------------
# ----------root5结果-----------
gridx <- seq(-1, 3, 0.01)
gridy <- g3(gridx)
plot(gridx, gridy, "l",
    lwd = 3,
    main = "Function", xlab = "x", ylab = expression(y = sqrt(abs(x)))
)
lines(c(-1, 3), c(0, 0))
gridx <- seq(0, root5[[2]], 1)
gridy <- root5[[1]]
plot(gridx, gridy, "l",
    lwd = 3, main = "Function", xlab = "n", ylab = "value of x"
)
points(gridx, gridy, pch = 18, col = "black")

gridx <- seq(0, root5[[2]], 1)
gridy <- g3(root5[[1]])
plot(gridx, gridy, "l",
    lwd = 3, main = "Function", xlab = "n", ylab = "value of g(x)"
)
points(gridx, gridy, pch = 18, col = "black")

gridx <- seq(0, root5[[2]], 1)
gridy <- dg3(root5[[1]])
plot(gridx, gridy, "l",
    lwd = 3, main = "Function", xlab = "n", ylab = "value of dg(x)"
)
points(gridx, gridy, pch = 18, col = "black")
# --------------------
g4 <- function(x) {
    y <- 2 * x^3 + 3 * x^2 + 5
    return(y)
}
dg4 <- function(x) {
    y <- 6 * x^2 - 6 * x
    return(y)
}
root6 <- findroot(g3, dg3, 0.5, plot = TRUE)
root7 <- findroot(g3, dg3, 0, plot = TRUE)
# ----------root6结果----------
gridx <- seq(-1, 3, 0.01)
gridy <- g4(gridx)
plot(gridx, gridy, "l",
    lwd = 3,
    main = "Function", xlab = "x", ylab = expression(y = sqrt(abs(x)))
)
lines(c(-1, 3), c(0, 0))
gridx <- seq(0, root6[[2]], 1)
gridy <- root6[[1]]
plot(gridx, gridy, "l",
    lwd = 3, main = "Function", xlab = "n", ylab = "value of x"
)
points(gridx, gridy, pch = 18, col = "black")

gridx <- seq(0, root6[[2]], 1)
gridy <- g4(root6[[1]])
plot(gridx, gridy, "l",
    lwd = 3, main = "Function", xlab = "n", ylab = "value of g(x)"
)
points(gridx, gridy, pch = 18, col = "black")

gridx <- seq(0, root6[[2]], 1)
gridy <- dg4(root6[[1]])
plot(gridx, gridy, "l",
    lwd = 3, main = "Function", xlab = "n", ylab = "value of dg(x)"
)
points(gridx, gridy, pch = 18, col = "black")
# --------------------
# ----------root7结果----------
gridx <- seq(-1, 3, 0.01)
gridy <- g4(gridx)
plot(gridx, gridy, "l",
    lwd = 3,
    main = "Function", xlab = "x", ylab = expression(y = sqrt(abs(x)))
)
lines(c(-1, 3), c(0, 0))
gridx <- seq(0, root7[[2]], 1)
gridy <- root7[[1]]
plot(gridx, gridy, "l",
    lwd = 3, main = "Function", xlab = "n", ylab = "value of x"
)
points(gridx, gridy, pch = 18, col = "black")

gridx <- seq(0, root7[[2]], 1)
gridy <- g4(root7[[1]])
plot(gridx, gridy, "l",
    lwd = 3, main = "Function", xlab = "n", ylab = "value of g(x)"
)
points(gridx, gridy, pch = 18, col = "black")
png("test.png")
gridx <- seq(0, root7[[2]], 1)
gridy <- dg4(root7[[1]])
plot(gridx, gridy, "l",
    lwd = 3, main = "Function", xlab = "n", ylab = "value of dg(x)"
)
points(gridx, gridy, pch = 18, col = "black")
dev.off()
# --------------------