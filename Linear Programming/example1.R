library("lpSolve")
f.obj <- c(0.4, 0.5)
f.con <- matrix(c(0.3, 0.1, 0.5, 0.5, 0.6, 0.4), nrow = 3, byrow = TRUE)
f.dir <- c("<=", "=", ">=")
f.rhs <- c(2.7, 6, 6)
solution <- lp("min", f.obj, f.con, f.dir, f.rhs)
solution$solution
