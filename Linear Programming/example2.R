library("lpSolve")
f.obj <- c(rep(1000, 3), rep(750, 3), rep(250, 3))
f.con <- matrix(c(
  rep(c(1, 0, 0), 3), rep(c(0, 1, 0), 3), rep(c(0, 0, 1), 3),
  3, 0, 0, 2, 0, 0, 1, 0, 0, 0, 3, 0, 0, 2, 0, 0, 1, 0, 0, 0, 3, 0, 0, 2, 0, 0, 1,
  rep(1, 3), rep(0, 6), rep(0, 3), rep(1, 3), rep(0, 9), rep(1, 3),
  rep(c(3, -2, 0), 3), rep(c(0, 1, -2), 3), rep(c(-3, 0, 4), 3)
), nrow = 12, byrow = TRUE)
f.dir <- c(rep("<=", 9), rep("=", 3))
f.rhs <- c(400, 600, 300, 600, 800, 375, 600, 500, 325, 0, 0, 0)
solution <- lp("max", f.obj, f.con, f.dir, f.rhs)
solution$solution