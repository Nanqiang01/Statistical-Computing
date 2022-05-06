# Q1: 用R确认20条矩阵性质
# 1. 矩阵加减法为对应元素相加减
A <- matrix(1:9, nrow = 3, ncol = 3)
B <- matrix(2:10, nrow = 3, ncol = 3)
C <- A + B
# 2. 矩阵交换律
D <- B + A
print(C == D)
# 3. 矩阵结合律
E <- (A + B) + B
G <- A + (B + B)
print(E == G)
# 4. 矩阵数乘为矩阵内每个元素乘上常数
H <- 4 * A
# 5. 矩阵乘法为对应元素相乘
I <- A %*% B
# 6. 矩阵乘法分配律
J <- A %*% (B + C)
K <- A %*% B + A %*% C
print(J == K)
# 7. 矩阵乘法结合律
L <- A %*% (B %*% C)
M <- (A %*% B) %*% C
print(L == M)
# 8. 转置性质1: 矩阵经两次转置后变回原状
print(A == t(t(A)))
# 9. 转置性质2: (A + B)^T = A^T + B^T
print(t(A + B) == t(A) + t(B))
# 10. 转置性质3: (AB)^T = A^TB^T
print(t(A %*% B) == t(B) %*% t(A))
# 11. 矩阵的迹为矩阵所有特征值的和
ev <- eigen(A)
library(lava)
print(tr(A))
print(sum(ev$val))
# 12. 矩阵转置的行列式等于矩阵的行列式
print(det(t(B)) == det(B))
# 13. 矩阵乘积的行列式等于行列式的乘积
print(det(A %*% B) == det(A) * det(B))
# 14. 逆矩阵和矩阵乘积为单位阵
N <- matrix(rnorm(16), 4, 4)
print(round(solve(N) %*% N) == diag(4))
# 15. 数乘矩阵的行列式的性质
print(det(H) == 4^3 * det(A))
# 16. A的逆的转置等于A的转置的逆
print(round(solve(t(N))) == round(t(solve(N))))
# 17， 0乘矩阵为0矩阵
P <- 0 * A
# 18. 数对矩阵的分配律
print(3 * (A + B) == 3 * A + 3 * B)
# 19. 矩阵对数的分配律
print((3 + 4) * A == 3 * A + 4 * A)
# 20. 数对矩阵的结合律
print((3 * 4) * A == 3 * (4 * A))

# Q2: 用R举例简述向量、矩阵、、数组、列表、数据框的适用范围以及相互转化方法
# 向量用于存储数值型、字符型或逻辑型的一维数组
vec1 <- c(1, 2, 5, 3, 6, -2, 4)
vec2 <- c("one", "two", "three")
vec3 <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
# 矩阵是一个二维数组，只是每个元素都属于相同的类型
mat1 <- matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE)
# 数组与矩阵类似，但是维度可以大于2
array1 <- array(1:24, c(2, 3, 4))
# 列表是一些对象的有序集合，某个列表可能是若干向量、矩阵、数据框，甚至是其他列表的组合
list1 <- list(
    title = "list1", ages = c(25, 26, 18, 39),
    matrix(1:10, nrow = 5, ncol = 2), list(title = "list2")
)
# 数据框是一般化的矩阵，不同的列可以包含不同模式的数据
patientID <- c(1, 2, 3, 4)
age <- (c(24, 35, 28, 52))
status <- c("poor", "Improved", "Excellent", "Poor")
df1 <- data.frame(patientID, age, status)
# 向量转化为矩阵
mat2 <- rbind(c(1:4), c(5:8))
# 矩阵转化为向量
vec4 <- c(mat2)
# 向量转化为数组
array2 <- array(vec4, c(2, 2, 2))
# 数组转化为向量
vec5 <- c(array2)
# 矩阵转化为数组
array3 <- array(mat2, c(2, 2, 2))
# 数组转化为矩阵
mat3 <- matrix(c(array3), nrow = 2, ncol = 4)
# 矩阵转化为数据框
df2 <- data.frame(mat1)
# 数据框转化为矩阵
mat4 <- as.matrix(df2)

# Q3: 用R举例列出不少于20个统计分布的随机数、分布函数、密度函数的表示方法
# 1. 二项分布
d1 <- dbinom(10000, 200, 0.4)
r1 <- rbinom(200, 200, 0.4)
q1 <- qbinom(0.6, 10000, 0.4)
# 2. 正态分布
d2 <- dnorm(0, 1)
r2 <- rnorm(200, 0, 1)
q2 <- qnorm(0.5, 0, 1)
# 3. Poisson分布
d3 <- dpois(200, 0.4)
r3 <- rpois(20, 3)
q3 <- qpois(1, 3)
# 4. 超几何分布
r4 <- rhyper(200, 2, 1, 0.4)
# 5. 负二项分布
r5 <- rchisq(10000, 2)
# 6. t分布
r6 <- rt(100, 6)
# 7. F分布
r7 <- rf(100, 6, 6)
# 8. 卡方分布
r8 <- rchisq(10000, 6)
# 9.几何分布
r9 <- rgeom(10000, 0.4)
# 10. 超几何分布
r10 <- rhyper(10000, 2, 1, 0.4)
# 11. 均匀分布
r11 <- runif(10000, 0, 1)
# 12. 指数分布
r12 <- rexp(10000, 0.4)
# 13. 对数正态分布
r13 <- rlnorm(10000, 0, 1)
# 14. logistic分布
r14 <- rlogis(10000, 0.4)