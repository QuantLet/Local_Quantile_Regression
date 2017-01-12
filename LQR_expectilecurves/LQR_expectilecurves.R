
## clear history
rm(list = ls(all = TRUE))
graphics.off()

## install and load packages
libraries = c("expectreg")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

## generating data from AR(1)
n = 100
m = 10
y = 0
a = 0.7
set.seed(2017)
for (j in 1:m) {
    innov = rnorm(n, mean = 0, sd = 1)
    x     = rep(0, n)
    x[1]  = innov[1]
    for (i in 2:n) {
        x[i] = a * x[i - 1] + innov[i]
    }
    y = cbind(y, x)
}
y = y[, -1]
plot(y[, 1], ylab = "", xlab = "", type = "l", col = "white", ylim = c(min(y), max(y)))
## plot the curves
for (j in 1:m) {
    lines(y[, j])
}
## compute and plot the expectile curves
e = 0
for (v in 1:n) {
    e = rbind(e, expectile(y[v, ], c(0.1, 0.5, 0.9)))
}
e = e[-1, ]
lines(e[, 1], col = "blue", lwd = 3)
lines(e[, 2], col = "darkgreen", lwd = 3)
lines(e[, 3], col = "red", lwd = 3)
