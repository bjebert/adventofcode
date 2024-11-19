
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")
inp <- get_input("2015/15", parse = F, user = "blakeebets", cache = T)

# Problem -----------------------------------------------------------------

inp <- strsplit("Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3", "\n")[[1]]
mat <- as.matrix(as.data.table(expand.grid(a = 0:100, b = 0:100))[a+b == 100])

ss <- strsplit(inp, " ")

stats <- list(as.numeric(gsub(",", "", sapply(ss, function(x) x[3]))),
              as.numeric(gsub(",", "", sapply(ss, function(x) x[5]))),
              as.numeric(gsub(",", "", sapply(ss, function(x) x[7]))),
              as.numeric(gsub(",", "", sapply(ss, function(x) x[9]))),
              as.numeric(gsub(",", "", sapply(ss, function(x) x[11]))))


mat <- as.matrix(as.data.table(expand.grid(a = 0:100, b = 0:100, c = 0:100, d = 0:100))[a+b+c+d == 100])

max(apply(sapply(1:4, function(i) {
    pmax(rowSums(mat * matrix(rep(stats[[i]], nrow(mat)), ncol = ncol(mat), byrow = T)), 0)
}), 1, prod))

# 25:43 (smh), 94th

# Part 2 ------------------------------------------------------------------

# Exactly 500 calories per cookie

dt <- as.data.table(expand.grid(a = 0:100, b = 0:100, c = 0:100, d = 0:100))[a+b+c+d == 100]

dt[, calories := a * stats[[5]][1] + b * stats[[5]][2] + c * stats[[5]][3] + d * stats[[5]][4]]
dt <- dt[calories == 500]

mat <- as.matrix(dt[, -"calories"])

max(apply(sapply(1:4, function(i) {
    pmax(rowSums(mat * matrix(rep(stats[[i]], nrow(mat)), ncol = ncol(mat), byrow = T)), 0)
}), 1, prod))

# 27:12, 81st