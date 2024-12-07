
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example
inp <- get_input("2016/06", parse = F, user = "blakeebets", cache = T)

inp[1]
m <- matrix(strsplit(paste(inp, collapse = ""), "")[[1]], ncol = 8, byrow = T)

x <- sapply(1:8, function(i) {
    t <- table(m[,i])
    names(rev(sort(t))[1])
})

paste(x, collapse = "")

# 1:36 (5th)

x <- sapply(1:8, function(i) {
    t <- table(m[,i])
    names(sort(t)[1])
})

paste(x, collapse = "")

# 1:52 (2nd)
