
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example
inp <- get_input("2016/03", parse = F, user = "blakeebets", cache = T)

inp <- gsub("  ", " ", inp)
inp <- gsub("  ", " ", inp)
inp <- gsub("  ", " ", inp)
inp <- gsub("  ", " ", inp)

ss <- strsplit(sapply(inp, trimws), " ")

tri <- sapply(1:3, function(y) as.numeric(sapply(ss, function(x) x[y])))


sum(sapply(1:nrow(tri), function(i) {
    row <- sort(tri[i,])
    row[1] + row[2] > row[3]
}))

# 3:22 (49th)

t2 <- matrix(tri, ncol = 3, byrow = T)

sum(sapply(1:nrow(t2), function(i) {
    row <- sort(t2[i,])
    row[1] + row[2] > row[3]
}))

# 5:02 (11th)
