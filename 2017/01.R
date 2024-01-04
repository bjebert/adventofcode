rm(list=ls())
source("utilities.R")
inp <- get_input("2017/1", deesblake = T, parse = F)

x <- as.numeric(strsplit(inp[1], "")[[1]])

z <- 0
for(i in 2:length(x)) {
    if(x[i] == x[i-1]) {
        z <- z + x[i]
    }
}

sum(x[x == shift(x, 1)], na.rm = T) + 9



z <- 0
# x <- c(1, 2, 1, 3, 1, 4, 1, 5)

for(i in 1:(length(x)/2)) {
    j <- i + (length(x)/2)
    if(x[i] == x[j]) {
        z <- z + x[i]*2 
    }
}

z

# 1:54 (16th) / 6:34 (>100th)