rm(list=ls())
source("utilities.R")
inp <- get_input("2021/1")

z <- 0
for(i in 4:length(inp)) {
    if(inp[i] + inp[i-1] + inp[i-2] > inp[i-1] + inp[i-2] + inp[i-3]) {
        z <- z + 1
    }
}

sum(inp > shift(inp, 1), na.rm = T)
sum(inp + shift(inp, 1) + shift(inp, 2) > shift(inp, 1) + shift(inp, 2) + shift(inp, 3), na.rm = T)

# 0:41 (16th, was 9th)
# 1:44 (17th, was 4th)
