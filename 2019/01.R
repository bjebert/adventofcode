rm(list=ls())
source("utilities.R")
inp <- get_input("2019/1")

sum(sapply(inp, function(x) floor(x/3) - 2))

ans(3332538, "2019/1/1")

# 0:22 (1st)

sum(sapply(inp, function(x) {
    tot <- 0
    while(x >= 0) {
        x <- floor(x/3) - 2    
        tot <- tot + max(0, x)
    }
    return(tot)
}))

ans(4995942, "2019/1/2")

# 4:28 (>100th)
