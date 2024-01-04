rm(list=ls())
source("utilities.R")
inp <- get_input("2018/1", deesblake = T)

sum(inp)

z <- 0
hist <- c()
while(T) {
    for(i in inp) {
        z <- z + i
        if(z %in% hist) {
            stop(z)
        } else {
            hist <- c(hist, z)
        }
    }
}

# 0:16 (1st) / # 1:43 (1st)
