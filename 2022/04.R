rm(list=ls())
source("utilities.R")
inp <- get_input("2022/4", parse = F, deesblake = F, cache = F)

inp <- strsplit("2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8", "\n")[[1]]

pairs <- lapply(strsplit(inp, ","), function(x) strsplit(x, "-"))

sum(sapply(pairs, function(p) {
    e1 <- as.numeric(p[[1]])
    e2 <- as.numeric(p[[2]])
    
    if(any(e1[1]:e1[2] %in% e2[1]:e2[2]) || any(e2[1]:e2[2] %in% e1[1]:e1[2])) {
        return(1)
    }
    return(0)
}))

# 2:22 / 2:34

