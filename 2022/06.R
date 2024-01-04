rm(list=ls())
source("utilities.R")
inp <- get_input("2022/6", parse = F, deesblake = F, cache = F)

inp <- "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

for(i in 14:nchar(inp)) {
    if(length(unique(strsplit(substr(inp, (i-13), i), "")[[1]])) == 14) {
        print(i)
        stop()
    }
}
