rm(list=ls())
source("utilities.R")
inp <- get_input("2022/1", parse = T, deesblake = F, cache = F)

elves <- list()
j <- 1
for(i in 1:length(inp)) {
    if(inp[i] == "") {
       j <- j + 1  
    } else {
        if(!(as.character(j) %in% names(elves))) {
            elves[[as.character(j)]] <- as.numeric(inp[i])
        } else {
            elves[[as.character(j)]] <- elves[[as.character(j)]] + as.numeric(inp[i])
        }
    }
}

sum(rev(sort(unlist(elves)))[1:3])


# Better solution... ------------------------------------------------------

sums <- sapply(strsplit(strsplit(paste(inp, collapse = ";"), ";;")[[1]], ";"), function(x) sum(as.numeric(x)))

max(sums)
sum(sort(sums, T)[1:3])




