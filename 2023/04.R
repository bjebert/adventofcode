rm(list=ls())
source("utilities.R")
inp <- get_input("2023/4", parse = F, deesblake = F, cache = T)


inp <- strsplit("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11", "\n")[[1]]

cards <- sapply(strsplit(inp, ": "), function(x) x[2])

wins <- lapply(strsplit(cards, " \\| "), function(x) as.numeric(strsplit(x[1], " ")[[1]]))
nums <- lapply(strsplit(cards, " \\| "), function(x) as.numeric(strsplit(x[2], " ")[[1]]))

wins <- lapply(wins, function(x) x[!is.na(x)])
nums <- lapply(nums, function(x) x[!is.na(x)])

sum(sapply(1:length(wins), function(i) {
    x <- sum(nums[[i]] %in% wins[[i]])
    
    if(x == 0) {
        return(0)
    } else {
        return(2 ^ (x - 1))
    }
}))



# pt 2 --------------------------------------------------------------------
scratch <- setNames(rep(1, length(cards)), as.character(1:length(cards)))

for(i in 1:length(cards)) {
    qty <- scratch[[as.character(i)]]
    matches <- sum(nums[[i]] %in% wins[[i]])
    
    if(matches > 0) {
        for(n in as.character((i+1):(i+matches))) {
            if(n %in% names(scratch)) {
                scratch[n] <- scratch[n] + qty
            }
        }
    }
}

sum(scratch)
