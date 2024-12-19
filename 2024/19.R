
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- get_input("2024/19", parse = F, user = "bjebert", cache = F)

# 
# inp <- strsplit("r, wr, b, g, bwu, rb, gb, br
# 
# brwrr
# bggr
# gbbr
# rrbgbr
# ubwu
# bwurrg
# brgr
# bbrgwb", "\n")[[1]]  # Example

towel <- inp[1]
towel <- strsplit(towel, ", ")[[1]]
goal <- inp[3:length(inp)]

# part 1 ------------------------------------------------------------------

sum(sapply(goal, function(g) {
    print(g)
    
    Q <- towel
    
    Q <- Q[sapply(Q, function(x) nchar(x) <= nchar(g) && x == substr(g, 1, nchar(x)))]
    v <- c()
    cnt <- 0
    
    while(length(Q) > 0) {
        curr <- Q[[1]]
        Q <- Q[-1]   
        
        if(curr == g) {
            return(T)
        }
        
        nb <- sprintf("%s%s", curr, towel)
        nb <- nb[sapply(nb, function(x) nchar(x) <= nchar(g) && x == substr(g, 1, nchar(x)))]
        
        if(length(nb) > 0) {
            nb <- nb[!(nb %in% v)]
            v <- unique(c(v, nb))
            Q <- unique(c(nb, Q))
        }
    }
    
    return(F)
}))


# part 2 ------------------------------------------------------------------
# recursive method

ways <- function(g) {
    cnt <- as.bigz(sum(towel == g))
    
    if(nchar(g) <= min(nchar(towel))) {
        return(cnt)
    }
    
    possible <- towel[sapply(towel, function(x) x == substr(g, 1, nchar(x)))]
    for(t in possible) {
        cnt <- cnt + ways_m(substr(g, (nchar(t) + 1), nchar(g)))
    }    
    
    return(cnt)
}

ways_m <- memoise::memoise(ways)
res <- sapply(goal, ways_m)
Reduce(`+`, res)

