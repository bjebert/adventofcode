
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

x <- 3004953

z <- unlist(lapply(0:21, function(i) seq(1, 2*2^i, 2)))
z[x]

# 24:13 (> 100th)

solve2 <- function(x) {
    elves <- setNames(rep(1, x), 1:x)
    i <- 1    
    while(length(elves) > 1) {
        j <- (floor(i + length(elves) / 2) - 1) %% length(elves) + 1
        elves[i] <- elves[i] + elves[j]
        elves <- elves[-j]
        
        if(j > i) {
            if(i >= length(elves)) {
                i <- 1
            } else {
                i <- i %% length(elves) + 1
            }
        } else {
            i <- (i - 1) %% length(elves) + 1
        }
    }
    
    as.numeric(names(elves))
}

z <- c(1, 1, 3)
while(length(z) < x) {
    z <- c(z, 1:max(z))
    z <- c(z, seq(max(z) + 2, max(z) + max(z)*2, 2))
}

z[x]

# 1:30:00 (> 100th)