
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")
source("D:/projects/projecteuler/helpers/primes.R")

# Problem -----------------------------------------------------------------

inp <- as.numeric(get_input("2015/20", parse = F, user = "blakeebets", cache = T))

factors <- function(x) {
    N <- 1:floor(sqrt(x))
    lower <- N[x %% N == 0]
    
    return(sort(unique(c(lower, x / lower))))
}

# Manual "binary" search

for(i in 7e5:8e5) {
    if(fs > inp) {
        print(i)
        break
    }
}

# 6:53 (7th)

for(i in 8e5:1e6) {
    f <- factors(i)
    fs <- sum(f[(i / f) <= 50] * 11)
    
    if(fs > inp) {
        print(i)
        break
    }
}

# 8:15 (3rd)
