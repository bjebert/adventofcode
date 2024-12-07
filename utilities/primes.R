sieve <- readRDS("utilities/prime_sieve.rds")


factors <- function(x) {
    N <- 1:floor(sqrt(x))
    lower <- N[x %% N == 0]
    
    return(sort(unique(c(lower, x / lower))))
}


proper_factors <- function(x) {
    f <- factors(x)
    return(f[f != 1 & f != x])
}


prime_factors <- function(x) {
    pf <- proper_factors(x)
    if(length(pf) == 0) {
        return(pf)
    }
    
    return(pf[sapply(pf, is_prime)])
}


proper_divisors <- function(x) {
    f <- factors(x)
    return(f[f != x])
}


is_prime <- function(x) {
    if(is.na(x)) return(NA)
    return(length(factors(x)) == 2)
}


is_prime_sieve <- function(x, sieve) {
    filt <- sieve[sieve <= floor(sqrt(x))]
    
    if(max(sieve) < floor(sqrt(x))) {  # Sieve not big enough
        filt <- c(filt, (max(filt) + 1):(floor(sqrt(x))))
    }
    
    return(all(x %% filt != 0))
}


create_prime_sieve <- function(max_N = 1e6, existing_sieve = NULL, write = FALSE) {
    
    if(is.null(existing_sieve)) {  # create a small sieve to speed up calculation
        num_small <- 1:floor(max_N / 100)
        existing_sieve <- num_small[sapply(num_small, is_prime)] 
    }
    
    p <- (1:max_N)[sapply(1:max_N, function(n) is_prime_sieve(n, existing_sieve))]
    
    if(write) {
        saveRDS(p, "helpers/prime_sieve.rds")
    }
    
    return(p)
}
