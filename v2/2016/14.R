
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

# inp <- strsplit("", "\n")[[1]]  # Example
inp <- "abc"
inp <- get_input("2016/14", parse = F, user = "blakeebets", cache = T)


generate_hashes <- function() {
    hashes <- sprintf("%s%d", inp, 0:3e4)
    
    for(i in 1:2017) {
        hashes <- md5(hashes)    
    }
    
    return(hashes)
}

hashes <- generate_hashes()
hs <- strsplit(hashes, "")

triple <- function(x) {
    res <- lapply(1:(length(x)-2), function(i) x[i:(i+2)])
    triples <- res[sapply(res, function(y) length(unique(y)) == 1)]
    
    if(length(triples) == 0) {
        return(NA)
    } else {
        return(triples[[1]][1])
    }
}


five <- function(x) {
    res <- lapply(1:(length(x)-4), function(i) x[i:(i+4)])
    fx <- res[sapply(res, function(y) length(unique(y)) == 1)]
    
    if(length(fx) == 0) {
        return(NA)
    } else {
        return(sapply(fx, function(x) x[1]))
    }
}


triples <- sapply(1:3e4, function(i) {
    triple(hs[[i]])
})


fives <- sapply(1:3e4, function(i) {
    five(hs[[i]])
})

w <- which(!sapply(triples, function(x) all(is.na(x))))

keys <- c()

get_key <- function(i) {
    for(char in triples[[i]]) {
        if(char %in% unlist(fives[(i+1):(i+1000)])) {
            keys <- c(keys, i)
            return(i)
        }
    }
    return(NULL)
}

while(length(keys) < 64) {
    for(i in w) {
        keys <- c(keys, get_key(i))
    }
}

keys[64] - 1   # zero-index

# 21:59 (83rd)

# 26:40 (50th)
