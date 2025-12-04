
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- get_input("2024/22", parse = F, user = "bjebert", cache = F)
inp <- as.numeric(inp)

mix <- function(n1, n2) {
    xor(n1, n2)  # finally we have a utilities function for 64-bit xor... need generic length now
}

prune <- function(n) {
    n %% 16777216
}

n <- copy(inp)

prices <- matrix(0, nrow = 2000, ncol = length(n))

for(i in 1:2000) {
    n <- prune(mix(n * 64, n))
    n <- prune(mix(floor(n / 32), n))
    n <- prune(mix(n * 2048, n))
    
    price <- as.numeric(n %% 10)
    prices[i, ] <- price
}

moves <- apply(prices, 2, function(x) x - shift(x, 1))


# slightly faster way - just look at sequences and count as we go ------------------
# in general, R is sloooow when not vectorised...

find_best <- function(prices, moves) {
    vmap <- NULL  # total value map
    
    imaps <- lapply(1:length(inp), function(i) {
        imap <- NULL  # iteration map (only add once)
        
        p <- prices[,i]
        m <- moves[,i]
        
        for(j in 2:(length(m) - 4)) {
            k <- paste(m[j:(j+3)], collapse = "")
            
            if(k %in% names(imap)) {
                next
            } else {
                imap[[k]] <- p[j+3]
            }
        }
        
        return(imap)
    })
    
    # aggregate imaps sum
    im <- unlist(imaps, recursive = F)
    inum <- setNames(as.numeric(im), names(im))
    ksum <- tapply(inum, names(inum), sum) 
    
    max(ksum)
}

find_best(prices, moves)

# slower way of doing it  ----------------------------------

get_possible <- function(moves) {
    mseq <- lapply(1:ncol(moves), function(i) {
        m <- moves[,i]
        unique(lapply(2:(length(m)-4), function(j) {
            m[j:(j+3)]
        }))
    })
    
    # optimise by finding sequences that appear in the most different deltas,
    # then sorting by those
    unique(unlist(mseq, recursive = F))
}

possible_seq <- perm(-9:9, 4)
possible_seq <- get_possible(moves)

sell_price <- function(price, delta, sell_seq) {
    for(i in 1:(length(delta) - 4)) {
        if(identical(delta[i:(i+3)], sell_seq)) {
            return(price[i+3])
        }
    }
    return(0)
}

(length(possible_seq) * 15 / 1000)  # estimated time

res <- sapply(possible_seq, function(sell_seq) {
    sum(sapply(1:ncol(prices), function(i) {
        sell_price(prices[,i], moves[,i], c(-2, 1, -1, 3))
    }))
})

max(res)


