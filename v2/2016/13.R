
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

# inp <- strsplit("", "\n")[[1]]  # Example
inp <- get_input("2016/13", parse = F, user = "blakeebets", cache = T)


get_tile <- function(x, y, inp = 1350) {
    z <- x*x + 3*x + 2*x*y + y + y*y
    z <- z + inp
    
    bits1 <- sum(strsplit(paste(as.character(intToBits(z)), collapse = ""), "")[[1]] == "1")
    
    if(bits1 %% 2 == 0) {
        return('.')
    } else {
        return('#')
    }
}

get_tile_m <- memoise::memoise(get_tile)


start <- "1,1"
best_moves <- Inf

Q <- list(list(pos = start, moves = 0))
V <- setNames(0, start)

while(length(Q) > 0) {
    curr <- Q[[1]]
    Q <- Q[-1]
    
    pos <- curr[["pos"]]
    moves <- curr[["moves"]]
    
    if(pos == "39,31") {
        if(moves < best_moves) {
            best_moves <- moves
            print(moves)
        }

        next
    }
    
    nb <- get_4nb_coord_fast(pos)
    
    w <- sapply(nb, function(x) {
        pos <- coord2pos(x)
        get_tile_m(pos[1], pos[2]) == '.'
    })
    
    possible <- nb[w]
    
    for(p in possible) {
        if(!(p %in% names(V))) {
            V[[p]] <- moves + 1
            Q <- c(list(list(pos = p, moves = moves + 1)), Q)
        } else if(V[[p]] > (moves + 1)) {
            V[[p]] <- moves + 1
            Q <- c(list(list(pos = p, moves = moves + 1)), Q)
        }
    }
    
    # Sort by distance to target
    
    Q <- unique(Q)
}

V["31,39"]

# 20:40 (86th)


# part 2 ------------------------------------------------------------------

start <- "1,1"
Q <- list(list(pos = start, moves = 0))
V <- NULL
last_len <- 0

i <- 0

while(length(Q) > 0) {
    curr <- Q[[1]]
    Q <- Q[-1]
    
    pos <- curr[["pos"]]
    hist <- curr[["hist"]]
    moves <- curr[["moves"]]
    
    if(moves == 50) {
        if(length(V) > last_len) {
            print(length(V))
            last_len <- length(V)
        }
        next
    }
    
    nb <- get_4nb_coord_fast(pos)
    
    w <- sapply(nb, function(x) {
        pos <- coord2pos(x)
        if(pos[1] < 0 || pos[2] < 0) {
            return(FALSE)
        }
        return(get_tile_m(pos[1], pos[2]) == '.')
    })
    
    potential <- lapply(nb[w], function(x) list(pos = x, hist = c(hist, x), moves = moves + 1))
    
    for(p in potential) {
        hash <- p[["pos"]]
        
        if(!(hash %in% names(V))) {
            V[[hash]] <- moves + 1
            Q <- c(list(list(pos = p[["pos"]], hist = p[["hist"]], moves = moves + 1)), Q)
        } else if(V[[hash]] > (moves + 1)) {
            V[[hash]] <- moves + 1
            Q <- c(list(list(pos = p[["pos"]], hist = p[["hist"]], moves = moves + 1)), Q)
        }
    }
    
    Q <- unique(Q)
    # print(length(Q))
    
    if(i %% 1000 == 0) {
        # print(length(Q))
        Q <- Q[order(-sapply(Q, function(x) length(unique(x[["hist"]]))))]
    }
    
    i <- i + 1
}


# 1:12:54 (>100th)


    
    
