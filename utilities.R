options(scipen = 99)

# Helpful libraries

library(plot.matrix)
library(combinat)     # t(combinat::combn(1:10, 3))
library(gtools)       # gtools::permutations(v = letters[1:10], n = 10, r = 3)
library(openssl)      # openssl::md5
library(gmp)          # gmp::as.bigz(n)
library(Rmpfr)
library(numbers)      # numbers::extGCD(a, b)
library(bit64)

source("utilities/data.R")
source("utilities/bitops.R")

i <- 1  # Debug

# Permutations/combinations -----------------------------------------------

comb <- function(set, n) t(combinat::combn(set, n))
perm <- function(set, n, repeats = FALSE) gtools::permutations(v = set, n = length(set), r = n, repeats.allowed = repeats)

# Matrix ------------------------------------------------------------------

rotate <- function(x) t(apply(x, 2, rev)) 

# Grid --------------------------------------------------------------------

move_map <- list(`^` = c(-1, 0), N = c(-1, 0), U = c(-1, 0), 
                 `>` = c(0, 1), E = c(0, 1), R = c(0, 1), 
                 `v` = c(1, 0), S = c(1, 0), D = c(1, 0), 
                 `<` = c(0, -1), W = c(0, -1), L = c(0, -1))

get_4nb <- function(pos) {
    list(c(pos[1] - 1, pos[2]),  # North
         c(pos[1] + 1, pos[2]),  # South
         c(pos[1], pos[2] + 1),  # East
         c(pos[1], pos[2] - 1))  # West
}

get_8nb <- function(pos) {
    list(
        c(pos[1] - 1, pos[2]),     # North
        c(pos[1] + 1, pos[2]),     # South
        c(pos[1], pos[2] + 1),     # East
        c(pos[1], pos[2] - 1),     # West
        c(pos[1] - 1, pos[2] + 1), # Northeast
        c(pos[1] - 1, pos[2] - 1), # Northwest
        c(pos[1] + 1, pos[2] + 1), # Southeast
        c(pos[1] + 1, pos[2] - 1)  # Southwest
    )
}


factors <- function(n) {
    n <- abs(n)
    f1 <- (1:floor(sqrt(n)))[sapply(1:floor(sqrt(n)), function(i) n %% i == 0)]
    unique(sort(c(f1, n / f1)))
}

str2pos <- function(str) as.numeric(strsplit(str, ",", fixed = TRUE)[[1]])
pos2str <- function(pos) paste0(pos, collapse = ",")

get_4nb_str_slow <- function(str) {
    sapply(get_4nb(str2pos(str)), pos2str)
}

get_8nb_str_slow <- function(str) {  # slow implementation
    sapply(get_8nb(str2pos(str)), pos2str)
}

get_4nb_str <- function(str) {  # fast implementation (N,S,E,W)
    pos <- str2pos(str)
    return(sprintf("%d,%d",
                   c(pos[1] - 1, pos[1] + 1, pos[1], pos[1]),
                   c(pos[2], pos[2], pos[2] + 1, pos[2] - 1)
    ))
}

get_8nb_str <- function(str) {  # fast implementation (N,S,E,W,NE,NW,SE,SW)
    pos <- str2pos(str)
    return(sprintf("%d,%d",
                   c(pos[1] - 1, pos[1] + 1, pos[1], pos[1], pos[1] - 1, pos[1] - 1, pos[1] + 1, pos[1] + 1),
                   c(pos[2], pos[2], pos[2] + 1, pos[2] - 1, pos[2] + 1, pos[2] - 1, pos[2] + 1, pos[2] - 1)
    ))
}


# Pattern -----------------------------------------------------------------

pattern <- function(x, offset = FALSE) {  # find repeating sub-sequences of a sequence x 
    Lx <- length(x)
    while(TRUE) {
        for(n in 2:floor(length(x)/2)) {
            if(length(unique(x[seq(1, length(x), n)])) == 1) {
                s <- seq(1, length(x), n)
                
                subseqs <- lapply(1:(length(x) %/% n), function(j) {
                    x[s[j]:(s[j]+n-1)]
                })
                
                if(length(unique(subseqs)) == 1) {
                    return(list(seq = subseqs[[1]], n = n, offset = Lx - length(x)))
                }
            }
        }
        
        if(!offset || length(x) < (Lx/2)) {
            return(NULL)
        }
        
        x <- x[-1]
        
        if(Lx - length(x) %% 10 == 0) {
            print(sprintf('no subsets found, current offset: %s', Lx - length(x)))
        }
    }
}


# Search ------------------------------------------------------------------


search <- function() {
    map <- mat2map(inp2mat(inp))
    
    walls <- names(map)[map == '#']
    start <- names(map)[map == 'S']
    end <- names(map)[map == 'E']
    
    Q <- list(list(pos = start, path = NULL))
    v <- setNames(0, start)
    
    best <- Inf
    paths <- NULL
    
    iter <- 0
    
    while(length(Q) > 0) {
        curr <- Q[[1]]
        Q <- Q[-1]
        
        pos <- curr[["pos"]]
        path <- curr[["path"]]
        steps <- length(curr[["path"]])
        
        if(steps > best) {
            next
        }
        
        if(pos == end) {
            if(steps < best) {
                best <- steps
                paths <- path
            } else if(steps == best) {
                paths <- c(paths, path)
            }
            
            next
        }
        
        nb <- get_4nb_str(pos)
        nb <- nb[!(nb %in% walls)]
        
        for(n in nb) {
            if(n %in% names(v) && (steps + 1) >= v[[n]]) {
                next
            }
            
            v[[n]] <- steps + 1
            Q <- c(Q, list(list(pos = n, path = c(path, n))))
        }
        
        iter <- iter + 1
        
        if(iter %% 1000 == 0) {
            # print(length(Q))
            Q <- Q[order(sapply(Q, function(x) sum(abs(str2pos(x[["pos"]]) - str2pos(end)))))]
        }
    }
    
    return(list(best = best, 
                paths = paths))
}


