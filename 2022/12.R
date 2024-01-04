rm(list=ls())
source("utilities.R")
inp <- get_input("2022/12", parse = F, deesblake = F, cache = T)

inp <- strsplit("Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi", "\n")[[1]]

grid <- matrix(strsplit(paste(inp, collapse = ""), "")[[1]], nrow = length(inp), byrow = T)


neighbours <- function(pos, steps) {
    moves <- list(c(pos[1] + 1, pos[2]),
                  c(pos[1] - 1, pos[2]),
                  c(pos[1], pos[2] - 1),
                  c(pos[1], pos[2] + 1))

    valid <- list()    
    curr <- grid[pos[1], pos[2]]
    
    if(curr == "S") {
        curr <- "a"
    } else if(curr == "E") {
        curr <- "z"
    }
        
    for(m in moves) {
        if(m[1] >= 1 && m[1] <= nrow(grid) && m[2] >= 1 && m[2] <= ncol(grid)) {
            target <- grid[m[1], m[2]]
            if(target == "E") {
                target <- "z"
            } else if(target == "S") {
                target <- "a"
            }
            
            w1 <- which(letters == curr)
            w2 <- which(letters == target)
            
            if(w2 - w1 <= 1) {
                valid[[length(valid) + 1]] <- c(m, steps + 1)
            }
        }
    }
    
    return(valid)
}


hist <- matrix(Inf, length(grid), nrow = nrow(grid), ncol = ncol(grid))
hist[pos] <- 0

pos <- as.numeric(which(grid == "S", arr.ind = T))
goal <- as.numeric(which(grid == "E", arr.ind = T))

unvisited <- neighbours(pos, 0)

while(length(unvisited) > 0) {
    v <- unvisited[[1]]
    
    if(length(unvisited) > 1) {
        unvisited <- unvisited[2:length(unvisited)]
    } else {
        unvisited <- list()
    }
    
    hist[v[1], v[2]] <- v[3]
    
    n <- neighbours(c(v[1], v[2]), v[3])
    n <- n[unlist(lapply(n, function(x) x[3] < hist[x[1], x[2]]))]
    n <- n[!(n %in% unvisited)]
    
    if(length(n) > 0) {
        unvisited <- c(unvisited, n)
    }
}

hist[grid == "E"]


# Part 2 ------------------------------------------------------------------


neighbours2 <- function(pos, steps) {
    moves <- list(c(pos[1] + 1, pos[2]),
                  c(pos[1] - 1, pos[2]),
                  c(pos[1], pos[2] - 1),
                  c(pos[1], pos[2] + 1))
    
    valid <- list()    
    curr <- grid[pos[1], pos[2]]
    
    if(curr == "S") {
        curr <- "a"
    } else if(curr == "E") {
        curr <- "z"
    }
    
    for(m in moves) {
        if(m[1] >= 1 && m[1] <= nrow(grid) && m[2] >= 1 && m[2] <= ncol(grid)) {
            target <- grid[m[1], m[2]]
            if(target == "E") {
                target <- "z"
            } else if(target == "S") {
                target <- "a"
            }
            
            w1 <- which(letters == curr)
            w2 <- which(letters == target)
            
            if(w1 - w2 <= 1) {
                valid[[length(valid) + 1]] <- c(m, steps + 1)
            }
        }
    }
    
    return(valid)
}


getsteps <- function(pos) {
    hist <- matrix(Inf, length(grid), nrow = nrow(grid), ncol = ncol(grid))
    hist[pos] <- 0
    
    goal <- as.numeric(which(grid == "E", arr.ind = T))
    unvisited <- neighbours2(pos, 0)
    
    while(length(unvisited) > 0) {
        v <- unvisited[[1]]
        
        if(length(unvisited) > 1) {
            unvisited <- unvisited[2:length(unvisited)]
        } else {
            unvisited <- list()
        }
        
        hist[v[1], v[2]] <- v[3]
        
        n <- neighbours2(c(v[1], v[2]), v[3])
        n <- n[unlist(lapply(n, function(x) x[3] < hist[x[1], x[2]]))]
        n <- n[!(n %in% unvisited)]
        
        if(length(n) > 0) {
            unvisited <- c(unvisited, n)
        }
    }
    
    return(hist)
}

pos <- which(grid == "E", arr.ind = T)
hist2 <- getsteps(pos)

min(hist2[grid == "a"])
