rm(list=ls())
source("utilities.R")
inp <- get_input("2023/24", parse = F, deesblake = F, cache = T)

inp <- strsplit("19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3", "\n")[[1]]

positions <- lapply(strsplit(sapply(strsplit(inp, " @ "), function(x) x[1]), ", "), as.numeric)
velos <- lapply(strsplit(sapply(strsplit(inp, " @ "), function(x) x[2]), ", "), as.numeric)

# Part 1 ------------------------------------------------------------------

test_area <- c(200000000000000, 400000000000000)


path_overlap <- function(i, j) {
    px_i <- positions[[i]][1]
    py_i <- positions[[i]][2]
    
    px_j <- positions[[j]][1]
    py_j <- positions[[j]][2]
    
    vx_i <- velos[[i]][1]
    vy_i <- velos[[i]][2]
    
    vx_j <- velos[[j]][1]
    vy_j <- velos[[j]][2]
    
    if(vx_j * vy_i == vx_i * vy_j) {
        return(FALSE)
    }
    
    t1 <- (vy_j * (px_i - px_j) + vx_j * (py_j - py_i)) / (vx_j*vy_i - vx_i*vy_j)
    t2 <- (vy_i * (px_i - px_j) + vx_i * (py_j - py_i)) / (vx_j*vy_i - vx_i*vy_j)
    
    if(t1 < 0 || t2 < 0) {
        return(FALSE)
    }
    
    pos_x <- px_i + vx_i * t1
    pos_y <- py_i + vy_i * t1
    
    return(pos_x >= test_area[1] && pos_x <= test_area[2] && pos_y >= test_area[1] && pos_y <= test_area[2])
}


s <- 0
for(i in 1:(length(positions) - 1)) {
    for(j in (i+1):length(positions)) {
        s <- s + path_overlap(i, j)
    }
}

s

