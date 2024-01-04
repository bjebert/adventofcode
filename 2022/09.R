rm(list=ls())
source("utilities.R")
inp <- get_input("2022/9", parse = F, deesblake = F, cache = F)

inp <- strsplit("R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20", "\n")[[1]]


is_touching <- function(posH, posT) {
    if(abs(posT[1] - posH[1]) <= 1 && abs(posT[2] - posH[2]) <= 1) {  # Diagonal
        return(TRUE)
    }
    return(FALSE)
}


move_tail <- function(posH, posT) {
    if(abs(posH[1] - posT[1]) == 2 && abs(posH[2] - posT[2]) == 2) {
        posT[1] <- posT[1] + (posH[1] - posT[1]) / 2
        posT[2] <- posT[2] + (posH[2] - posT[2]) / 2
    } else if(abs(posH[1] - posT[1]) == 2 && abs(posH[2] - posT[2]) == 1) {
        posT[1] <- posT[1] + (posH[1] - posT[1]) / 2
        posT[2] <- posT[2] + (posH[2] - posT[2])
    } else if(abs(posH[1] - posT[1]) == 1 && abs(posH[2] - posT[2]) == 2) {
        posT[1] <- posT[1] + (posH[1] - posT[1])
        posT[2] <- posT[2] + (posH[2] - posT[2]) / 2
    } else if(abs(posH[1] - posT[1]) == 2 && abs(posH[2] - posT[2]) == 0) {
        posT[1] <- posT[1] + (posH[1] - posT[1]) / 2
    } else if(abs(posH[2] - posT[2]) == 2 && abs(posH[1] - posT[1]) == 0) {
        posT[2] <- posT[2] + (posH[2] - posT[2]) / 2
    } else {
        stop()
    }
    
    return(posT)
}

positions <- lapply(1:10, function(x) c(0, 0))
hist <- c("0|0")


for(i in 1:length(inp)) {
    line <- inp[i]

    ss <- strsplit(line, " ")[[1]]
    dir <- ss[1]
    amt <- as.numeric(ss[2])
    
    if(dir == "R") {
        x <- 0
        y <- 1
    } else if(dir == "L") {
        x <- 0
        y <- -1
    } else if(dir == "U") {
        x <- 1
        y <- 0
    } else if(dir == "D") {
        x <- -1
        y <- 0
    }
    
    for(step in 1:amt) {
        positions[[1]] <- c(positions[[1]][1] + x, positions[[1]][2] + y)
        
        for(pos in 2:10) {
            if(!is_touching(positions[[pos-1]], positions[[pos]])) {
                positions[[pos]] <- move_tail(positions[[pos-1]], positions[[pos]])
                
                if(pos == 10) {
                    pos_str <- sprintf("%s|%s", positions[[pos]][1], positions[[pos]][2])
                    if(!(pos_str %in% hist)) {
                        hist <- c(hist, pos_str)
                    }
                }
            }
        }
    }
    
    for(pos in 1:9) {
        if(positions[[pos]][1] %% 1 != 0) {
            stop(i)
        }
        
        if(sum(abs(positions[[pos]] - positions[[pos+1]])) > 3) {
            stop(i)
        }
    }
}

length(hist)
