rm(list=ls())
source("utilities.R")
inp <- get_input("2023/14", parse = F, deesblake = F, cache = T)

inp <- strsplit("O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....", "\n")[[1]]

mat <- inp2mat(inp)


# Part 1 ------------------------------------------------------------------

roll_n <- function(mat) {
    for(i in 1:ncol(mat)) {
        col <- mat[, i]
        
        w_o <- which(col == 'O')
        w_hash <- c(0, which(col == '#'))
        
        stacks <- sapply(1:length(w_hash), function(j) {
            if(j == length(w_hash)) {
                return(sum(w_o > w_hash[j]))
            } else {
                return(sum(w_o > w_hash[j] & w_o < w_hash[j + 1]))
            }
        })
        
        # Restack Os
        col[col != '#'] <- '.'
        for(j in 1:length(w_hash)) {
            if(stacks[j] > 0) {
                col[(w_hash[j]+1):(w_hash[j]+stacks[j])] <- "O"
            }
        }
        
        mat[, i] <- col
    }
    
    return(mat)
}


count <- function(mat) {
    sum(matrix(rep(nrow(mat):1, ncol(mat)), nrow = nrow(mat)) * (mat == "O"))
}

count(roll_n(copy(mat)))


# Part 2 ------------------------------------------------------------------

roll_s <- function(mat) {
    for(i in 1:ncol(mat)) {
        col <- mat[, i]
        
        w_o <- which(col == 'O')
        w_hash <- c(which(col == '#'), nrow(mat) + 1)
        
        stacks <- sapply(1:length(w_hash), function(j) {
            if(j == 1) {
                return(sum(w_o < w_hash[j]))
            } else {
                return(sum(w_o < w_hash[j] & w_o > w_hash[j - 1]))
            }
        })
        
        # Restack Os
        col[col != '#'] <- '.'
        for(j in 1:length(w_hash)) {
            if(stacks[j] > 0) {
                col[(w_hash[j]-stacks[j]):(w_hash[j]-1)] <- "O"
            }
        }
        
        mat[, i] <- col
    }
    
    return(mat)
}


roll_e <- function(mat) {
    for(i in 1:nrow(mat)) {
        row <- mat[i, ]
        
        w_o <- which(row == 'O')
        w_hash <- c(which(row == '#'), ncol(mat) + 1)
        
        stacks <- sapply(1:length(w_hash), function(j) {
            if(j == 1) {
                return(sum(w_o < w_hash[j]))
            } else {
                return(sum(w_o < w_hash[j] & w_o > w_hash[j - 1]))
            }
        })
        
        # Restack Os
        row[row != '#'] <- '.'
        for(j in 1:length(w_hash)) {
            if(stacks[j] > 0) {
                row[(w_hash[j]-stacks[j]):(w_hash[j]-1)] <- "O"
            }
        }
        
        mat[i, ] <- row
    }
    
    return(mat)
}


roll_w <- function(mat) {
    for(i in 1:nrow(mat)) {
        row <- mat[i, ]
        
        w_o <- which(row == 'O')
        w_hash <- c(0, which(row == '#'))
        
        stacks <- sapply(1:length(w_hash), function(j) {
            if(j == length(w_hash)) {
                return(sum(w_o > w_hash[j]))
            } else {
                return(sum(w_o > w_hash[j] & w_o < w_hash[j + 1]))
            }
        })
        
        # Restack Os
        row[row != '#'] <- '.'
        for(j in 1:length(w_hash)) {
            if(stacks[j] > 0) {
                row[(w_hash[j]+1):(w_hash[j]+stacks[j])] <- "O"
            }
        }
        
        mat[i, ] <- row
    }
    
    return(mat)
}


spin <- function(mat) {
    roll_e(roll_s(roll_w(roll_n(mat))))
}


cyc <- c()
mat_spin <- copy(mat)
for(n in 1:1000) {
    mat_spin <- spin(mat_spin)
    cyc <- c(cyc, count(mat_spin))
}


find_cycle_length <- function(cyc) {
    for(i in 1:(length(cyc) / 2)) {
        for(j in 1:(i-1)) {
            if(length(unique(cyc[seq(j, length(cyc), i)])) == 1) {
                return(i)
            }
        }
    }
    return(NA)
}

cyc_len <- find_cycle_length(cyc)

N <- 1000000000
cyc[N %% cyc_len + cyc_len]
