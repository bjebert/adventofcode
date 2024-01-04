rm(list=ls())
source("utilities.R")
inp <- get_input("2023/11", parse = F, deesblake = F, cache = T)

inp <- strsplit("...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....", "\n")[[1]]

mat <- inp2mat(inp)
map <- mat2map(mat)

galaxies <- names(map)[map == '#']

row_idx <- which(sapply(1:nrow(mat), function(i) {
    all(mat[i,] == ".")
}))

col_idx <- which(sapply(1:ncol(mat), function(i) {
    all(mat[,i] == ".")
}))

# Part 1 ------------------------------------------------------------------

exp_factor <- 2

gx_pos <- lapply(galaxies, coord2pos)

for(i in 1:length(gx_pos)) {
    row_exp <- sum(row_idx < gx_pos[[i]][1]) * (exp_factor - 1)
    col_exp <- sum(col_idx < gx_pos[[i]][2]) * (exp_factor - 1)
    
    gx_pos[[i]] <- c(gx_pos[[i]][1] + row_exp, gx_pos[[i]][2] + col_exp)
}

sum(unlist(sapply(1:(length(gx_pos) - 1), function(i) {
    sapply((i+1):length(gx_pos), function(j) {
        sum(abs(gx_pos[[i]] - gx_pos[[j]]))
    })
})))



# Part 2 ------------------------------------------------------------------

exp_factor <- 1e6

gx_pos <- lapply(galaxies, coord2pos)

for(i in 1:length(gx_pos)) {
    row_exp <- sum(row_idx < gx_pos[[i]][1]) * (exp_factor - 1)
    col_exp <- sum(col_idx < gx_pos[[i]][2]) * (exp_factor - 1)
    
    gx_pos[[i]] <- c(gx_pos[[i]][1] + row_exp, gx_pos[[i]][2] + col_exp)
}


sum(unlist(sapply(1:(length(gx_pos) - 1), function(i) {
    sapply((i+1):length(gx_pos), function(j) {
        sum(abs(gx_pos[[i]] - gx_pos[[j]]))
    })
})))

