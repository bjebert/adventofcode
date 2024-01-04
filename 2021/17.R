input <- ".###..#.
##.##...
....#.#.
#..#.###
...#...#
##.#...#
#..##.##
#......."

# input <- ".#.
# ..#
# ###"

input_mat <- matrix(strsplit(gsub("\n", "", input), "")[[1]], ncol = 8, byrow = T)


# If active: needs 2 or 3 neighbours to be active, then -> active, else -> inactive
# If inactive: needs 3 neighbours to be active, then -> active, else -> inactive

create_matrices <- function(input_mat, steps = 6) {
    N <- nrow(input_mat) + steps * 2 + 3
    
    dimensional_mats <- lapply(1:(3+steps*2), function(x) matrix(".", nrow = N, ncol = N))
    
    init_l <- (N %/% 2 + 1 - nrow(input_mat) %/% 2)
    init_u <- (N %/% 2 + 1 + nrow(input_mat) %/% 2) - as.numeric(nrow(input_mat) %% 2 == 0)
    
    dimensional_mats[[steps+1]][init_l:init_u, init_l:init_u] <- input_mat
    
    return(dimensional_mats)
}

get_active_neighbours <- function(dimensional_mats, i, row, col) {
    
    N <- nrow(dimensional_mats[[i]])
    
    n_active <- 0
    
    for(z in max(1, i - 1):min(length(dimensional_mats), i + 1)) {
        for(ro in max(1, row - 1):min(N, row + 1)) {
            for(co in max(1, col - 1):min(N, col + 1)) {
                if(z == i && ro == row && co == col) {
                    next
                }
                
                n_active <- n_active + as.numeric(dimensional_mats[[z]][ro, co] == "#")
            }
        }
    }
    
    return(n_active)    
}

update_matrices <- function(dimensional_mats) {
    updated_mats <- copy(dimensional_mats)
    
    for(i in 1:length(dimensional_mats)) {
        for(row in 1:nrow(dimensional_mats[[i]])) {
            for(col in 1:ncol(dimensional_mats[[i]])) {
                active_neighbours <- get_active_neighbours(dimensional_mats, i, row, col)
                
                if(dimensional_mats[[i]][row, col] == "." && active_neighbours == 3) {
                    updated_mats[[i]][row, col] <- "#"
                } else if(dimensional_mats[[i]][row, col] == "#" && active_neighbours %in% 2:3) {
                    updated_mats[[i]][row, col] <- "#"
                } else {
                    updated_mats[[i]][row, col] <- "."
                }
            }
        }
    }
    
    return(updated_mats)
}

dimensional_mats <- create_matrices(input_mat)

for(i in 1:6) {
    dimensional_mats <- update_matrices(dimensional_mats)
}

sum(sapply(dimensional_mats, function(x) sum(x == "#")))
