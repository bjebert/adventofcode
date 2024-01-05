rm(list=ls())
source("utilities.R")
inp <- get_input("2023/21", parse = F, deesblake = F, cache = T)

inp <- strsplit("...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........", "\n")[[1]]

mat <- inp2mat(inp)
map <- mat2map(mat) 


# Part 1 ------------------------------------------------------------------

Q <- names(map)[map == "S"]

get_neighbours <- function(curr) {
    pos <- coord2pos(curr)
    
    nb <- list(c(pos[1] + 1, pos[2]),
               c(pos[1] - 1, pos[2]),
               c(pos[1], pos[2] + 1),
               c(pos[1], pos[2] - 1))
    
    possible <- map[sapply(nb, pos2coord)]
    possible <- possible[possible %in% c('.', 'S')]
    
    return(names(possible))
}

for(steps in 1:64) {
    nb <- unlist(lapply(Q, function(x) get_neighbours(x)))
    Q <- unique(nb)
}

print(length(Q))

# 13:26
