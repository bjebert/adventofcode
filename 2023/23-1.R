rm(list=ls())
source("utilities.R")
inp <- get_input("2023/23", parse = F, deesblake = F, cache = T)

# inp <- strsplit("#.#####################
# #.......#########...###
# #######.#########.#.###
# ###.....#.>.>.###.#.###
# ###v#####.#v#.###.#.###
# ###.>...#.#.#.....#...#
# ###v###.#.#.#########.#
# ###...#.#.#.......#...#
# #####.#.#.#######.#.###
# #.....#.#.#.......#...#
# #.#####.#.#.#########v#
# #.#...#...#...###...>.#
# #.#.#v#######v###.###v#
# #...#.>.#...>.>.#.###.#
# #####v#.#.###v#.#.###.#
# #.....#...#...#.#.#...#
# #.#########.###.#.#.###
# #...###...#...#...#.###
# ###.###.#.###v#####v###
# #...#...#.#.>.>.#.>.###
# #.###.###.#.###.#.#v###
# #.....###...###...#...#
# #####################.#", "\n")[[1]]

mat <- inp2mat(inp)
map <- mat2map(mat)

# Part 2

map[map %in% c(">", "<", "^", "v")] <- "."

# Part 1 ------------------------------------------------------------------

get_neighbours <- function(coord) {
    pos <- coord2pos(coord)
    
    if(map[[coord]] == ">") {
        return(pos2coord(c(pos[1], pos[2] + 1)))
    } else if(map[[coord]] == "<") {
        return(pos2coord(c(pos[1], pos[2] - 1)))
    } else if(map[[coord]] == "^") {
        return(pos2coord(c(pos[1] - 1, pos[2])))
    } else if(map[[coord]] == "v") {
        return(pos2coord(c(pos[1] + 1, pos[2])))
    }
    
    coords <- list(c(pos[1] + 1, pos[2]),
                   c(pos[1] - 1, pos[2]),
                   c(pos[1], pos[2] + 1),
                   c(pos[1], pos[2] - 1))
    
    map_loc <- map[sapply(coords, pos2coord)]
    nb <- names(map_loc[map_loc %in% c('.', '^', 'v', '>', '<')])
    
    return(nb)
}


start <- paste(c(1, which(mat[1,] == ".")), collapse = ",")
goal <- paste(c(nrow(mat), which(mat[nrow(mat),] == ".")), collapse = ",")

Q <- list(list(coord = start,
               v = start))

longest <- 0
while(length(Q) >= 1) {
    curr <- Q[[1]]
    Q <- Q[-1]
    
    coord <- curr[["coord"]]
    if(coord == goal) {
        path_len <- length(curr[["v"]])
        
        if(path_len > longest) {
            print(path_len - 1)
            longest <- path_len
        }
    }
    
    nb <- get_neighbours(coord)
    nb <- nb[!(nb %in% curr[["v"]])]    
    
    if(length(nb) > 0) {
        Q <- c(Q, lapply(nb, function(n) list(coord = n, v = c(curr[["v"]], n))))
    }
}

# 12:53

# Part 2 ------------------------------------------------------------------


