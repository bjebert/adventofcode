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

get_neighbours <- function(coord) {
    pos <- coord2pos(coord)
    
    coords <- list(c(pos[1] + 1, pos[2]),
                   c(pos[1] - 1, pos[2]),
                   c(pos[1], pos[2] + 1),
                   c(pos[1], pos[2] - 1))
    
    map_loc <- map[sapply(coords, pos2coord)]
    nb <- names(map_loc[map_loc %in% c('.', '^', 'v', '>', '<')])
    
    return(nb)
}


# Part 2 ------------------------------------------------------------------
# Notice that all intersections contain slopes.  Instead of trecking each path,
# we can just mark the distance between intersections on our map, to make the
# overall graph much shorter.

slopes <- names(map[map %in% c(">", "<", "^", "v")])

# Need to set the nodes as being the tiles in the middle of the intersection.
# There will always be 3-4 slopes surrounding a node.
find_nodes <- function(slopes) {
    nodes <- c()
    while(length(slopes) > 0) {
        slope <- slopes[1]
        pos <- coord2pos(slope)
        
        potential <- get_4nb(pos)
        
        for(p in potential) {
            p_coord <- sapply(get_4nb(p), pos2coord)
            w_slopes <- p_coord %in% slopes
            if(sum(w_slopes) >= 3) {
                nodes <- c(nodes, pos2coord(p))
                slopes <- slopes[!(slopes %in% p_coord[w_slopes])]
                break                
            }
        }
    }
    return(nodes)
}


start <- paste(c(1, which(mat[1,] == ".")), collapse = ",")
goal <- paste(c(nrow(mat), which(mat[nrow(mat),] == ".")), collapse = ",")
nodes <- c(start, find_nodes(slopes), goal)

# Create graph showing connections between nodes

bfs_graph <- function(node) {
    Q <- list(list(coord = node,
                   v = node))
    
    paths <- c()
    
    while(length(Q) > 0) {
        curr <- Q[[1]]
        Q <- Q[-1]
        
        coord <- curr[["coord"]]
        
        if(coord %in% nodes && coord != node) {
            paths[[coord]] <- length(curr[["v"]]) - 1
            next
        }
        
        nb <- get_neighbours(coord)
        nb <- nb[!(nb %in% curr[["v"]])]     
        
        if(length(nb) > 0) {
            Q <- c(Q, lapply(nb, function(n) list(coord = n, v = c(curr[["v"]], n))))
        }
    }
    
    return(paths)
}

graph <- setNames(lapply(nodes, bfs_graph), nodes)

Q <- list(list(coord = start,
               v = start,
               length = 0))

longest <- 0

while(length(Q) >= 1) {
    curr <- Q[[1]]
    Q <- Q[-1]
    
    coord <- curr[["coord"]]
    
    if(coord == goal) {
        path_len <- curr[["length"]]
        
        if(path_len > longest) {
            print(path_len)
            longest <- path_len
            
            print(curr[["v"]])
        }
    }
    
    nb <- graph[[coord]]
    nb <- nb[!(names(nb) %in% curr[["v"]])]    
    
    if(length(nb) > 0) {
        Q <- c(lapply(1:length(nb), function(i) list(coord = names(nb)[i], 
                                                        v = c(curr[["v"]], names(nb)[i]),
                                                        length = curr[["length"]] + nb[[i]])), Q)
        
        if(length(Q) %% 100 == 0) {
            Q <- Q[order(-sapply(Q, function(x) x[["length"]]))]
            print(length(Q))
        }
    }
}
