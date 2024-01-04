rm(list=ls())
source("utilities.R")
inp <- get_input("2023/10", parse = F, deesblake = F, cache = T)

inp <- strsplit("..........
.S------7.
.|F----7|.
.||....||.
.||....||.
.|L-7F-J|.
.|..||..|.
.L--JL--J.
..........", "\n")[[1]]

inp2map <- function(inp) {
    mat <- matrix(strsplit(paste(inp, collapse = ""), "")[[1]], nrow = length(inp), ncol = nchar(inp[1]), byrow = T)
    map <- setNames(c(mat), sprintf("%s,%s", rep(1:nrow(mat), times = ncol(mat)), rep(1:ncol(mat), each = nrow(mat))))
    map
}

map2mat <- function(map, default = ".") {
    coords <- lapply(strsplit(names(map_m), ","), as.numeric)
    
    nr <- max(sapply(coords, function(x) x[1])) - min(sapply(coords, function(x) x[1])) + 1
    nc <- max(sapply(coords, function(x) x[2])) - min(sapply(coords, function(x) x[2])) + 1
    
    mat2 <- matrix(NA, nrow = nr, ncol = nc)
    
    for(i in 1:nr) {
        for(j in 1:nc) {
            nm <- sprintf("%s,%s", i, j)
            mat2[i, j] <- if(nm %in% names(map)) map[[nm]] else default
        }
    }
    mat2
}

xy <- function(coord) as.numeric(strsplit(coord, ",")[[1]])
mat <- matrix(strsplit(paste(inp, collapse = ""), "")[[1]], nrow = length(inp), ncol = nchar(inp[1]), byrow = T)
map <- inp2map(inp)


# each pipe in main loop has 2 neighbours
# find tile in loop farthest from start pos (longest number of steps)
# first: mark tiles in main loop



get_neighbours <- function(map, tile, coord, w = F) {
    con_N <- c("|", "L", "J", "S")
    con_E <- c("-", "L", "F", "S")
    con_S <- c("|", "7", "F", "S")
    con_W <- c("-", "J", "7", "S")
    
    if(tile == "|") {
        t1 <- sprintf("%s,%s", coord[1] - 1, coord[2])
        t2 <- sprintf("%s,%s", coord[1] + 1, coord[2])
        
        if(w == T) {
            return(c(t1, t2))
        }
        
        return(sum(c(map[t1] %in% con_S, map[t2] %in% con_N), na.rm = T))
        
    } else if(tile == "-") {
        t1 <- sprintf("%s,%s", coord[1], coord[2] - 1)
        t2 <- sprintf("%s,%s", coord[1], coord[2] + 1)
        
        if(w == T) {
            return(c(t1, t2))
        }
        
        return(sum(c(map[t1] %in% con_E, map[t2] %in% con_W), na.rm = T))
        
    } else if(tile == "L") {
        t1 <- sprintf("%s,%s", coord[1] - 1, coord[2])
        t2 <- sprintf("%s,%s", coord[1], coord[2] + 1)
        
        if(w == T) {
            return(c(t1, t2))
        }
        
        return(sum(c(map[t1] %in% con_S, map[t2] %in% con_W), na.rm = T))
        
    } else if(tile == "J") {
        t1 <- sprintf("%s,%s", coord[1] - 1, coord[2])
        t2 <- sprintf("%s,%s", coord[1], coord[2] - 1)
        
        if(w == T) {
            return(c(t1, t2))
        }
        
        return(sum(c(map[t1] %in% con_S, map[t2] %in% con_E), na.rm = T))
        
    } else if(tile == "7") {
        t1 <- sprintf("%s,%s", coord[1] + 1, coord[2])
        t2 <- sprintf("%s,%s", coord[1], coord[2] - 1)
        
        if(w == T) {
            return(c(t1, t2))
        }
        
        return(sum(c(map[t1] %in% con_N, map[t2] %in% con_E), na.rm = T))
        
    } else if(tile == "F") {
        t1 <- sprintf("%s,%s", coord[1] + 1, coord[2])
        t2 <- sprintf("%s,%s", coord[1], coord[2] + 1)
        
        if(w == T) {
            return(c(t1, t2))
        }
        
        return(sum(c(map[t1] %in% con_N, map[t2] %in% con_W), na.rm = T))
        
    } else if(tile == "S") {
        return(-1)
    }
}

# Try and determine main loop
map_m <- copy(map)
start_len <- length(map)

pipes <- map_m[map_m != "."]
nb <- setNames(sapply(1:length(pipes), function(i) get_neighbours(map_m, pipes[i], xy(names(pipes[i])))), names(pipes))
nb <- nb[nb %in% c(-1, 2)]

while(TRUE) {
    start_len <- length(nb)
    map_m <- map_m[names(map_m) %in% names(nb)]
    pipes <- map_m[map_m != "."]
    nb <- setNames(sapply(1:length(pipes), function(i) get_neighbours(map_m, pipes[i], xy(names(pipes[i])))), names(pipes))
    nb <- nb[nb %in% c(-1, 2)]
    
    if(length(nb) == start_len) {
        break
    }
}

# Main loop established (= map_m)

# Find potential values for starting coord

res <- sapply(c("|", "-", "L", "J", "7", "F"), function(tile) get_neighbours(map_m, tile, xy(names(map)[map == "S"])))
stile <- names(which(res == 2))

map_m[map_m == "S"] <- stile
map[map == "S"] <- stile


# Part 2 ------------------------------------------------------------------

# Check all tiles that aren't part of map_m and see if they can reach an exit?
# Mark each tile as OUT from outwards to inwards, then if a tile is adjacent to outwards, it is not part of IN

to_check <- setdiff(names(map), names(map_m))
status <- setNames(rep("?", length(to_check)), to_check)

dist_edge <- function(coord) {
    min(abs(c(nrow(mat) - coord[1],
              coord[1] - 1,
              ncol(mat) - coord[2],
              coord[2] - 1)))
}

from_edge <- sapply(names(status), function(x) dist_edge(xy(x)))
status <- status[order(status, from_edge)]

curr_sum <- 0

while(sum(status == "?") != curr_sum) {
    curr_sum <- sum(status == "?")
    for(i in 1:length(status)) {
        coord <- xy(names(status)[i])
        
        if(dist_edge(coord) == 0) {
            status[i] <- "O"
            next
        }
        
        adj <- c(sprintf("%s,%s", coord[1] - 1, coord[2]),
                 sprintf("%s,%s", coord[1] + 1, coord[2]),
                 sprintf("%s,%s", coord[1], coord[2] - 1),
                 sprintf("%s,%s", coord[1], coord[2] + 1))
        
        if(any(status[adj] == "O", na.rm = T)) {
            status[i] <- "O"
        }
    }    
}


# Now, these tiles are not all guaranteed to be inside, some may be able to escape through intersections of pipes
rem <- status[status == "?"]


# vertical
vert_int <- c(sapply(1:ncol(mat), function(i) sprintf("%s,%s-%s,%s", 1:(nrow(mat)-1), i, 2:nrow(mat), i)))

vert <- setNames(sapply(vert_int, function(x) {
    coords <- strsplit(x, "-")[[1]]
    t1 <- map[coords[1]]
    t2 <- map[coords[2]]
    
    return(!(t1 %in% c("|", "7", "F") && t2 %in% c("|", "L", "J")))
}), vert_int)

# horizontal
horz_int <- c(sapply(1:nrow(mat), function(i) sprintf("%s,%s-%s,%s", i, 1:(ncol(mat)-1), i, 2:ncol(mat))))

horz <- setNames(sapply(horz_int, function(x) {
    coords <- strsplit(x, "-")[[1]]
    t1 <- map[coords[1]]
    t2 <- map[coords[2]]
    
    return(!(t1 %in% c("-", "L", "F") && t2 %in% c("-", "J", "7")))
}), horz_int)

horz["6,4-6,5"]


# Part 1 ------------------------------------------------------------------


# Perform a BFS to find furthest tile

curr <- names(map)[map == "S"]
vis <- setNames(0, curr)
Q <- c(curr)

while(length(Q) > 0) {
    curr <- Q[1]
    if(length(Q) > 1) {
        Q <- Q[2:length(Q)]
    } else {
        Q <- c()
    }
    
    nb <- get_neighbours(map_m, map_m[curr], xy(curr), w = T)
    
    for(n in nb) {
        if(!n %in% names(vis)) {
            vis[[n]] <- vis[[curr]] + 1
            Q <- c(Q, n)
        }
    }
}

max(vis)



