rm(list=ls())
source("utilities.R")
inp <- get_input("2023/18", parse = F, deesblake = F, cache = T)



inp <- strsplit("R 2 (#70c710)
D 2 (#000000)
R 1 (#000000)
D 1 (#000000)
R 2 (#000000)
U 2 (#000000)
R 1 (#000000)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)", "\n")[[1]]

inp <- strsplit("R 3 (#70c710)
D 2 (#000000)
R 3 (#000000)
U 1 (#000000)
R 1 (#000000)
D 1 (#000000)
R 3 (#000000)
D 3 (#000000)
L 10 (#000000)
U 4 (#000000)", "\n")[[1]]

dirs <- substr(inp, 1, 1)
amts <- as.numeric(sapply(strsplit(inp, " "), function(x) x[2]))
cols <- gsub("\\(|\\)", "", sapply(strsplit(inp, " "), function(x) x[3]))


# Part 2 ------------------------------------------------------------------

dir_map <- list("N" = c(-1, 0), "S" = c(1, 0), "E" = c(0, 1), "W" = c(0, -1))
compass_map <- c(`U` = "N", `R` = "E", `D` = "S", `L` = "W")

# dirs <- unname(c(`0` = 'R', `1` = 'D', `2` = 'L', `3` = 'U')[substr(cols, nchar(cols), nchar(cols))])
# amts <- sapply(substr(cols, 2, nchar(cols) - 1), function(x) strtoi(x, base = 16L), USE.NAMES = F)

# Need to create boxes, row-by-row

walls <- list()
curr <- c(1, 1)

for(i in 1:length(dirs)) {
    new_loc <- curr + dir_map[[compass_map[dirs[i]]]] * amts[i]
    walls[[length(walls) + 1]] <- list(curr, new_loc)
    
    curr <- new_loc
}

# Start with full grid and see which squares aren't part of it?

min_x <- min(sapply(walls, function(x) min(c(x[[1]][2], x[[2]][2]))))
max_x <- max(sapply(walls, function(x) max(c(x[[1]][2], x[[2]][2]))))
min_y <- min(sapply(walls, function(x) min(c(x[[1]][1], x[[2]][1]))))
max_y <- max(sapply(walls, function(x) max(c(x[[1]][1], x[[2]][1]))))

full_area <- (max_y - min_y + 1) * (max_x - min_x + 1)

walls_n <- walls[sapply(walls, function(x) x[[1]][1] == min_y && x[[2]][1] == min_y)]
walls_e <- walls[sapply(walls, function(x) x[[1]][2] == max_x && x[[2]][2] == max_x)]
walls_s <- walls[sapply(walls, function(x) x[[1]][1] == max_y && x[[2]][1] == max_y)]
walls_w <- walls[sapply(walls, function(x) x[[1]][2] == min_x && x[[2]][2] == min_x)]

w_n <- sapply(walls_n, function(y) which(sapply(walls, function(x) identical(x, y))))
w_e <- sapply(walls_e, function(y) which(sapply(walls, function(x) identical(x, y))))
w_s <- sapply(walls_s, function(y) which(sapply(walls, function(x) identical(x, y))))
w_w <- sapply(walls_w, function(y) which(sapply(walls, function(x) identical(x, y))))

move_n <- function() {

    sum_outside <- 0
        
    # Find northernmost western wall
    ww_y <- sapply(walls_w, function(x) min(c(x[[1]][1], x[[2]][1])))
    northernmost_w <- walls_w[[which(ww_y == min(ww_y))]]
    
    # Find northernmost eastern wall
    we_y <- sapply(walls_e, function(x) min(c(x[[1]][1], x[[2]][1])))
    northernmost_e <- walls_e[[which(we_y == min(we_y))]]
    
    # Move between these two northernmost walls to build our northern edge
    i_w <- which(sapply(walls, function(x) identical(x, northernmost_w)))
    i_e <- which(sapply(walls, function(x) identical(x, northernmost_e)))
    
    start <- min(i_w, i_e)
    end <- max(i_w, i_e)
    
    i <- start + 1
    
    while(i < end) {
        walls[[i]]
        
        dist_edge <- min(walls[[i]][[1]][1], walls[[i]][[2]][1]) - min_y
        wall_length <- abs(walls[[i]][[2]][2] - walls[[i]][[1]][2]) 
        
        # if last wall was south, subtract 1
        if(walls[[i-1]][[1]][1] < walls[[i-1]][[2]][1]) {
            wall_length <- wall_length - 1
        }
        
        # if next wall is south, add 1
        if(walls[[i+1]][[1]][1] < walls[[i+1]][[2]][1]) {
            wall_length <- wall_length + 1
        }
        
        print(sprintf("i: %s / Dist: %s / Length: %s / Box size: %s", i, dist_edge, wall_length, dist_edge * wall_length))
        sum_outside <- sum_outside + dist_edge * wall_length
        
        if(walls[[i]][[2]][2] %in% c(min_x, max_x)) {
            if(walls[[i]][[2]][2] == min_x) print("west edge") else print("east edge")
            return(list(i = i+1, sum_outside = sum_outside))
        }
        
        i <- i + 2
    }
}


move_n(1)

# EAST
while(TRUE) {
    dist_edge <- max_x - max(walls[[i]][[1]][2], walls[[i]][[2]][2]) 
    wall_length <- abs(walls[[i]][[2]][1] - walls[[i]][[1]][1])
    
    # if last wall was west, subtract 1
    if(i != start) {
        if(walls[[i-1]][[1]][2] > walls[[i-1]][[2]][2]) {  
            wall_length <- wall_length - 1        
        }
    }
    
    # if next wall is west, add 1
    if(walls[[i+1]][[1]][2] > walls[[i+1]][[2]][2]) {  
        wall_length <- wall_length + 1        
    }
    
    print(sprintf("i: %s / Dist: %s / Length: %s / Box size: %s", i, dist_edge, wall_length, dist_edge * wall_length))
    miss <- c(miss, dist_edge * wall_length)
    
    if(walls[[i]][[2]][1] %in% c(min_y, max_y)) {
        if(walls[[i]][[2]][1] == min_y) print("north edge") else print("south edge")
        break
    }
    
    i <- i + 2
}

i <- i + 1
start <- i


# SOUTH
while(TRUE) {
    dist_edge <- max_y - max(walls[[i]][[1]][1], walls[[i]][[2]][1])
    wall_length <- abs(walls[[i]][[2]][2] - walls[[i]][[1]][2])
    
    # if last wall was north, subtract 1
    if(i != start) {
        if(walls[[i-1]][[1]][1] > walls[[i-1]][[2]][1]) {
            wall_length <- wall_length - 1
        }
    }
    
    # if next wall is north, add 1
    if(walls[[i+1]][[1]][1] > walls[[i+1]][[2]][1]) {
        wall_length <- wall_length + 1
    }
    
    print(sprintf("i: %s / Dist: %s / Length: %s / Box size: %s", i, dist_edge, wall_length, dist_edge * wall_length))
    miss <- c(miss, dist_edge * wall_length)
    
    if(walls[[i]][[2]][2] %in% c(min_x, max_x)) {
        if(walls[[i]][[2]][2] == min_x) print("west edge") else print("east edge")
    }
    
    i <- i + 2
}

i <- i + 1
start <- i

# WEST
while(TRUE) {
    dist_edge <- max(walls[[i]][[1]][2], walls[[i]][[2]][2]) - min_x
    wall_length <- abs(walls[[i]][[2]][1] - walls[[i]][[1]][1])
    
    # if last wall was east, subtract 1
    if(i != start) {
        if(walls[[i-1]][[1]][2] < walls[[i-1]][[2]][2]) {  
            wall_length <- wall_length - 1        
        }
    }
    
    # if next wall is east, add 1
    if(i < length(walls)) {
        if(walls[[i+1]][[1]][2] < walls[[i+1]][[2]][2]) {  
            wall_length <- wall_length + 1        
        }
    }
    
    print(sprintf("i: %s / Dist: %s / Length: %s / Box size: %s", i, dist_edge, wall_length, dist_edge * wall_length))
    miss <- c(miss, dist_edge * wall_length)
    
    if(walls[[i]][[2]][1] %in% c(min_y, max_y)) {
        if(walls[[i]][[2]][1] == min_y) print("north edge") else print("south edge")
        break
    }
    
    i <- i + 2
}

full_area - sum(miss)


# Part 1 ------------------------------------------------------------------



curr <- "1,1"
map <- c()

for(i in 1:length(inp)) {
    dir <- dirs[i]
    amt <- amts[i]
    
    xy <- coord2pos(curr)
    
    if(dir == "R") {
        for(tile in sprintf("%s,%s", xy[1], xy[2]:(xy[2] + amt))) {
            map[[tile]] <- "#"
        }
        
        xy[2] <- xy[2] + amt   
    } else if(dir == "L") {
        for(tile in sprintf("%s,%s", xy[1], xy[2]:(xy[2] - amt))) {
            map[[tile]] <- "#"
        }
        
        xy[2] <- xy[2] - amt   
    } else if(dir == "U") {
        for(tile in sprintf("%s,%s", xy[1]:(xy[1] - amt), xy[2])) {
            map[[tile]] <- "#"
        }
        
        xy[1] <- xy[1] - amt   
    } else if(dir == "D") {
        for(tile in sprintf("%s,%s", xy[1]:(xy[1] + amt), xy[2])) {
            map[[tile]] <- "#"
        }
        
        xy[1] <- xy[1] + amt   
    }
    
    curr <- sprintf("%s,%s", xy[1], xy[2])
}

mat <- map2mat(map)

# Fill internal tiles with #

dist_from_edge <- function(coord) {
    xy <- coord2pos(coord)
    min(abs(c(xy[1] - 1, nrow(mat) - xy[1],
              xy[2] - 1, ncol(mat) - xy[2])))
}

map2 <- setNames(c(mat), c(sapply(1:ncol(mat), function(i) sapply(1:nrow(mat), function(j) sprintf("%s,%s", j, i)))))
to_check <- names(map2[map2 == "."])


can_escape <- function(tile) {
    Q <- tile
    vis <- c()
    
    while(length(Q) > 0) {
        tile <- Q[1]
        vis <- c(vis, tile)
        Q <- Q[-1]
        
        if(dist_from_edge(tile) == 0) {
            to_check <<- setdiff(to_check, vis)
            return(TRUE)
        }
        
        xy <- coord2pos(tile)
        
        adj_map <- list(`N` = c(xy[1] - 1, xy[2]),
                        `S` = c(xy[1] + 1, xy[2]),
                        `W` = c(xy[1], xy[2] - 1),
                        `E` = c(xy[1], xy[2] + 1))
        
        adj_coords <- sapply(adj_map, function(x) sprintf("%s,%s", x[1], x[2]))  
        
        adj2 <- map2[adj_coords]
        adj_coords <- names(adj2)[adj2 == "."]
        
        Q <- setdiff(unique(c(Q, adj_coords)), vis)
    }
    
    map2[vis] <<- "#"
    to_check <<- setdiff(to_check, vis)
    return(FALSE)
}

while(length(to_check) > 0) {
    print(length(to_check))
    tile <- to_check[1]
    can_escape(tile)
}

sum(map2 == "#")

mat2 <- map2mat(map2)
mat[mat == "#"] <- 1
mat2[mat2 == "#"] <- 1

fwrite(mat, "D:/projects/aoc/2023/18-input.csv", col.names = F)
fwrite(mat2, "D:/projects/aoc/2023/18-input.csv", col.names = F)
