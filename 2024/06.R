
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...", "\n")[[1]]  # Example
inp <- get_input("2024/06", parse = F, user = "bjebert", cache = F)


mat <- inp2mat(inp)
map <- mat2map(inp2mat(inp))

start <- names(map)[which(map == "^")]
pos <- start

facing <- "N"
spots <- list()
iter <- 0

while(pos %in% names(map)) {
    spots[[length(spots) + 1]] <- list(pos = pos, facing = facing)
    
    p <- coord2pos(pos)
    pnew <- p + move_map[[facing]]
    
    ppos <- pos2coord(pnew)
    if(!(ppos %in% names(map))) {
        break
    }
    
    turnmap <- c("N" = "E", "E" = "S", "S" = "W", "W" = "N")
    if(map[[ppos]] == '#') {
        facing <- turnmap[[facing]]    
    } else {
        pos <- copy(ppos)
    }
    
    iter <- iter + 1
}

spots <- spots[!duplicated(sapply(spots, function(x) x[["pos"]]))]
length(spots)


# part 2 ------------------------------------------------------------------

# for each spot, test replacing with an obstacle, and see if we get into a loop.
# how to check for loop?

walls <- names(map)[map == '#']


get_wall <- function(p, dir, walls) {
    if(dir == "U") {  # look for right wall
        x <- p[1] + 1
        y <- (p[2] + 1):ncol(mat)
    } else if(dir == "D") {  # look for left wall
        x <- p[1] - 1
        y <- (p[2] - 1):1
    } else if(dir == "L") {  # look for up wall
        x <- (p[1] - 1):1
        y <- p[2] + 1
    } else if(dir == "R") {  # look for down wall
        x <- (p[1] + 1):nrow(mat)
        y <- p[2] - 1
    }
    
    z <- sprintf("%s,%s", x, y)
    
    w <- which(z %in% walls)
    
    if(length(w) == 0) {
        return(NULL)
    }
    return(z[w[1]])
}



z <- sapply(spots, function(s) {
    turnmap <- c("U" = "R", "R" = "D", "D" = "L", "L" = "U")
    facemap <- c("N" = "U", "E" = "R", "S" = "D", "W" = "L")
    
    if(s[["pos"]] == start) {
        return(FALSE)
    }
    
    w <- coord2pos(s[["pos"]])
    facing <- facemap[[s[["facing"]]]]
    
    tmp_walls <- copy(walls)
    tmp_walls <- c(walls, s[["pos"]])
    
    v <- sprintf("%s|%s", s[["pos"]], facing)
    
    while(TRUE) {
        w <- get_wall(w, facing, tmp_walls)
        
        if(is.null(w)) {
            return(FALSE)
        }
        
        facing <- turnmap[[facing]]
        id <- sprintf("%s|%s", w, facing)
        
        if(id %in% v) {
            return(TRUE)
        } else {
            v <- c(v, id)
        }
        
        w <- coord2pos(w)
        v <- c(v, w)
        
    }
})

sum(z)
