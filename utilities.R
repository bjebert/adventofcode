<<<<<<< HEAD
get_input <- function(aoc_id, parse = T, deesblake = F, cache = F) {
    f <- sprintf("2023/%s.txt", gsub("/", "-", aoc_id))
    if(cache && file.exists(f)) {
        return(readLines(f))
    }
    
    library(httr)
    
    aoc_split <- as.numeric(strsplit(aoc_id, "/")[[1]])
    address <- sprintf("https://adventofcode.com/%s/day/%s/input", aoc_split[1], aoc_split[2])
    sessions <- readLines("session.txt")
    
    if(deesblake) {
        cookie <- c(`session` = sessions[1])
    } else {
        cookie <- c(`session` = sessions[2])
    }
    
    res <- content(GET(address, set_cookies(cookie)), encoding = 'UTF-8')
    lines <- strsplit(res, "\n")[[1]]
    
    writeLines(lines, f)
    
    if(!parse) {
        return(lines)
    }
    
    # Auto-parse input
    lines_num <- suppressWarnings(as.numeric(lines))
    if(all(!is.na(lines_num))) {
        return(lines_num)
    }
    
    nc <- unique(sapply(lines, nchar))
    if(length(nc) == 1 && length(lines) > 1) {
        m <- matrix(data = strsplit(paste(lines, collapse = ""), "")[[1]], nrow = length(lines), ncol = nc, byrow = T)
        return(m)
    } else {
        return(lines)
    }
}


inp2mat <- function(inp) {
    matrix(strsplit(paste(inp, collapse = ""), "")[[1]], nrow = length(inp), ncol = nchar(inp[1]), byrow = T)
}


mat2map <- function(mat) {
    setNames(c(mat), sprintf("%s,%s", rep(1:nrow(mat), times = ncol(mat)), rep(1:ncol(mat), each = nrow(mat))))
}


map2mat <- function(map, default = ".") {
    coords <- lapply(strsplit(names(map), ","), as.numeric)
    
    min_x <- min(sapply(coords, function(x) x[2]))
    max_x <- max(sapply(coords, function(x) x[2]))
    
    min_y <- min(sapply(coords, function(x) x[1]))
    max_y <- max(sapply(coords, function(x) x[1]))
    
    nr <- max_y - min_y + 1
    nc <- max_x - min_x + 1
    
    mat2 <- matrix(NA, nrow = nr, ncol = nc)
    
    for(i in 1:nr) {
        for(j in 1:nc) {
            nm <- sprintf("%s,%s", i+min_y-1, j+min_x-1)
            mat2[i, j] <- if(nm %in% names(map)) map[[nm]] else default
        }
    }
    
    mat2
}


get_4nb <- function(pos) {
    list(c(pos[1] + 1, pos[2]),
         c(pos[1] - 1, pos[2]),
         c(pos[1], pos[2] + 1),
         c(pos[1], pos[2] - 1))
=======
source("utilities/data.R")

# Helpful libraries

library(plot.matrix)
library(combinat)     # t(combinat::combn(1:10, 3))
library(gtools)       # gtools::permutations(v = letters[1:10], n = 10, r = 3)
library(openssl)      # openssl::md5


# Matrix ------------------------------------------------------------------

rotate <- function(x) t(apply(x, 2, rev)) 

# Grid --------------------------------------------------------------------

move_map <- list(`^` = c(1, 0), N = c(1, 0), U = c(1, 0), 
                 `>` = c(0, 1), E = c(0, 1), R = c(0, 1), 
                 `v` = c(-1, 0), S = c(-1, 0), D = c(-1, 0), 
                 `<` = c(0, -1), W = c(0, -1), L = c(0, -1))

get_4nb <- function(pos) {
    list(c(pos[1] + 1, pos[2]),  # North
         c(pos[1] - 1, pos[2]),  # South
         c(pos[1], pos[2] + 1),  # East
         c(pos[1], pos[2] - 1))  # West
>>>>>>> 57859467950c2ac3b49a2163344b572250ecf2a7
}

get_8nb <- function(pos) {
    list(
        c(pos[1] + 1, pos[2]),     # North
        c(pos[1] - 1, pos[2]),     # South
        c(pos[1], pos[2] + 1),     # East
        c(pos[1], pos[2] - 1),     # West
        c(pos[1] + 1, pos[2] + 1), # Northeast
        c(pos[1] + 1, pos[2] - 1), # Northwest
        c(pos[1] - 1, pos[2] + 1), # Southeast
        c(pos[1] - 1, pos[2] - 1)  # Southwest
    )
}

<<<<<<< HEAD

factors <- function(n) {
    n <- abs(n)
    f1 <- (1:floor(sqrt(n)))[sapply(1:floor(sqrt(n)), function(i) n %% i == 0)]
    unique(sort(c(f1, n / f1)))
}

coord2pos <- function(coord) as.numeric(c(strsplit(coord, ",")[[1]][1], strsplit(coord, ",")[[1]][2]))
pos2coord <- function(pos) sprintf("%s,%s", pos[1], pos[2])

library(plot.matrix)
rotate <- function(x) t(apply(x, 2, rev))
=======
coord2pos <- function(coord) as.numeric(strsplit(coord, ",", fixed = TRUE)[[1]])
pos2coord <- function(pos) paste0(pos, collapse = ",")

get_4nb_coord <- function(coord) {
    sapply(get_4nb(coord2pos(coord)), pos2coord)
}

get_8nb_coord <- function(coord) {  # slow implementation
    sapply(get_8nb(coord2pos(coord)), pos2coord)
}

get_4nb_coord_fast <- function(coord) {  # fast implementation
    pos <- coord2pos(coord)
    return(sprintf("%d,%d",
        c(pos[1] + 1, pos[1] - 1, pos[1], pos[1]),
        c(pos[2], pos[2], pos[2] + 1, pos[2] - 1)
    ))
}

get_8nb_coord_fast <- function(coord) {  # fast implementation
    pos <- coord2pos(coord)
    return(sprintf("%d,%d",
        c(pos[1] + 1, pos[1] - 1, pos[1], pos[1], pos[1] + 1, pos[1] + 1, pos[1] - 1, pos[1] - 1),
        c(pos[2], pos[2], pos[2] + 1, pos[2] - 1, pos[2] + 1, pos[2] - 1, pos[2] + 1, pos[2] - 1)
    ))
}
>>>>>>> 57859467950c2ac3b49a2163344b572250ecf2a7
