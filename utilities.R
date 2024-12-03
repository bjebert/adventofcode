source("utilities/data.R")

# Helpful libraries

library(plot.matrix)
library(combinat)
library(openssl)  # md5 hashes


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
