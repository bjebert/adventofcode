input <- readLines("20.txt")
# 
# input <- strsplit("Tile 2311:
# ..##.#..#.
# ##..#.....
# #...##..#.
# ####.#...#
# ##.##.###.
# ##...#.###
# .#.#.#..##
# ..#....#..
# ###...#.#.
# ..###..###
# 
# Tile 1951:
# #.##...##.
# #.####...#
# .....#..##
# #...######
# .##.#....#
# .###.#####
# ###.##.##.
# .###....#.
# ..#.#..#.#
# #...##.#..
# 
# Tile 1171:
# ####...##.
# #..##.#..#
# ##.#..#.#.
# .###.####.
# ..###.####
# .##....##.
# .#...####.
# #.##.####.
# ####..#...
# .....##...
# 
# Tile 1427:
# ###.##.#..
# .#..#.##..
# .#.##.#..#
# #.#.#.##.#
# ....#...##
# ...##..##.
# ...#.#####
# .#.####.#.
# ..#..###.#
# ..##.#..#.
# 
# Tile 1489:
# ##.#.#....
# ..##...#..
# .##..##...
# ..#...#...
# #####...#.
# #..#.#.#.#
# ...#.#.#..
# ##.#...##.
# ..##.##.##
# ###.##.#..
# 
# Tile 2473:
# #....####.
# #..#.##...
# #.##..#...
# ######.#.#
# .#...#.#.#
# .#########
# .###.#..#.
# ########.#
# ##...##.#.
# ..###.#.#.
# 
# Tile 2971:
# ..#.#....#
# #...###...
# #.#.###...
# ##.##..#..
# .#####..##
# .#..####.#
# #..#.#..#.
# ..####.###
# ..#.#.###.
# ...#.#.#.#
# 
# Tile 2729:
# ...#.#.#.#
# ####.#....
# ..#.#.....
# ....#..#.#
# .##..##.#.
# .#.####...
# ####.#.#..
# ##.####...
# ##..#.##..
# #.##...##.
# 
# Tile 3079:
# #.#.#####.
# .#..######
# ..#.......
# ######....
# ####.#..#.
# .#...#.##.
# #.#####.##
# ..#.###...
# ..#.......
# ..#.###...", "\n")[[1]]

input <- input[input != ""]

tile_ids <- input[seq(1, length(input), 11)]
input_text <- sapply(seq(2, length(input), 11), function(x) input[x:(x+9)])

input_mats <- lapply(1:ncol(input_text), function(x) t(sapply(strsplit(input_text[, x], ""), function(x) x)))
names(input_mats) <- sapply(strsplit(tile_ids, " "), function(x) gsub(":", "", x[2]))

rotate <- function(x) t(apply(x, 2, rev))

get_matrix_transformations <- function(mat) {
    return(list(mat, rotate(mat), rotate(rotate(mat)), rotate(rotate(rotate(mat))),
                t(mat), rotate(t(mat)), rotate(rotate(t(mat))), rotate(rotate(rotate(t(mat))))))
}

#' Apply all transformations and add to the list now

transform_mats <- lapply(input_mats, get_matrix_transformations)

#' Return a vector of compass directions:
#' N if mat_j could be bordered to the north of mat_i,
#' E if mat_j could be bordered to the east, etc.
get_borders <- function(mat_i, mat_j) {
    N <- nrow(mat_i)
    compass <- c()
    
    if(all(mat_i[1,] == mat_j[N,])) {
        compass <- c(compass, "N")
    }
    
    if(all(mat_i[N,] == mat_j[1,])) {
        compass <- c(compass, "S")
    }
    
    if(all(mat_i[,N] == mat_j[,1])) {
        compass <- c(compass, "E")
    }
    
    if(all(mat_i[,1] == mat_j[,N])) {
        compass <- c(compass, "W")
    }
    
    return(compass)
}

border_map <- list()

for(i in 1:length(transform_mats)) {
    tile_id <- names(transform_mats)[i]
    
    border_map[[tile_id]] <- lapply(1:4, function(x) NULL)
    names(border_map[[tile_id]]) <- c("N", "E", "S", "W")
    
    for(j in 1:length(transform_mats)) {
        if(i != j) {
            
            tile_other_id <- names(transform_mats)[j]            
            
            for(t_i in 1:8) {
                for(t_j in 1:8) {
                    mat_i <- transform_mats[[i]][[t_i]]
                    mat_j <- transform_mats[[j]][[t_j]]
                    
                    borders <- get_borders(mat_i, mat_j)
                    
                    for(dir in borders) {
                        border_map[[tile_id]][[dir]] <- c(border_map[[tile_id]][[dir]], sprintf("%s-%s, %s-%s", tile_id, t_i, tile_other_id, t_j))
                    }
                }
            }
            
        }
    }
}

prod(as.numeric(names(border_map)[sapply(border_map, function(x) length(x[["N"]])) == 4]))

border_map[sapply(border_map, function(x) length(x[["N"]])) == 4]


# Part 2 ------------------------------------------------------------------

#' I suspect that although there are multiple choices, taking any will work as we can re-transform
#' later to find the sea monsters.  Let's build our image!

corners <- border_map[sapply(border_map, function(x) length(x[["N"]])) == 4]


named_mat <- matrix("?", nrow = 12, ncol = 12)

# NW corner - 3209-1

named_mat[1,1] <- "3701-5"

for(row in 1:nrow(named_mat)) {
    for(col in 1:ncol(named_mat)) {
        if(col > 1) {
            western_mat <- named_mat[row, (col - 1)]
            
            which_idx <- which(sapply(lapply(border_map, function(x) grepl(western_mat, x[["W"]])), any))
            
            w_matches <- border_map[[names(which_idx)[names(which_idx) != strsplit(western_mat, "-")[[1]][1]][1]]][["W"]]
            w_match <- w_matches[sapply(strsplit(w_matches, ", "), function(x) x[2] == western_mat)]
            
            named_mat[row, col] <- strsplit(w_match, ", ")[[1]][1]
        } else if(row > 1) {
            northern_mat <- named_mat[(row - 1), col]
            
            which_idx <- which(sapply(lapply(border_map, function(x) grepl(northern_mat, x[["N"]])), any))
            
            n_matches <- border_map[[names(which_idx)[names(which_idx) != strsplit(northern_mat, "-")[[1]][1]][1]]][["N"]]
            n_match <- n_matches[sapply(strsplit(n_matches, ", "), function(x) x[2] == northern_mat)]
            
            named_mat[row, col] <- strsplit(n_match, ", ")[[1]][1]          
        }
    }
}

# Build global mat

global_mat <- matrix("?", nrow = 12*8, ncol = 12*8)
L <- length(seq(1, 12*8, 8))

borderless_mats <- lapply(input_mats, function(x) x[2:9, 2:9])
transform_mats <- lapply(borderless_mats, get_matrix_transformations)

for(i in 1:L) {
    for(j in 1:L) {
        
        x <- seq(1, 12*8, 8)[i]
        y <- seq(1, 12*8, 8)[j]
        
        mat_name <- strsplit(named_mat[i, j], "-")[[1]][1]
        trans_idx <- as.numeric(strsplit(named_mat[i, j], "-")[[1]][2])
        
        global_mat[(x:(x+7)), (y:(y+7))] <- transform_mats[[mat_name]][[trans_idx]]
    }
}


global_trans <- get_matrix_transformations(global_mat)


# Look for sea monsters ---------------------------------------------------

sea_monster <- "                  # 
#    ##    ##    ###
 #  #  #  #  #  #   "

sea_mat <- matrix(strsplit(gsub("\n", "", sea_monster), "")[[1]], ncol = 20, byrow = T)

gmat <- global_trans[[1]]

for(row in 1:(96-2)) {
    for(col in 1:(96-19)) {
        if(all(c(gmat[row, col+18], 
                 gmat[row+1, col], gmat[row+1, col+5], gmat[row+1, col+6], gmat[row+1, col+11], gmat[row+1, col+12], gmat[row+1, col+17], gmat[row+1, col+18], gmat[row+1, col+19],
                 gmat[row+2, col+1], gmat[row+2, col+4], gmat[row+2, col+7], gmat[row+2, col+10], gmat[row+2, col+13], gmat[row+2, col+16]) == "#")) {
            gmat[row, col+18] <- "O"
            gmat[row+1, col] <- "O"
            gmat[row+1, col+5] <- "O"
            gmat[row+1, col+6] <- "O"
            gmat[row+1, col+11] <- "O"
            gmat[row+1, col+12] <- "O"
            gmat[row+1, col+17] <- "O"
            gmat[row+1, col+18] <- "O"
            gmat[row+1, col+19] <- "O"
            gmat[row+2, col+1] <- "O"
            gmat[row+2, col+4] <- "O"
            gmat[row+2, col+7] <- "O"
            gmat[row+2, col+10] <- "O"
            gmat[row+2, col+13] <- "O"
            gmat[row+2, col+16] <- "O"
        }
    }
}

sum(gmat == "#")
