rm(list = ls())
source("utilities.R")
inp <- get_input("2023/18", parse = F, deesblake = F, cache = T)

# inp <- strsplit("R 6 (#70c710)
# D 5 (#0dc571)
# L 2 (#5713f0)
# D 2 (#d2c081)
# R 2 (#59c680)
# D 2 (#411b91)
# L 5 (#8ceee2)
# U 2 (#caa173)
# L 1 (#1b58a2)
# U 2 (#caa171)
# R 2 (#7807d2)
# U 3 (#a77fa3)
# L 2 (#015232)
# U 2 (#7a21e3)", "\n")[[1]]

dirs <- substr(inp, 1, 1)
amts <- as.numeric(sapply(strsplit(inp, " "), function(x) x[2]))
cols <- gsub("\\(|\\)", "", sapply(strsplit(inp, " "), function(x) x[3]))


# Part 2 ------------------------------------------------------------------
dirs <- unname(c(`0` = 'R', `1` = 'D', `2` = 'L', `3` = 'U')[substr(cols, nchar(cols), nchar(cols))])
amts <- sapply(substr(cols, 2, nchar(cols) - 1), function(x) strtoi(x, base = 16L), USE.NAMES = F)


create_walls <- function(zero = FALSE) {
    
    dir_map <- list("N" = c(-1, 0), "S" = c(1, 0), "E" = c(0, 1), "W" = c(0, -1))
    compass_map <- c(`U` = "N", `R` = "E", `D` = "S", `L` = "W")
    
    walls <- list()
    curr <- c(1, 1)
    
    for(i in 1:length(dirs)) {
        new_loc <- curr + dir_map[[compass_map[dirs[i]]]] * amts[i]
        walls[[length(walls) + 1]] <- list(curr, new_loc)
        curr <- new_loc
    }
    
    return(walls)
}

walls <- create_walls()
is_horizontal <- sapply(walls, function(w) w[[1]][2] != w[[2]][2])

min_x <- min(sapply(walls, function(x) min(c(x[[1]][2], x[[2]][2]))))
max_x <- max(sapply(walls, function(x) max(c(x[[1]][2], x[[2]][2]))))

min_y <- min(sapply(walls, function(x) min(c(x[[1]][1], x[[2]][1]))))
max_y <- max(sapply(walls, function(x) max(c(x[[1]][1], x[[2]][1]))))

# Zero wall coords to match Excel sheet
walls <- lapply(walls, function(x) list(x[[1]] - c(min_y - 1, min_x - 1), x[[2]] - c(min_y - 1, min_x - 1)))


x_overlap <- function(wall, x_start, x_end) {  # Get number of tiles overlapping this wall on the X axis
    bounds <- sort(c(wall[[1]][2], wall[[2]][2]))
    bounds_cap <- c(max(bounds[1], x_start), min(bounds[2], x_end))
    return(bounds_cap[2] - bounds_cap[1] + 1)
}


reduce_box_walls <- function(box_walls) {
    if(length(box_walls) == 1) {
        return(box_walls)
    }
    
    box_walls <- lapply(1:length(box_walls), function(j) {
        j_bounds <- sort(c(box_walls[[j]][[1]][2], box_walls[[j]][[2]][2]))
        j_y <- box_walls[[j]][[1]][1]
        
        for(k in 1:length(box_walls)) {
            if(k != j) {
                k_y <- box_walls[[k]][[1]][1]
                
                if(k_y < j_y) {
                    # Set any overlapping bounds to k_y
                    k_bounds <- sort(c(box_walls[[k]][[1]][2], box_walls[[k]][[2]][2]))
                    
                    if(x_overlap(box_walls[[j]], k_bounds[1], k_bounds[2]) > 0) {
                        if(j_bounds[1] >= k_bounds[1]) {  # Right-overlap
                            j_bounds[1] <- k_bounds[2] + 1
                        } else if(j_bounds[2] <= k_bounds[2]) {  # Left-overlap
                            j_bounds[2] <- k_bounds[1] - 1
                        } else if(j_bounds[1] <= k_bounds[1] && j_bounds[2] >= k_bounds[2]) {  # Full-overlap
                            
                            jb_left <- c(j_bounds[1], k_bounds[1] - 1)
                            jb_right <- c(k_bounds[2] + 1, j_bounds[2])
                            
                            return(list(
                                list(c(j_y, jb_left[1]), c(j_y, jb_left[2])),
                                list(c(j_y, jb_right[1]), c(j_y, jb_right[2]))
                            ))
                        }
                        
                        if(j_bounds[1] > j_bounds[2]) {
                            j_bounds <- NULL
                            break
                        }
                    }
                }
            }
        }
        
        if(is.null(j_bounds)) {
            return(NULL)
        }
        
        return(list(c(j_y, j_bounds[1]), c(j_y, j_bounds[2])))
    })
    
    box_walls <- box_walls[!sapply(box_walls, is.null)]
    
    # Flatten third layer
    is_flat <- sapply(box_walls, function(x) !is.list(x[[1]]))
    
    c(box_walls[is_flat], unlist(box_walls[!is_flat], recursive = F))
}


cell_count <- 0

for(i in 1:length(walls)) {
    
    if(walls[[i]][[2]][2] > walls[[i]][[1]][2]) {  # Only count tiles DOWNwards on right-moving walls (west -> east)
        y <- walls[[i]][[1]][1]
        
        # Was the previous wall north or south?  (If north, then only start counting down from 2nd tile in this wall)
        prv <- if(i != 1) i - 1 else length(walls)
        
        # Is the next wall north or south?  (If south, then stop counting from the 2nd-last tile)
        nxt <- if(i != length(walls)) i + 1 else 1
        
        is_last_north <- walls[[prv]][[1]][1] > walls[[prv]][[2]][1]
        is_next_north <- walls[[nxt]][[1]][1] > walls[[nxt]][[2]][1]
        
        x_start <- walls[[i]][[1]][2] + if(is_last_north) 1 else 0
        x_end <- walls[[i]][[2]][2] - if(is_next_north) 0 else 1
        
        # Look for horizontal walls with Y > y, and X overlapping x_start to x_end
        
        lower_y <- sapply(walls, function(w) max(w[[1]][1], w[[2]][1]) > y)
        is_x_overlap <- sapply(walls, function(w) x_overlap(w, x_start, x_end)) > 0
        
        box_walls <- walls[is_horizontal & lower_y & is_x_overlap]
        
        # Remove parts of each box wall if there is another horizontal wall above it
        for(z in 1:5) {  # As box flattening is slightly dodgy, we run a few times to ensure all walls appropriately flattened
            box_walls <- reduce_box_walls(box_walls)
        }
        
        cells <- sum(sapply(box_walls, function(w) {
            return(max(0, x_overlap(w, x_start, x_end)) * (w[[1]][1] - y - 1))
        }))
        
        print(sprintf("%s | (%s,%s) -> (%s,%s): %s", i, walls[[i]][[1]][1], walls[[i]][[1]][2], walls[[i]][[2]][1], walls[[i]][[2]][2], cells))
        
        cell_count <- cell_count + cells
    }
}

cell_count + sum(sapply(walls, function(x) abs(x[[1]][1] - x[[2]][1]) + abs(x[[1]][2] - x[[2]][2]))) 

