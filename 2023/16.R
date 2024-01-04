rm(list=ls())
source("utilities.R")
inp <- get_input("2023/16", parse = F, deesblake = F, cache = T)

inp <- strsplit(".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....", "\n")[[1]]

# 6:56:07

mat <- inp2mat(inp)
map <- mat2map(mat)


# Part 1 ------------------------------------------------------------------

get_energized <- function(start_pos, start_dir) {
    beams <- list()
    beams[[1]] <- list(pos = start_pos, dir = start_dir)
    
    hist <- c()
    hist_with_pos <- c()
    
    while(length(beams) >= 1) {
        to_delete <- c()
        
        # move each beam
        for(i in 1:length(beams)) {
            pos <- beams[[i]][["pos"]]
            dir <- beams[[i]][["dir"]]
            new_pos <- pos + dir
            
            beams[[i]][["pos"]] <- new_pos
            tile <- map[pos2coord(new_pos)]
            
            if(is.na(tile)) {
                next
            }
            
            # if split, create new beams and delete curr
            if(tile == "|" && dir[2] != 0) {
                to_delete <- c(to_delete, i)
                
                beams[[length(beams) + 1]] <- list(pos = new_pos, dir = c(-1, 0))
                beams[[length(beams) + 1]] <- list(pos = new_pos, dir = c(1, 0))
                
            } else if(tile == "-" && dir[1] != 0) {
                to_delete <- c(to_delete, i)
                
                beams[[length(beams) + 1]] <- list(pos = new_pos, dir = c(0, -1))
                beams[[length(beams) + 1]] <- list(pos = new_pos, dir = c(0, 1))
            } 
            
            # if mirror, then change dir
            if(tile == "/") {
                if(dir[2] == 1) {  # right -> up
                    new_dir <- c(-1, 0)  
                } else if(dir[2] == -1) {  # left -> down
                    new_dir <- c(1, 0)
                } else if(dir[1] == 1) {  # down -> left
                    new_dir <- c(0, -1)
                } else if(dir[1] == -1) {  # up -> right
                    new_dir <- c(0, 1)
                } else {
                    stop()
                }
                
                beams[[i]][["dir"]] <- new_dir
            } else if(tile == "\\") {
                
                if(dir[2] == 1) {  # right -> down
                    new_dir <- c(1, 0)
                } else if(dir[2] == -1) {  # left -> up
                    new_dir <- c(-1, 0)
                } else if(dir[1] == 1) {  # down -> right
                    new_dir <- c(0, 1)
                } else if(dir[1] == -1) {  # up -> left
                    new_dir <- c(0, -1)
                } else {
                    stop()
                }
                
                beams[[i]][["dir"]] <- new_dir
            }
            
            if(paste(pos2coord(beams[[i]][["pos"]]),
                     pos2coord(beams[[i]][["dir"]]), sep = ",") %in% hist_with_pos) {
                to_delete <- c(to_delete, i)
            }
        }
        
        if(length(to_delete) > 0) {
            beams <- beams[-to_delete]
        }
        
        if(length(beams) > 0) {
            # delete beams that have left the grid
            beams <- beams[sapply(beams, function(beam) pos2coord(beam[["pos"]]) %in% names(map))]
            
            # add to history position of all beams
            hist <- unique(c(hist, sapply(beams, function(beam) pos2coord(beam[["pos"]]))))
            hist_with_pos <- unique(c(hist_with_pos, sapply(beams, function(beam) paste(pos2coord(beam[["pos"]]),
                                                                                        pos2coord(beam[["dir"]]), sep = ","))))
        }
    }
    
    return(length(hist))
}

nrow(mat)

start_l <- lapply(1:nrow(mat), function(i) {
    return(list(start_pos = c(i, 0), start_dir = c(0, 1)))
})

start_r <- lapply(1:nrow(mat), function(i) {
    return(list(start_pos = c(i, ncol(mat) + 1), start_dir = c(0, -1)))
})

start_u <- lapply(1:nrow(mat), function(i) {
    return(list(start_pos = c(0, i), start_dir = c(1, 0)))
})

start_d <- lapply(1:nrow(mat), function(i) {
    return(list(start_pos = c(nrow(mat) + 1, i), start_dir = c(-1, 0)))
})

start_potentials <- c(start_l, start_r, start_u, start_d)

max_en <- 0
sapply(start_potentials, function(x) {
    en <- get_energized(x[["start_pos"]], x[["start_dir"]])
    
    if(en > max_en) {
        print(x)
        print(en)
        max_en <<- en
    }
})

# 7:42

