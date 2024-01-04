rm(list=ls())
source("utilities.R")
inp <- get_input("2022/14", parse = F, deesblake = F, cache = T)

# inp <- strsplit("498,4 -> 498,6 -> 496,6
# 503,4 -> 502,4 -> 502,9 -> 494,9", "\n")[[1]]


pos2str <- function(x, y) sprintf("%s,%s", x, y)

# Construct cave
cave <- list()
for(line in inp) {
    coords <- strsplit(strsplit(line, " -> ")[[1]], ",")
    i <- 1
    for(i in 1:(length(coords) - 1)) {
        start <- as.numeric(coords[[i]])
        end <- as.numeric(coords[[i+1]])
        mat <- sapply(data.table(x = start[1]:end[1], y = start[2]:end[2]), as.numeric)
        for(j in 1:nrow(mat)) {
            cave[[pos2str(mat[j,][1], mat[j,][2])]] <- "#"
        }
    }
}


simulate_sand <- function() {
    pos <- c(500, 0)
    while(TRUE) {
        d <- pos2str(pos[1], pos[2] + 1)
        dl <- pos2str(pos[1] - 1, pos[2] + 1)
        dr <- pos2str(pos[1] + 1, pos[2] + 1)        
        
        if(pos[2] > 200) {
            return(FALSE)
        }
        
        if(!(d %in% names(grid))) {
            pos <- c(pos[1], pos[2] + 1)
            next
        }
        
        if(!(dl %in% names(grid))) {
            pos <- c(pos[1] - 1, pos[2] + 1)
            next
        }
        
        if(!(dr %in% names(grid))) {
            pos <- c(pos[1] + 1, pos[2] + 1)
            next
        }
        
        if(pos[1] == 500 && pos[2] == 0) {
            return(FALSE)
        }
        
        grid[[pos2str(pos[1], pos[2])]] <<- "o"
        # print(pos2str(pos[1], pos[2]))
        return(TRUE)
    }
}


# Simulate sand
grid <- copy(cave)
while(TRUE) {
    res <- simulate_sand()
    if(!res) {
        break
    }
}

sum(grid == "o")


# Part 2 ------------------------------------------------------------------

floor_y <- max(sapply(strsplit(names(cave), ","), function(x) as.numeric(x[2]))) + 2

for(x in 300:700) {
    cave[pos2str(x, floor_y)] <- "#"
}

grid <- copy(cave)
while(TRUE) {
    res <- simulate_sand()
    if(!res) {
        break
    }
}

sum(grid == "o") + 1

grid



write_grid <- function() {
    x1 <- min(sapply(strsplit(names(cave), ","), function(x) as.numeric(x[1])))
    x2 <- max(sapply(strsplit(names(cave), ","), function(x) as.numeric(x[1])))
    y1 <- min(sapply(strsplit(names(cave), ","), function(x) as.numeric(x[2])))
    y2 <- max(sapply(strsplit(names(cave), ","), function(x) as.numeric(x[2])))
    
    disp <- matrix(".", ncol = 710, nrow = 185)
    
    for(x in 300:700) {
        for(y in 0:180) {
            p <- pos2str(x, y)
            if(p %in% names(grid)) {
                disp[y+1, x+1] <- grid[[p]]
            }
        }
    }
    
    disp[1, 501] <- "+"
    fwrite(disp, "aoc.csv")
    
}

write_grid()
