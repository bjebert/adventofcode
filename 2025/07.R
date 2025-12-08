
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inr <- get_input("2025/07", parse = F, user = "bjebert", cache = F)
inx <- strsplit(".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............", "\n")[[1]]

# Part 1 ------------------------------------------------------------------

inp <- inx
inp <- inr

map <- mat2map(inp2mat(inp))

start <- names(map)[map == "S"]
map[[start]] <- "."
beams <- start
splits <- 0

while(length(beams) > 0) {
    
    new_beams <- NULL
    
    for(beam in beams) {
        if(is.na(map[beam])) {
            next
        } else if(map[beam] == "^") {
            # Split beam left and right
            new_l <- pos2str(str2pos(beam) + c(0, -1))
            new_r <- pos2str(str2pos(beam) + c(0, 1))
            splits <- splits + 1
            
            if(map[[new_l]] == "^" || map[[new_r]] == "^") {
                stop("Split into splitter?")
            }
            
            new_beams <- c(new_beams, new_l, new_r)
        } else if(map[beam] == ".") {
            new_d <- pos2str(str2pos(beam) + c(1, 0))
            map[[beam]] <- "|"
            
            new_beams <- c(new_beams, new_d)
        } 
    }
    
    beams <- unique(new_beams)
}

splits

# Part 2 ------------------------------------------------------------------

inp <- inr

map <- mat2map(inp2mat(inp))
mat <- inp2mat(inp)
start <- names(map)[map == "S"]

# If a particle goes left or right, need to know how many possible solutions either side
library(memoise)

solve <- memoise(function(pos) {
    coord <- str2pos(pos)
    below <- map[sapply((coord[1]+1):nrow(mat), function(i) pos2str(c(i, coord[2])))]
    w <- which(below == "^")[1]
    
    if(is.na(w)) {
        return(1)
    }
    
    splitter <- names(below[w])
    
    new_l <- pos2str(str2pos(splitter) + c(0, -1))
    new_r <- pos2str(str2pos(splitter) + c(0, 1))
    
    return(solve(new_l) + solve(new_r))
})


solve(start)
