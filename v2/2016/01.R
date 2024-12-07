
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example
inp <- get_input("2016/01", parse = F, user = "blakeebets", cache = T)

dirs <- strsplit(inp, ", ")[[1]]

dir <- substr(dirs, 1, 1)
amt <- as.numeric(substr(dirs, 2, nchar(dirs)))

pos <- c(0, 0)
facing <- "N"

left_turn <- c("N" = "W", "W" = "S", "S" = "E", "E" = "N")
right_turn <- c("N" = "E", "E" = "S", "S" = "W", "W" = "N")

for(i in 1:length(dirs)) {
    if(dir[i] == "L") {
        facing <- left_turn[[facing]]
    } else {
        facing <- right_turn[[facing]]
    }
    
    pos <- pos + move_map[[facing]] * amt[i]
}

sum(abs(pos))

# 5:29 (typo'd on submission at 4:31), 6th

# Part 2 ------------------------------------------------------------------

pos <- c(0, 0)
facing <- "N"

hist <- c(pos2coord(pos))

left_turn <- c("N" = "W", "W" = "S", "S" = "E", "E" = "N")
right_turn <- c("N" = "E", "E" = "S", "S" = "W", "W" = "N")

for(i in 1:length(dirs)) {
    if(dir[i] == "L") {
        facing <- left_turn[[facing]]
    } else {
        facing <- right_turn[[facing]]
    }
    
    for(j in 1:amt[i]) {
        pos <- pos + move_map[[facing]]
        coord <- pos2coord(pos)
        
        if(coord %in% hist) {
            print(sum(abs(pos)))
            stop()
        } else {
            hist <- c(hist, coord)
        }
    }
}

# 8:17, 3rd
