rm(list=ls())
source("utilities.R")
inp <- get_input("2016/1", deesblake = T)


dirx <- strsplit(inp, ", ")[[1]]
d <- substr(dirx, 1, 1)
amt <- as.numeric(substr(dirx, 2, nchar(dirx)))


l_map <- c("N" = "W", "W" = "S", "S" = "E", "E" = "N")
r_map <- c("N" = "E", "S" = "W", "E" = "S", "W" = "N")


facing <- "N"
hist <- c()
pos <- c(0, 0)

for(i in 1:length(dirx)) {
    
    if(d[i] == "L") {
        facing <- l_map[facing]
    } else if(d[i] == "R") {
        facing <- r_map[facing]
    }
    
    if(facing == "N") {
        for(x in 1:amt[i]) {
            pos[2] <- pos[2] + 1
            z <- sprintf("%s|%s", pos[1], pos[2])
            if(z %in% hist) {
                stop(z)
            }
            
            hist <- c(hist, z)
        }
    } else if(facing == "S") {
        for(x in 1:amt[i]) {
            pos[2] <- pos[2] - 1
            z <- sprintf("%s|%s", pos[1], pos[2])
            if(z %in% hist) {
                stop(z)
            }
            
            hist <- c(hist, z)
        }
    } else if(facing == "E") {
        for(x in 1:amt[i]) {
            pos[1] <- pos[1] + 1
            z <- sprintf("%s|%s", pos[1], pos[2])
            if(z %in% hist) {
                stop(z)
            }
            
            hist <- c(hist, z)
        }
    } else if(facing == "W") {
        for(x in 1:amt[i]) {
            pos[1] <- pos[1] - 1
            z <- sprintf("%s|%s", pos[1], pos[2])
            if(z %in% hist) {
                stop(z)
            }
            
            hist <- c(hist, z)
        }
    }
}

# 3:49 (1st) / 8:29 (4th)

