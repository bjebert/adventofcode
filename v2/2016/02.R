
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example
inp <- get_input("2016/02", parse = F, user = "blakeebets", cache = T)

btn_map <- c("1,-1" = 1, "1,0" = 2, "1,1" = 3, "0,-1" = 4, "0,0" = 5, "0,1" = 6,
             "-1,-1" = 7, "-1,0" = 8, "-1,1" = 9)

btn_map2 <- c("0,0" = 5, 
              "1,1" = 2, "0,1" = 6, "-1,1" = "A",
              "2,2" = 1, "1,2" = 3, "0,2" = 7, "-1,2" = "B", "-2,2" = "D",
              "1,3" = 4, "0,3" = 8, "-1,3" = "C",
              "0,4" = 9)

pos <- c(0, 0)

for(line in inp) {
    dirs <- strsplit(line, "")[[1]]
    
    for(x in move_map[dirs]) {
        newpos <- pos + x
        
        if(!pos2coord(newpos) %in% names(btn_map2)) {
            next
        }
        
        pos <- pos + x
    }
    
    print(btn_map2[[pos2coord(pos)]])
}

# 3:16 (4th)

# 5:14 (1st)