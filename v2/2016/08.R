
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

# inp <- strsplit("rect 3x2
# rotate column x=1 by 1
# rotate row y=0 by 4
# rotate column x=1 by 1", "\n")[[1]]  # Example
# m <- matrix('.', nrow = 3, ncol = 7)

inp <- get_input("2016/08", parse = F, user = "blakeebets", cache = T)
m <- matrix('.', nrow = 6, ncol = 50)

map <- mat2map(m)

for(line in inp) {
    ls <- strsplit(line, " ")[[1]]
    
    if(ls[1] == "rect") {
        dim <- strsplit(ls[2], "x")[[1]]
        A <- as.numeric(dim[1])
        B <- as.numeric(dim[2])
        
        for(a in 1:A) {
            for(b in 1:B) {
                map[[pos2coord(c(b, a))]] <- "#"
            }
        }
    } else if(ls[1] == "rotate") {
        id <- as.numeric(strsplit(ls[3], "=")[[1]][2]) + 1
        amt <- as.numeric(ls[5])
        
        tmp_map <- copy(map)
        
        if(ls[2] == "row") {
            for(i in 1:ncol(m)) {
                tmp_map[sprintf("%d,%d", id, i)] <- map[sprintf("%d,%d", id, ((i - amt) - 1) %% ncol(m) + 1)]
            }
            
        } else if(ls[2] == "column") {
            for(i in 1:nrow(m)) {
                tmp_map[sprintf("%d,%d", i, id)] <- map[sprintf("%d,%d", ((i - amt) - 1) %% nrow(m) + 1, id)]
            }
        }
        
        map <- copy(tmp_map)
    }
    
    # print(map2mat(map))
}

sum(map == '#')

# 14:02 (45th)

plot(map2mat(map))

# 14:53 (31st)
