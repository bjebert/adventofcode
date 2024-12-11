
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("T.........
...T......
.T........
..........
..........
..........
..........
..........
..........
..........", "\n")[[1]]  # Example

inp <- get_input("2024/08", parse = F, user = "bjebert", cache = F)

map <- mat2map(inp2mat(inp))

nodes <- sort(unique(map[map != "."]))


create_antinodes <- function(antenna) {
    an <- c()
    for(i in 1:(length(antenna)-1)) {
        for(j in (i+1):length(antenna)) {
            
            curr_1 <- str2pos(antenna[j])
            curr_2 <- str2pos(antenna[i])
            
            delta_1 <- (str2pos(antenna[j]) - str2pos(antenna[i]))
            delta_2 <- (str2pos(antenna[i]) - str2pos(antenna[j]))
            
            for(k in 1:100) {
                an <- c(an, pos2str(curr_1 + delta_1))
                an <- c(an, pos2str(curr_2 + delta_2))
                
                curr_1 <- curr_1 + delta_1
                curr_2 <- curr_2 + delta_2
            }
        }
    }
    
    return(an[an %in% names(map)])
}


all_an <- c()
for(n in nodes) {
    antenna <- names(map)[map == n]
    all_an <- c(all_an, create_antinodes(antenna))
}

length(unique(all_an))
length(unique(c(all_an, names(map)[map != "."])))


map[names(map) %in% all_an] <- "#"
fwrite(map2mat(map), "08.csv")
       
>>>>>>> ae4d582c9fbbd730c314143e6aa2faf63580b549
