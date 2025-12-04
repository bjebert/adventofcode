
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- get_input("2025/04", parse = F, user = "bjebert", cache = F)

inp <- strsplit("..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.", "\n")[[1]]  # Example

map <- mat2map(inp2mat(inp))

while(T) {
    to_remove <- names(map)[map == "@"][sapply(names(map)[map == "@"], function(coord) {
        sum(map[get_8nb_str(coord)] == '@', na.rm = T) < 4
    })]
    
    if(length(to_remove) == 0) {
        break
    }
    
    map[to_remove] <- '.'
}

orig <- mat2map(inp2mat(inp))
sum(orig == '@') - sum(map == '@')

