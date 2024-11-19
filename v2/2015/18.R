
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example
inp <- get_input("2015/18", parse = F, user = "blakeebets", cache = T)

map <- mat2map(inp2mat(inp))

get_nb_on <- function(coord) {
    return(sum(map[get_8nb_coord_fast(coord)] == '#', na.rm = T))
}

for(i in 1:100) {
    # Part 2
    map["1,1"] <- "#"
    map["1,100"] <- "#"
    map["100,1"] <- "#"
    map["100,100"] <- "#"
    
    print(i)
    map_new <- copy(map)
    
    nb_on <- sapply(names(map), function(coord) get_nb_on(coord))
    
    map_new[map_new == '#'] <- '.'
    map_new[map == '#' & nb_on %in% 2:3] <- '#'
    map_new[map == '.' & nb_on == 3] <- '#'
    
    map <- copy(map_new)
}

map["1,1"] <- "#"
map["1,100"] <- "#"
map["100,1"] <- "#"
map["100,100"] <- "#"
sum(map == '#')

# 14:15 (34th)

# 19:01 (45th)