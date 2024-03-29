rm(list=ls())
source("utilities.R")
inp <- get_input("2023/10", parse = F, deesblake = F, cache = T)

# inp <- strsplit("..........
# .S------7.
# .|F----7|.
# .||....||.
# .||....||.
# .|L-7F-J|.
# .|..||..|.
# .L--JL--J.
# ..........", "\n")[[1]]

mat <- matrix(strsplit(paste(inp, collapse = ""), "")[[1]],
              nrow = length(inp),
              ncol = nchar(inp[1]),
              byrow = T)

map <- setNames(c(mat), c(sapply(1:ncol(mat), function(i) sapply(1:nrow(mat), function(j) sprintf("%s,%s", j, i)))))

map2mat <- function(map, default = '.') {
    ss <- strsplit(names(map), ",")
    
    min_i <- min(as.numeric(sapply(ss, function(x) x[1])))
    max_i <- max(as.numeric(sapply(ss, function(x) x[1])))
    
    min_j <- min(as.numeric(sapply(ss, function(x) x[2])))
    max_j <- max(as.numeric(sapply(ss, function(x) x[2])))
    
    mat <- matrix(default, nrow = length(min_i:max_i), ncol = length(min_j:max_j))
    
    nr <- max_i - min_i
    nc <- max_j - min_j
    
    for(i in 1:(nr+1)) {
        for(j in 1:(nc+1)) {
            coord <- sprintf("%s,%s", i + min_i - 1, j + min_j - 1)
            if(coord %in% names(map)) {
                mat[i, j] <- map[coord]
            } else {
                mat[i, j] <- default
            }
            
        }
    }
    
    return(mat)    
}

coord2num <- function(coord) as.numeric(c(strsplit(coord, ",")[[1]][1], strsplit(coord, ",")[[1]][2]))

connections <- list(`|` = c('N', 'S'),
                    `-` = c('E', 'W'),
                    `L` = c('N', 'E'),
                    `J` = c('N', 'W'),
                    `7` = c('S', 'W'),
                    `F` = c('S', 'E'))

# Neighbours --------------------------------------------------------------

get_adjacent <- function(map, coord) {
    tile <- map[[coord]]
    xy <- coord2num(coord)
    
    adj_map <- list(`N` = c(xy[1] - 1, xy[2]),
                    `S` = c(xy[1] + 1, xy[2]),
                    `W` = c(xy[1], xy[2] - 1),
                    `E` = c(xy[1], xy[2] + 1))
    
    coords <- adj_map[connections[[tile]]]
    coords <- coords[sapply(coords, function(x) sprintf("%s,%s", x[1], x[2]) %in% names(map))]
    
    return(coords)
}

get_neighbours <- function(map, coord) {
    tile <- map[[coord]]
    
    if(tile == 'S') {
        return(2)
    }
    
    if(tile == '.') {
        return(0)
    }
    
    xy <- coord2num(coord)
    
    opp_map <- c(`N` = "S",
                 `S` = "N",
                 `E` = "W",
                 `W` = "E")
    
    adj_map <- list(`N` = c(xy[1] - 1, xy[2]),
                    `S` = c(xy[1] + 1, xy[2]),
                    `W` = c(xy[1], xy[2] - 1),
                    `E` = c(xy[1], xy[2] + 1))
    
    adj_tiles <- adj_map[connections[[tile]]]
    
    # Check if the adjacent tiles also connect back    
    adj_coords <- sapply(adj_tiles, function(x) sprintf("%s,%s", x[1], x[2]))
    
    return(sum(sapply(1:length(adj_coords), function(i) {
        if(!(adj_coords[i] %in% names(map))) {
            return(FALSE)
        }
        
        adj_tile <- map[[adj_coords[i]]]
        
        if(adj_tile == "S") {
            return(TRUE)
        }
        
        return(opp_map[[names(adj_coords)[i]]] %in% connections[[adj_tile]])
    })))
}



# Establish main loop -----------------------------------------------------

nb_map <- setNames(lapply(names(map), function(x) get_neighbours(map, x)),
                   names(map))

map_main <- copy(map)
loop_len <- sum(nb_map == 2)

while(TRUE) {
    map_main <- map[names(nb_map)[nb_map == 2]]
    
    nb_map <- setNames(lapply(names(map_main), function(x) get_neighbours(map_main, x)),
                       names(map_main))
    
    if(sum(nb_map == 2) == loop_len) {
        break
    }
    
    loop_len <- sum(nb_map == 2)
}

# Replace S with fit
starting_tile <- names(map)[map == "S"]

start_potential <- sapply(c('|', '-', 'L', 'J', 'F', '7'), function(tile) {
    tmp_map <- copy(map_main)
    tmp_map[[starting_tile]] <- tile
    
    get_neighbours(tmp_map, starting_tile)
}) 

map_main[[starting_tile]] <- names(start_potential)[start_potential == 2]
map[[starting_tile]] <- names(start_potential)[start_potential == 2]

# Part 2 ------------------------------------------------------------------

# Assess intersections between coordinates to determine whether they can be sqzd through

horz_int <- unlist(sapply(names(map), function(coord) {
    xy <- coord2num(coord)
    adj_south <- sprintf("%s,%s", xy[1] + 1, xy[2])
    
    if(!adj_south %in% names(map)) {
        return(NULL)
    }
    
    # Check
    if("S" %in% connections[[map[[coord]]]] &&
       "N" %in% connections[[map[[adj_south]]]]) {
        return(sprintf("%s-%s", coord, adj_south))
    }
}))

vert_int <- unlist(sapply(names(map), function(coord) {
    xy <- coord2num(coord)
    adj_east <- sprintf("%s,%s", xy[1], xy[2] + 1)
    
    if(!adj_east %in% names(map)) {
        return(NULL)
    }
    
    # Check
    if("E" %in% connections[[map[[coord]]]] &&
       "W" %in% connections[[map[[adj_east]]]]) {
        return(sprintf("%s-%s", coord, adj_east))
    }
}))

# Note that vert_int, horz_int are the paths that are BLOCKED by pipes
blocked_int <- unname(c(vert_int, horz_int))

all_vert <- unlist(lapply(1:ncol(mat), function(x) sapply(0:nrow(mat), function(i) sprintf("%s,%s-%s,%s", i, x, i+1, x))))
all_horz <- unlist(lapply(1:nrow(mat), function(x) sapply(0:ncol(mat), function(i) sprintf("%s,%s-%s,%s", x, i, x, i+1))))

all_int <- c(all_vert, all_horz)

int_esc <- setNames(rep("?", length(all_int)), all_int)
int_esc <- int_esc[!names(int_esc) %in% blocked_int]

# Mark whole tiles that can escape to outside

dist_from_edge <- function(coord) {
    xy <- coord2num(coord)
    min(c(xy[1] - 1, nrow(mat) - xy[1],
          xy[2] - 1, ncol(mat) - xy[2]))
}

tile_esc <- setNames(rep("?", length(map)), names(map))
tile_esc <- tile_esc[!(names(tile_esc) %in% names(map_main))]

tile_dist <- sapply(names(tile_esc), dist_from_edge)
tile_esc <- tile_esc[order(tile_dist)]


# Recursive loop to check tiles that can escape ---------------------------


update_maps <- function(Q, vis, status = "O") {  # After success escaping, mark all adjacent tiles/intersections as O
    all_adj <- unique(c(Q, vis))
    
    tile_esc[names(tile_esc) %in% all_adj] <<- status
    int_esc[names(int_esc) %in% all_adj] <<- status
}


can_escape <- function(curr) {
    Q <- curr
    vis <- c()
    
    # Perform BFS, looking at both intersection and tile paths to escape
    while(length(Q) > 0) {
        curr <- Q[1]
        vis <- c(vis, curr)
        Q <- Q[-1]
        
        is_tile <- !grepl("-", curr)
        
        if(is_tile) {
            if(dist_from_edge(curr) == 0) {
                update_maps(Q, vis)
                return(TRUE)
            }
            
            xy <- coord2num(curr)
            
            adj_map <- list(`N` = c(xy[1] - 1, xy[2]),
                            `S` = c(xy[1] + 1, xy[2]),
                            `W` = c(xy[1], xy[2] - 1),
                            `E` = c(xy[1], xy[2] + 1))
            
            adj_coords <- sapply(adj_map, function(x) sprintf("%s,%s", x[1], x[2]))    
            adj_coords <- unname(adj_coords[adj_coords %in% names(tile_esc)])
            
            if(any(tile_esc[adj_coords] == "O")) {
                update_maps(Q, vis)
                return(TRUE)
            } else if(any(tile_esc[adj_coords] == "I")) {
                update_maps(Q, vis, "I")
                return(FALSE)
            }
            
            Q <- c(Q, setdiff(adj_coords, vis))
            
            adj_ints <- c(sprintf("%s,%s-%s,%s", xy[1] - 1, xy[2], xy[1], xy[2]),
                          sprintf("%s,%s-%s,%s", xy[1], xy[2], xy[1] + 1, xy[2]),
                          sprintf("%s,%s-%s,%s", xy[1], xy[2] - 1, xy[1], xy[2]),
                          sprintf("%s,%s-%s,%s", xy[1], xy[2], xy[1], xy[2] + 1))
            
            adj_ints <- intersect(adj_ints, names(int_esc))
            
            if(any(int_esc[adj_ints] == "O")) {
                update_maps(Q, vis)
                return(TRUE)
            } else if(any(int_esc[adj_ints] == "I")) {
                update_maps(Q, vis, "I")
                return(FALSE)
            }
            
            Q <- unique(c(Q, setdiff(adj_ints, vis)))
        } else {
            coords <- lapply(strsplit(curr, "-")[[1]], coord2num)
            dists <- sapply(strsplit(curr, "-")[[1]], dist_from_edge)
            
            if(any(dists < 0)) {
                update_maps(Q, vis)
                return(TRUE)
            }
            
            # If intersection is horizontal, look at adjacent 4 vertical intersections + 2 horizontals
            if(coords[[1]][1] != coords[[2]][1]) {
                adj_ints <- c(sprintf("%s,%s-%s,%s", coords[[1]][1], coords[[1]][2] - 1, coords[[2]][1] - 1, coords[[2]][2]),
                              sprintf("%s,%s-%s,%s", coords[[1]][1] + 1, coords[[1]][2] - 1, coords[[2]][1], coords[[2]][2]),
                              sprintf("%s,%s-%s,%s", coords[[1]][1], coords[[1]][2], coords[[2]][1] - 1, coords[[2]][2] + 1),
                              sprintf("%s,%s-%s,%s", coords[[1]][1] + 1, coords[[1]][2], coords[[2]][1], coords[[2]][2] + 1),
                              sprintf("%s,%s-%s,%s", coords[[1]][1], coords[[1]][2] - 1, coords[[2]][1], coords[[2]][2] - 1),
                              sprintf("%s,%s-%s,%s", coords[[1]][1], coords[[1]][2] + 1, coords[[2]][1], coords[[2]][2] + 1))
                
            } else {  # If intersection is vertical, look at adjacent 4 horizontal intersections + 2 verticals
                adj_ints <- c(sprintf("%s,%s-%s,%s", coords[[1]][1] - 1, coords[[1]][2], coords[[2]][1], coords[[2]][2] - 1),
                              sprintf("%s,%s-%s,%s", coords[[1]][1] - 1, coords[[1]][2] + 1, coords[[2]][1], coords[[2]][2]),
                              sprintf("%s,%s-%s,%s", coords[[1]][1], coords[[1]][2], coords[[2]][1] + 1, coords[[2]][2] - 1),
                              sprintf("%s,%s-%s,%s", coords[[1]][1], coords[[1]][2] + 1, coords[[2]][1] + 1, coords[[2]][2]),
                              sprintf("%s,%s-%s,%s", coords[[1]][1] - 1, coords[[1]][2], coords[[2]][1] - 1, coords[[2]][2]),
                              sprintf("%s,%s-%s,%s", coords[[1]][1] + 1, coords[[1]][2], coords[[2]][1] + 1, coords[[2]][2]))
            }
            
            adj_ints <- intersect(adj_ints, names(int_esc))
            
            if(any(int_esc[adj_ints] == "O")) {
                update_maps(Q, vis)
                return(TRUE)
            } else if(any(int_esc[adj_ints] == "I")) {
                update_maps(Q, vis, "I")
                return(FALSE)
            }
            
            Q <- unique(c(Q, setdiff(adj_ints, vis)))
        }
    }
    
    update_maps(Q, vis, "I")     
    return(FALSE)
}


for(i in 1:length(tile_esc)) {
    print(i)
    curr <- names(tile_esc)[i]
    if(tile_esc[i] != "?") {
        next
    }
    
    can_escape(curr)
}

sum(tile_esc == "I") + 4  # there is a loop of 4 tiles trapped inside, not part of main as doesnt connect to start tile

# Export to Excel
map_export <- copy(map)
map_export[names(map_export) %in% names(tile_esc)[tile_esc == "I"]] <- "1"
map_export[names(map_export) %in% names(tile_esc)[tile_esc == "O"]] <- "2"
map_export[names(map_export) %in% names(map_main)] <- "3"

fwrite(map2mat(map_export), "D:/projects/aoc/2023/10-b2.csv", col.names = F)
