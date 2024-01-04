inp <- readLines("2023/10.txt")

inp <- strsplit("...........|
.S------7..|
.|F----7|..|
.||....||..|
.||....||..|
.|L-7F-J|F-|
.|..||..||.|
.L--JL--JL-J
...........|
--7........|
..|........|
..|........|", "\n")[[1]]

inp <- strsplit("..........
.S------7.
.|F----7|.
.||....||.
.||....||.
.|L-7F-J|.
.|..||..|.
.L--JL--J.
..........", "\n")[[1]]

mat <- matrix(strsplit(paste(inp, collapse = ""), "")[[1]],
              nrow = length(inp),
              ncol = nchar(inp[1]),
              byrow = T)

map <- setNames(c(mat), c(sapply(1:ncol(mat), function(i) sapply(1:nrow(mat), function(j) sprintf("%s,%s", j, i)))))

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

dist_from_edge <- function(coord) {
    xy <- coord2num(coord)
    min(abs(c(xy[1] - 1, nrow(mat) - xy[1],
              xy[2] - 1, ncol(mat) - xy[2])))
}

# Mark whole tiles that can escape to outside

tile_esc <- setNames(rep("?", length(map)), names(map))
tile_esc <- tile_esc[!(names(map) %in% names(map_main))]

tile_dist <- sapply(names(tile_esc), dist_from_edge)
tile_esc <- tile_esc[order(tile_dist)]

unvisited <- names(tile_esc)
Q <- unvisited[1]

while(length(unvisited) > 0) {
    curr <- Q[1]
    unvisited <- setdiff(unvisited, curr)
    
    if(length(Q) > 1) {
        Q <- Q[2:length(Q)]
    } else if(length(Q) == 1) {
        Q <- c()
    } else if(length(Q) == 0) {
        Q <- unvisited[1]
    }
    
    status <- "?"
    
    # Check if curr tile is on edge, if so it can escape (O)
    if(dist_from_edge(curr) == 0) {
        status <- "O"
    }
    
    xy <- coord2num(curr)
    
    adj_map <- list(`N` = c(xy[1] - 1, xy[2]),
                    `S` = c(xy[1] + 1, xy[2]),
                    `W` = c(xy[1], xy[2] - 1),
                    `E` = c(xy[1], xy[2] + 1))
    
    adj_coords <- sapply(adj_map, function(x) sprintf("%s,%s", x[1], x[2]))    
    adj_coords <- adj_coords[adj_coords %in% names(tile_esc)]
    
    # Add all neighbours to queue if not already visited
    
    
    adj_coords
    
    
    
}





# Assess intersections between coordinates to determine whether they can be sqzd through

vert_int <- unlist(sapply(names(map), function(coord) {
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

horz_int <- unlist(sapply(names(map), function(coord) {
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

all_vert <- unlist(lapply(1:ncol(mat), function(x) sapply(1:(nrow(mat)-1), function(i) sprintf("%s,%s-%s,%s", i, x, i+1, x))))
all_horz <- unlist(lapply(1:nrow(mat), function(x) sapply(1:(ncol(mat)-1), function(i) sprintf("%s,%s-%s,%s", x, i, x, i+1))))

all_int <- c(all_vert, all_horz)

int_esc <- setNames(rep("?", length(all_int)), all_int)

unsure_sum <- sum(int_esc == "?")

while(TRUE) {
    for(int in names(int_esc)[int_esc == "?"]) {
        if(int %in% blocked_int) {
            int_esc[[int]] <- "X"
            next
        }
        
        tiles <- strsplit(int, "-")[[1]]
        
        if(dist_from_edge(tiles[1]) == 0 || dist_from_edge(tiles[2]) == 0) {
            int_esc[[int]] <- "O"
            next
        }
        
        coords <- lapply(tiles, coord2num)
        
        # if horizontal int, need to look N/S 
        if(coords[[1]][2] != coords[[2]][2]) {
            int_n <- sprintf("%s,%s-%s,%s", coords[[1]][1] - 1, coords[[1]][2],
                             coords[[2]][1] - 1, coords[[2]][2])
            
            int_s <- sprintf("%s,%s-%s,%s", coords[[1]][1] + 1, coords[[1]][2],
                             coords[[2]][1] + 1, coords[[2]][2])
            
            if(int_n %in% names(int_esc) && int_esc[[int_n]] == "O") {
                int_esc[[int]] <- "O"
            }        
            
            if(int_s %in% names(int_esc) && int_esc[[int_s]] == "O") {
                int_esc[[int]] <- "O"
            }
        }
        
        # If vertical, look E/W
        if(coords[[1]][1] != coords[[2]][1]) {
            int_e <- sprintf("%s,%s-%s,%s", coords[[1]][1], coords[[1]][2] - 1,
                             coords[[2]][1], coords[[2]][2] - 1)
            
            int_w <- sprintf("%s,%s-%s,%s", coords[[1]][1], coords[[1]][2] + 1,
                             coords[[2]][1], coords[[2]][2] + 1)
            
            if(int_e %in% names(int_esc) && int_esc[[int_e]] == "O") {
                int_esc[[int]] <- "O"
            }        
            
            if(int_w %in% names(int_esc) && int_esc[[int_w]] == "O") {
                int_esc[[int]] <- "O"
            }
            
            ints <- int_esc[c(int_e, int_w)]
            ints <- ints[!is.na(ints)]
            
            if(length(ints) > 0 && all(ints == "X")) {  # Both directions are blocked
                int_esc[[int]] <- "X"  
            }    
        }
    }
    
    if(sum(int_esc == "?") == unsure_sum) {
        break
    }
    
    unsure_sum <- sum(int_esc == "?")
    print(unsure_sum)
}

int_esc[int_esc == "?"] <- "X"

# Now, check uncertain tiles to see if any of their bordering intersections is O
# (has an escape route)

unsure_tiles <- tile_esc[tile_esc == "?"]

for(tile in names(unsure_tiles)) {
    coord <- coord2num(tile)
    
    ints <- c(sprintf("%s-%s,%s", tile, coord[1] + 1, coord[2]),
              sprintf("%s-%s,%s", tile, coord[1], coord[2] + 1),
              sprintf("%s,%s-%s", coord[1] - 1, coord[2], tile),
              sprintf("%s,%s-%s", coord[1], coord[2] - 1, tile))
    
    if(any(int_esc[ints] == "O", na.rm = T)) {
        tile_esc[tile] <- "O"
    }
}

sum(tile_esc == "?")

# Repeat loop for tiles

unsure_sum <- Inf
while(TRUE) {
    for(coord in names(tile_esc)) {
        if(dist_from_edge(coord) == 0) {
            tile_esc[[coord]] <- "O"
            next
        }
        
        xy <- coord2num(coord)
        
        adj_map <- list(`N` = c(xy[1] - 1, xy[2]),
                        `S` = c(xy[1] + 1, xy[2]),
                        `W` = c(xy[1], xy[2] - 1),
                        `E` = c(xy[1], xy[2] + 1))
        
        adj_coords <- sapply(adj_map, function(x) sprintf("%s,%s", x[1], x[2]))
        
        for(ac in adj_coords) {
            if(ac %in% names(tile_esc) && tile_esc[[ac]] == "O") {
                tile_esc[[coord]] <- "O"
            }   
        }
    }
    
    if(sum(tile_esc == "?") == unsure_sum) {
        break
    }
    
    unsure_sum <- sum(tile_esc == "?")
}

# Export to Excel

map_export <- copy(map)
map_export[names(map_export) %in% names(tile_esc)[tile_esc == "?"]] <- "1"

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

mat_export <- map2mat(map_export)
# mat_export[mat_export == "L"] <- "└"
# mat_export[mat_export == "F"] <- "┌"
# mat_export[mat_export == "J"] <- "┘"
# mat_export[mat_export == "7"] <- "┐"
# mat_export[mat_export == "|"] <- "│"
# mat_export[mat_export == "-"] <- "─"

fwrite(mat_export, "~/aoc/2023/10.csv", col.names = F)

# Part 1 ------------------------------------------------------------------

# BFS to find max distance

vis <- list()
vis[[starting_tile]] <- 0
Q <- starting_tile

while(length(Q) > 0) {
    
    curr <- Q[1]
    
    if(length(Q) == 1) {
        Q <- c()
    } else {
        Q <- c(Q[2:length(Q)])
    }
    
    nb <- get_adjacent(map_main, curr)
    nb_coords <- sapply(nb, function(x) sprintf("%s,%s", x[1], x[2]))
    
    nb_coords <- nb_coords[!(nb_coords %in% names(vis))]
    
    if(length(nb_coords) > 0) {
        Q <- c(Q, nb_coords)
        for(nb_c in nb_coords) {
            vis[[nb_c]] <- vis[[curr]] + 1
        }
    }
}

max(unlist(vis))

