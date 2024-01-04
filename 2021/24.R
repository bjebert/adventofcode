input <- readLines("24.txt")

# input <- strsplit("sesenwnenenewseeswwswswwnenewsewsw
# neeenesenwnwwswnenewnwwsewnenwseswesw
# seswneswswsenwwnwse
# nwnwneseeswswnenewneswwnewseswneseene
# swweswneswnenwsewnwneneseenw
# eesenwseswswnenwswnwnwsewwnwsene
# sewnenenenesenwsewnenwwwse
# wenwwweseeeweswwwnwwe
# wsweesenenewnwwnwsenewsenwwsesesenwne
# neeswseenwwswnwswswnw
# nenwswwsewswnenenewsenwsenwnesesenew
# enewnwewneswsewnwswenweswnenwsenwsw
# sweneswneswneneenwnewenewwneswswnese
# swwesenesewenwneswnwwneseswwne
# enesenwswwswneneswsenwnewswseenwsese
# wnwnesenesenenwwnenwsewesewsesesew
# nenewswnwewswnenesenwnesewesw
# eneswnwswnwsenenwnwnwwseeswneewsenese
# neswnwewnwnwseenwseesewsenwsweewe
# wseweeenwnesenwwwswnew", "\n")[[1]]

blacks <- c()

for(line in input) {
    
    dirs <- c()
    i <- 1
    while(i <= nchar(line)) {
        if(substr(line, i, i) %in% c("n", "s")) {
            dirs <- c(dirs, substr(line, i, i+1))
            i <- i + 2
        } else {
            dirs <- c(dirs, substr(line, i, i))
            i <- i + 1
        }
    }
    
    # Counter balance easts/wests
    dir_e <- sum(dirs == "e") - sum(dirs == "w")
    
    # Counter balance NW/SE
    dir_nw <- sum(dirs == "nw") - sum(dirs == "se")
    
    # Counter balance NE/SW
    dir_ne <- sum(dirs == "ne") - sum(dirs == "sw")
    
    hex_e <- dir_e - (dir_nw / 2) + (dir_ne / 2)
    hex_n <- dir_nw + dir_ne
    
    hex_key <- sprintf("%s|%s", hex_e, hex_n)
    
    if(hex_key %in% blacks) {
        blacks <- blacks[blacks != hex_key]
    } else {
        blacks <- c(blacks, hex_key)
    }
}


# Generate tiles for part 2 -----------------------------------------------

get_adjacents <- function(tile) {
    coords <- as.numeric(strsplit(tile, "\\|")[[1]])
    coords_list <- list(c(coords[1] - 1, coords[2]),
                        c(coords[1] + 1, coords[2]),
                        c(coords[1] - 0.5, coords[2] + 1),
                        c(coords[1] - 0.5, coords[2] - 1),
                        c(coords[1] + 0.5, coords[2] + 1),
                        c(coords[1] + 0.5, coords[2] - 1))
    
    sapply(coords_list, function(x) sprintf("%s|%s", x[1], x[2]))
}

tiles_grid <- as.data.table(expand.grid(east = seq(-100, 100, 0.5),
                                        north = -100:100))

# if N/S is even, then east must be whole, if N/S is odd, then east/west must be .5
tiles_grid <- tiles_grid[!(north %% 2 == 0 & east %% 1 == 0.5)]
tiles_grid <- tiles_grid[!(north %% 2 == 1 & east %% 1 == 0)]

tiles_keys <- tiles_grid[, sprintf("%s|%s", east, north)] 
tiles_map <- lapply(1:length(tiles_keys), function(x) 1)
names(tiles_map) <- tiles_keys
tiles_map[names(tiles_map) %in% blacks] <- 0

for(day in 1:100) {
    new_map <- copy(tiles_map)
    for(tile in names(tiles_map)) {
        
        adjacents <- unlist(tiles_map[get_adjacents(tile)])
        black_adjacents <- sum(adjacents == 0)
        
        if(length(adjacents) < 6 && black_adjacents > 0) {
            stop("Need more tiles!")
        }
        
        if(black_adjacents == 2 && tiles_map[[tile]] == 1) {
            new_map[[tile]] <- 0
        } else if((black_adjacents == 0 || black_adjacents > 2) && tiles_map[[tile]] == 0) {
            new_map[[tile]] <- 1
        }
    }
    
    print(sprintf("%d: %d", day, sum(new_map == 0)))
    
    tiles_map <- copy(new_map)
}