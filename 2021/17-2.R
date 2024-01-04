input <- ".###..#.
##.##...
....#.#.
#..#.###
...#...#
##.#...#
#..##.##
#......."

input_mat <- matrix(strsplit(gsub("\n", "", input), "")[[1]], ncol = 8, byrow = T)

#' Part B: we will represent the input inside a list
# Key: 0|-1|0|2 -> W|X|Y|Z

init_sz <- 11

init_key_mat <- sapply(-init_sz:init_sz, function(w) sapply(-init_sz:init_sz, function(x) sapply(-init_sz:init_sz, function(y) sapply(-init_sz:init_sz, function(z) sprintf("%d|%d|%d|%d", w, x, y, z)))))
init_keys <- sapply(init_key_mat, function(x) x)
coordinates <- lapply(init_keys, function(x) ".")
names(coordinates) <- init_keys


# Initialise input --------------------------------------------------------

N <- nrow(input_mat)
for(x in 1:N) {
    for(y in 1:N) {
        co_x <- x - ((N %/% 2) + 1)
        co_y <- y - ((N %/% 2) + 1)
        coordinates[[sprintf("%d|%d|%d|%d", co_x, co_y, 0, 0)]] <- input_mat[x, y]
    }
}


# Update ------------------------------------------------------------------


get_neighbouring_keys <- function(cell) {
    coord_split <- as.numeric(strsplit(cell, "\\|")[[1]])
    
    nb_keys <- sapply(sapply((coord_split[1] - 1):(coord_split[1] + 1), 
                             function(w) sapply((coord_split[2] - 1):(coord_split[2] + 1), 
                                                function(x) sapply((coord_split[3] - 1):(coord_split[3] + 1), 
                                                                   function(y) sapply((coord_split[4] - 1):(coord_split[4] + 1), 
                                                                                      function(z) coords2index(w, x, y, z))))), function(a) a)
    
    nb_keys <- nb_keys[nb_keys != coords2index(coord_split[1], coord_split[2], coord_split[3], coord_split[4])]
    nb_keys <- nb_keys[nb_keys >= 1 & nb_keys <= 23^4]
    
    return(nb_keys)
}


coords2index <- function(w, x, y, z, init_sz = 11) {
    ((z - -init_sz) + (y - -init_sz) * (init_sz * 2 + 1)^1 + (x - -init_sz) * (init_sz * 2 + 1)^2 + (w - -init_sz) * (init_sz * 2 + 1)^3) + 1
}


get_neighbours <- function(cell, coordinates) {
    nb_keys <- get_neighbouring_keys(cell)
    return(sum(coordinates[nb_keys] == "#"))
}


for(i in 1:6) {
    #' Updating every single cell is going to take a while - 279K of them.  If this is going to be too slow, then
    #' we can later try and come up with a faster algorithm which only looks at near-active cells.
    
    new_coordinates <- lapply(init_keys, function(x) ".")
    names(new_coordinates) <- init_keys
    
    tm <- Sys.time()
    for(cell in names(coordinates)) {
        neighbours <- get_neighbours(cell, coordinates)
        
        if(neighbours %in% 2:3) {
            if(coordinates[[cell]] == "." && neighbours == 3) {
                new_coordinates[[cell]] <- "#"
            } else if(coordinates[[cell]] == "#") {
                new_coordinates[[cell]] <- "#"
            }
        }
    }
    print(Sys.time() - tm)
    
    coordinates <- copy(new_coordinates)
}

sum(coordinates == "#")
