
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################", "\n")[[1]]  # Example


inp <- get_input("2024/16", parse = F, user = "bjebert", cache = F)
inp

map <- mat2map(inp2mat(inp))

start <- names(map)[map == "S"]
end <- names(map)[map == "E"]
walls <- names(map)[map == '#']

Q <- list(list(pos = start, facing = 'E', score = 0, path = start))
v <- setNames(0, sprintf("%s|%s", start, "E"))

best <- 65436
turnmap <- list('E' = c('N', 'S'), 'N' = c('E', 'W'), 'W' = c('N', 'S'), 'S' = c('E', 'W'))
seats <- NULL

while(length(Q) > 0) {
    curr <- Q[[1]]
    Q <- Q[-1]
    
    pos <- curr[["pos"]]
    score <- curr[["score"]]
    facing <- curr[["facing"]]
    path <- curr[["path"]]
    
    if(score > best) {
        next
    }
    
    if(pos == end) {
        if(score == best) {
            seats <- unique(c(seats, curr[["path"]]))
            print(sprintf("***%s***", length(seats)))
        }
        
        next
    }
    
    # either move forwards or turn
    # move fwd
    newstates <- list()
    newpos <- pos2str(str2pos(pos) + move_map[[facing]])
    
    if(!(newpos %in% walls)) {
        newstates[[length(newstates) + 1]] <- list(pos = newpos, facing = facing, score = score + 1,
                                                   path = c(path, newpos))
    }
    
    
    # turn either way
    for(f in turnmap[[facing]]) {
        # optimisation - never turn into a wall
        if(!(pos2str(str2pos(pos) + move_map[[f]]) %in% walls)) {
            newstates[[length(newstates) + 1]] <- list(pos = pos, facing = f, score = score + 1000,
                                                       path = path)    
        }
    }
    
    if(length(newstates) == 0) {
        next
    }
    
    # check if in v
    
    newstates <- newstates[sapply(newstates, function(s) {
        
        # dont visit a state we have already been to
        sid <- sprintf("%s|%s", s[["pos"]], s[["facing"]])
        
        if(sid %in% names(v) && v[[sid]] < s[["score"]]) {
            return(FALSE)
        }
        return(T)
    })]
    
    if(length(newstates) > 0) {
        Q <- c(newstates, Q)
        
        for(s in newstates) {
            sid <- sprintf("%s|%s", s[["pos"]], s[["facing"]])
            v[[sid]] <- s[["score"]]
        }
    }
}
