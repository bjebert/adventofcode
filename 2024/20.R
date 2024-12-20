
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- get_input("2024/20", parse = F, user = "bjebert", cache = F)

inp <- strsplit("###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############", "\n")[[1]]  # Example
inp

map <- mat2map(inp2mat(inp))

start <- names(map)[map == "S"]
end <- names(map)[map == "E"]
walls <- names(map)[map == '#']

ground <- setdiff(names(map), walls)

# cheats <- unique(unname(unlist(lapply(walls, function(w) {
#     nb <- get_4nb_str(w)
#     wn <- which(map[nb] %in% c(".", "S", "E"))
#     
#     if(length(wn) > 0) {
#         nb <- names(map[nb])[wn]
#         
#         return(lapply(nb, function(x) {
#             sprintf("%s|%s", w, x)
#         }))
#     }
#     return(NULL)
# }))))


cheats_part2 <- function(size = 20) {
    star_0 <- matrix(unlist(sapply(0:size, function(i) {
        sapply(0:(size-i), function(j) {
            c(i, j)
        })
    })), ncol = 2, byrow = T)
    
    # can also move negative directions, just absolute travelled must be <= size
    star_a <- star_0 * matrix(rep(c(1, -1), nrow(star_0)), ncol = 2, byrow = T)
    star_b <- star_0 * matrix(rep(c(-1, 1), nrow(star_0)), ncol = 2, byrow = T)
    star_c <- star_0 * matrix(rep(c(-1, -1), nrow(star_0)), ncol = 2, byrow = T)
    
    star <- unique(Reduce(`rbind`, list(star_0, star_a, star_b, star_c)))
    star_size <- rowSums(abs(star))
    
    cheats <- unlist(lapply(ground, function(g) {
        end_pos <- apply(star + matrix(rep(str2pos(g), nrow(star)), ncol = 2, byrow = T), 
                         1, pos2str)
        
        w <- !(end_pos %in% c(g, walls))
        end_pos <- end_pos[w]
        cheat_len <- star_size[w]
        
        lapply(1:length(end_pos), function(x) c(g, end_pos[x], cheat_len[x]))
    }), recursive = F)
    
    return(cheats)
}


solve_init <- function() {
    Q <- list(list(pos = start, path = start))
    v <- start
    
    while(length(Q) > 0) {
        curr <- Q[[1]]
        Q <- Q[-1]
        
        pos <- curr[["pos"]]
        path <- curr[["path"]]
        
        if(pos == end) {
            return(setNames(0:(length(path)-1), rev(path)))
        }
        
        nb <- get_4nb_str(pos)
        nb <- nb[!(nb %in% walls)]
        nb <- nb[!(nb %in% v)]
        
        if(length(nb) > 0) {
            Q <- c(Q, lapply(nb, function(n) list(pos = n, path = c(path, n))))
            v <- unique(c(v, nb))
        }
    }
}

step_map <- solve_init()

solve_cheat <- function(step_map, cheat_start, cheat_end, cheat_len) {
    delta <- step_map[[cheat_start]] - step_map[[cheat_end]] - as.numeric(cheat_len)
    delta <- delta[!is.na(delta)]
    return(max(delta))
}


cheats <- cheats_part2()

# matrix-ize to speed up
cmat <- matrix(unlist(cheats), ncol = 3, byrow = T)
cmat <- cmat[cmat[,2] %in% names(step_map), ]
delta <- step_map[cmat[,1]] - step_map[cmat[,2]] - as.numeric(cmat[,3])

sum(delta >= 100)




