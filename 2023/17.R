rm(list=ls())
source("utilities.R")
inp <- get_input("2023/17", parse = F, deesblake = F, cache = T)


# inp <- strsplit("2413432311323
# 3215453535623
# 3255245654254
# 3446585845452
# 4546657867536
# 1438598798454
# 4457876987766
# 3637877979653
# 4654967986887
# 4564679986453
# 1224686865563
# 2546548887735
# 4322674655533", "\n")[[1]]

mat <- inp2mat(inp)
map <- mat2map(mat)


# Part 2 ------------------------------------------------------------------


get_neighbour <- function(curr, v, new_pos, new_dir, new_steps) {
    new_loss <- v[curr] + as.numeric(map[pos2coord(new_pos)])
    
    if(new_pos[1] >= 1 && new_pos[1] <= nrow(mat) && new_pos[2] >= 1 && new_pos[2] <= ncol(mat)) {
        new_state <- sprintf("%s,%s,%s,%s", new_pos[1], new_pos[2], new_dir, new_steps)  
        
        if(new_state %in% names(v)) {
            if(new_loss < v[new_state]) {
                return(setNames(new_loss, new_state))
            }
        } else {
            return(setNames(new_loss, new_state))
        }
    }
    
    return(NULL)
}


get_neighbours2 <- function(curr, v, pos, dir, steps) {
    pos_map <- list(`R` = c(0, 1),
                    `D` = c(1, 0),
                    `L` = c(0, -1),
                    `U` = c(-1, 0))
    
    inverse_map <- c(`R` = "L", `L` = "R", `U` = "D", `D` = "U")
    
    if(dir == "X") {
        possible_dirs <- c("R", "D")
    } else if(steps < 4) {
        possible_dirs <- dir        
    } else {
        possible_dirs <- names(inverse_map)[names(inverse_map) != inverse_map[[dir]]]  # Cannot "reverse"
            
        if(steps >= 10) {
            possible_dirs <- possible_dirs[possible_dirs != dir]  # Must turn
        }
    }
    
    nb <- unlist(sapply(possible_dirs, function(new_dir) {
        new_pos <- pos + pos_map[[new_dir]]
        new_steps <- if(dir == new_dir) steps + 1 else 1
        get_neighbour(curr, v, new_pos, new_dir, new_steps)
    }, USE.NAMES = F))
    
    return(nb)
}


init <- c("1,1,X,0" = 0)
v <- init
Q <- names(init)


while(length(Q) >= 1) {
    minQ <- min(v[Q])
    
    if(length(v) %% 100 == 0) {
        print(sprintf("%s/%s/%s", length(Q), length(v), minQ))
    }
    
    # choose vertex with smallest distance first (Djikstra's)
    curr <- names(v[Q])[which(v[Q] == minQ)[1]]
    Q <- Q[-which(Q == curr)]
    
    state <- strsplit(curr, ","[[1]])[[1]]
    
    pos <- as.numeric(state[1:2])
    dir <- state[3]
    steps <- as.numeric(state[4])
    
    if(pos[1] == nrow(mat) && pos[2] == ncol(mat)) {
        next
    }
    
    nb <- get_neighbours2(curr, v, pos, dir, steps)
    
    if(length(nb) > 0) {
        for(i in 1:length(nb)) {
            v[names(nb)[i]] <- nb[i]
        }
        
        Q <- c(Q, names(nb))
    }
    
    # print(v[curr])
    # print(nb)
    # print(v[Q])
}

Q <- readRDS("D:/Q.rds")
v <- readRDS("D:/v.rds")

# saveRDS(Q, "D:/Q.rds")
# saveRDS(v, "D:/v.rds")


target <- sprintf("%s,%s", nrow(mat), ncol(mat))
sort(v[substr(names(v), 1, nchar(target)) == target])

# Takes nearly a full day to run :/


# Part 1 ------------------------------------------------------------------
# 
# get_neighbours <- function(v, pos, dir, steps, loss) {
#     nb <- c()
#     
#     # if target square is already in set with same direction and less/equal number of steps and less/equal loss, then don't add
#     
#     if(dir != "R" && (steps != 3 || dir != "L")) {
#         new_pos <- pos + c(0, -1)
#         new_steps <- if(dir == "L") steps + 1 else 1
#         new_loss <- loss + as.numeric(map[pos2coord(new_pos)])
#         
#         if(pos2coord(new_pos) %in% names(map)) {
#             new_states <- sprintf("%s,%s,L,%s", new_pos[1], new_pos[2], 1:new_steps)  
#             curr_state <- new_states[length(new_states)]
#             
#             if(any(new_states %in% names(v))) {
#                 if(new_loss < min(v[new_states], na.rm = T)) {
#                     nb[curr_state] <- new_loss
#                 }
#             } else {
#                 nb[curr_state] <- new_loss
#             }
#         }
#     } 
#     
#     if(dir != "L" && (steps != 3 || dir != "R")) {
#         new_pos <- pos + c(0, 1)
#         new_steps <- if(dir == "R") steps + 1 else 1
#         new_loss <- loss + as.numeric(map[pos2coord(new_pos)])
#         
#         if(pos2coord(new_pos) %in% names(map)) {
#             new_states <- sprintf("%s,%s,R,%s", new_pos[1], new_pos[2], 1:new_steps)  
#             curr_state <- new_states[length(new_states)]
#             
#             if(any(new_states %in% names(v))) {
#                 if(new_loss < min(v[new_states], na.rm = T)) {
#                     nb[curr_state] <- new_loss
#                 }
#             } else {
#                 nb[curr_state] <- new_loss
#             }
#         }
#     }
#     
#     if(dir != "D" && (steps != 3 || dir != "U")) {
#         new_pos <- pos + c(-1, 0)
#         new_steps <- if(dir == "U") steps + 1 else 1
#         new_loss <- loss + as.numeric(map[pos2coord(new_pos)])
#         
#         if(pos2coord(new_pos) %in% names(map)) {
#             new_states <- sprintf("%s,%s,U,%s", new_pos[1], new_pos[2], 1:new_steps)  
#             curr_state <- new_states[length(new_states)]
#             
#             if(any(new_states %in% names(v))) {
#                 if(new_loss < min(v[new_states], na.rm = T)) {
#                     nb[curr_state] <- new_loss
#                 }
#             } else {
#                 nb[curr_state] <- new_loss
#             }
#         }
#     }
#     
#     if(dir != "U" && (steps != 3 || dir != "D")) {
#         new_pos <- pos + c(1, 0)
#         new_steps <- if(dir == "D") steps + 1 else 1
#         new_loss <- loss + as.numeric(map[pos2coord(new_pos)])
#         
#         if(pos2coord(new_pos) %in% names(map)) {
#             new_states <- sprintf("%s,%s,D,%s", new_pos[1], new_pos[2], 1:new_steps)  
#             curr_state <- new_states[length(new_states)]
#             
#             if(any(new_states %in% names(v))) {
#                 if(new_loss < min(v[new_states], na.rm = T)) {
#                     nb[curr_state] <- new_loss
#                 }
#             } else {
#                 nb[curr_state] <- new_loss
#             }
#         }
#     }
#     
#     return(nb)
# }
# 
# 
# 
# init <- c("1,1,X,0" = 0)
# v <- init
# Q <- names(init)
# 
# while(length(Q) >= 1) {
#     print(sprintf("%s/%s", length(Q), length(v)))
#     curr <- Q[1]
#     Q <- Q[-1]
#     
#     state <- strsplit(names(curr), ","[[1]])[[1]]
#     
#     pos <- as.numeric(state[1:2])
#     dir <- state[3]
#     steps <- as.numeric(state[4])
#     loss <- unname(curr)
#     
#     # get neighbours
#     nb <- get_neighbours(v, pos, dir, steps, loss)
#     if(length(nb) > 0) {
#         for(i in 1:length(nb)) {
#             v[names(nb)[i]] <- nb[i]
#         }
#         
#         Q <- c(Q, nb)
#         
#         # Periodically remove all duplicates with higher value
#         if(length(Q) %% 100 == 0) {
#             u <- unique(names(Q))
#             u_val <- sapply(u, function(x) min(Q[names(Q) == x]))
#             
#             Q <- setNames(u_val, u)
#         }
#     }
# }
# 
# target <- sprintf("%s,%s", nrow(mat), ncol(mat))
# sort(v[substr(names(v), 1, nchar(target)) == target])
# 
# 
