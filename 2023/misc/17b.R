rm(list=ls())
source("utilities.R")
inp <- get_input("2023/17", parse = F, deesblake = F, cache = T)



inp <- strsplit("2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533", "\n")[[1]]

mat <- inp2mat(inp)
map <- mat2map(mat)

# Part 2 ------------------------------------------------------------------

get_neighbours2 <- function(v, pos, dir, steps, loss) {
    nb <- c()
    
    if(dir != "L" && (steps != 3 || dir != "R")) {
        new_pos <- pos + c(0, 1)
        new_steps <- if(dir == "R") steps + 1 else 1
        new_loss <- loss + as.numeric(map[pos2coord(new_pos)])
        
        if(pos2coord(new_pos) %in% names(map)) {
            new_states <- sprintf("%s,%s,R,%s", new_pos[1], new_pos[2], 1:new_steps)  
            curr_state <- new_states[length(new_states)]
            
            if(new_pos[1] == nrow(mat) && new_pos[2] == ncol(mat)) {
                new_states <- sprintf("%s,%s", new_pos[1], new_pos[2])
                curr_state <- copy(new_states)
            }
            
            if(any(new_states %in% names(v))) {
                if(new_loss < min(v[new_states], na.rm = T)) {
                    v[[curr_state]] <<- new_loss
                    nb[curr_state] <- new_loss
                }
            } else {
                nb[curr_state] <- new_loss
            }
        }
    }
    
    if(dir != "U" && (steps != 3 || dir != "D")) {
        new_pos <- pos + c(1, 0)
        new_steps <- if(dir == "D") steps + 1 else 1
        new_loss <- loss + as.numeric(map[pos2coord(new_pos)])
        
        if(pos2coord(new_pos) %in% names(map)) {
            new_states <- sprintf("%s,%s,D,%s", new_pos[1], new_pos[2], 1:new_steps)  
            curr_state <- new_states[length(new_states)]
            
            if(new_pos[1] == nrow(mat) && new_pos[2] == ncol(mat)) {
                new_states <- sprintf("%s,%s", new_pos[1], new_pos[2])
                curr_state <- copy(new_states)
            }
            
            if(any(new_states %in% names(v))) {
                if(new_loss < min(v[new_states], na.rm = T)) {
                    v[[curr_state]] <<- new_loss
                    nb[curr_state] <- new_loss
                }
            } else {
                nb[curr_state] <- new_loss
            }
        }
    }
    
    if(dir != "R" && (steps != 3 || dir != "L")) {
        new_pos <- pos + c(0, -1)
        new_steps <- if(dir == "L") steps + 1 else 1
        new_loss <- loss + as.numeric(map[pos2coord(new_pos)])
        
        if(pos2coord(new_pos) %in% names(map)) {
            new_states <- sprintf("%s,%s,L,%s", new_pos[1], new_pos[2], 1:new_steps)  
            curr_state <- new_states[length(new_states)]
            
            if(any(new_states %in% names(v))) {
                if(new_loss < min(v[new_states], na.rm = T)) {
                    v[[curr_state]] <<- new_loss
                    nb[curr_state] <- new_loss
                }
            } else {
                nb[curr_state] <- new_loss
            }
        }
    } 
    
    if(dir != "D" && (steps != 3 || dir != "U")) {
        new_pos <- pos + c(-1, 0)
        new_steps <- if(dir == "U") steps + 1 else 1
        new_loss <- loss + as.numeric(map[pos2coord(new_pos)])
        
        if(pos2coord(new_pos) %in% names(map)) {
            new_states <- sprintf("%s,%s,U,%s", new_pos[1], new_pos[2], 1:new_steps)  
            curr_state <- new_states[length(new_states)]
            
            if(any(new_states %in% names(v))) {
                if(new_loss < min(v[new_states], na.rm = T)) {
                    v[[curr_state]] <<- new_loss
                    nb[curr_state] <- new_loss
                }
            } else {
                nb[curr_state] <- new_loss
            }
        }
    }
    
    return(nb)
}



init <- c("1,1,X,0" = 0)
v <- init
Q <- init

while(length(Q) >= 1) {
    # print(length(Q))
    curr <- Q[1]
    Q <- Q[-1]

    state <- strsplit(names(curr), ","[[1]])[[1]]

    pos <- as.numeric(state[1:2])
    dir <- state[3]
    steps <- as.numeric(state[4])
    loss <- unname(curr)

    if(pos[1] == nrow(mat) && pos[2] == ncol(mat)) {
        print(sprintf("%s: %s", Sys.time(), loss))
        next
    }

    # get neighbours
    nb <- get_neighbours(v, pos, dir, steps, loss)

    if(length(nb) > 0) {
        for(i in 1:length(nb)) {
            v[names(nb)[i]] <- nb[i]
        }

        Q <- c(Q, nb)

        # Periodically remove all duplicates with higher value
        if(length(Q) %% 20 == 0) {
            u <- unique(names(Q))
            u_val <- sapply(u, function(x) min(Q[names(Q) == x]))

            Q <- setNames(u_val, u)

            # Sort Q to favour right/down directions first, then positions closest to exit
            Qpos1 <- as.numeric(sub("^([^,]*,){0}([^,]*).*", "\\2", names(Q)))
            Qpos2 <- as.numeric(sub("^([^,]*,){1}([^,]*).*", "\\2", names(Q)))
            dists <- nrow(mat) - Qpos1 + ncol(mat) - Qpos2

            Q <- Q[order(dists, c(`R` = 1, `D` = 1, `L` = 2, `U` = 2)[sub("^([^,]*,){2}([^,]*).*", "\\2", names(Q))])]
        }
    }
}

target <- sprintf("%s,%s", nrow(mat), ncol(mat))
sort(v[substr(names(v), 1, nchar(target)) == target])



# Part 1 ------------------------------------------------------------------

# get_neighbours <- function(v, pos, dir, steps, loss) {
#     nb <- c()
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
#             if(new_pos[1] == nrow(mat) && new_pos[2] == ncol(mat)) {
#                 new_states <- sprintf("%s,%s", new_pos[1], new_pos[2])
#                 curr_state <- copy(new_states)
#             }
#             
#             if(any(new_states %in% names(v))) {
#                 if(new_loss < min(v[new_states], na.rm = T)) {
#                     v[[curr_state]] <<- new_loss
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
#             if(new_pos[1] == nrow(mat) && new_pos[2] == ncol(mat)) {
#                 new_states <- sprintf("%s,%s", new_pos[1], new_pos[2])
#                 curr_state <- copy(new_states)
#             }
#             
#             if(any(new_states %in% names(v))) {
#                 if(new_loss < min(v[new_states], na.rm = T)) {
#                     v[[curr_state]] <<- new_loss
#                     nb[curr_state] <- new_loss
#                 }
#             } else {
#                 nb[curr_state] <- new_loss
#             }
#         }
#     }
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
#                     v[[curr_state]] <<- new_loss
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
#                     v[[curr_state]] <<- new_loss
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
# Q <- init
# 
# while(length(Q) >= 1) {
#     # print(length(Q))
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
#     if(pos[1] == nrow(mat) && pos[2] == ncol(mat)) {
#         print(sprintf("%s: %s", Sys.time(), loss))
#         next
#     }
#     
#     # get neighbours
#     nb <- get_neighbours(v, pos, dir, steps, loss)
#     
#     if(length(nb) > 0) {
#         for(i in 1:length(nb)) {
#             v[names(nb)[i]] <- nb[i]
#         }
#         
#         Q <- c(Q, nb)
#         
#         # Periodically remove all duplicates with higher value
#         if(length(Q) %% 20 == 0) {
#             u <- unique(names(Q))
#             u_val <- sapply(u, function(x) min(Q[names(Q) == x]))
#             
#             Q <- setNames(u_val, u)
#             
#             # Sort Q to favour right/down directions first, then positions closest to exit
#             Qpos1 <- as.numeric(sub("^([^,]*,){0}([^,]*).*", "\\2", names(Q)))
#             Qpos2 <- as.numeric(sub("^([^,]*,){1}([^,]*).*", "\\2", names(Q)))
#             dists <- nrow(mat) - Qpos1 + ncol(mat) - Qpos2
#             
#             Q <- Q[order(dists, c(`R` = 1, `D` = 1, `L` = 2, `U` = 2)[sub("^([^,]*,){2}([^,]*).*", "\\2", names(Q))])]
#         }
#     }
# }
# 
# target <- sprintf("%s,%s", nrow(mat), ncol(mat))
# sort(v[substr(names(v), 1, nchar(target)) == target])

