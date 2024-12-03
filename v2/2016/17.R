
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example

check_doors <- function(path, inp) {
    if(length(path) == 0) {
        hash <- md5(inp)
    } else {
        hash <- md5(sprintf("%s%s", inp, paste(path, collapse = "")))
    }
    
    strsplit(substr(hash, 1, 4), "")[[1]] %in% letters[2:6]  # U, D, L, R
}


# inp <- get_input("2016/17", parse = F, user = "blakeebets", cache = T)
# 
# start <- "1,1"
# inp <- "ulqzkmiv"
# 
# Q <- list(list(pos = start, path = NULL))
# V <- setNames(0, start)
# 
# shortest <- Inf
# i <- 0
# 
# while(length(Q) > 0) {
#     curr <- Q[[1]]
#     Q <- Q[-1]
# 
#     pos <- curr[["pos"]]
#     path <- curr[["path"]]
# 
#     if(pos == "4,4") {
#         if(length(path) < shortest) {
#             shortest <- length(path)
#             print(shortest)
#             print(paste(path, collapse = ""))
#         }
# 
#         next
#     }
# 
#     nb <- get_4nb_coord(pos)  # D, U, R, L
#     nb <- nb[c(2, 1, 4, 3)]  # U, D, L, R
# 
#     inside_grid <- !(sapply(nb, function(x) any(coord2pos(x) > 4 | coord2pos(x) < 1)))
#     
#     path_order <- c("U", "D", "L", "R")
#     potential <- lapply(1:4, function(i) list(pos = nb[[i]], path = c(path, path_order[i])))
# 
#     doors <- check_doors(path, inp)  # U, D, L, R
#     potential <- potential[doors & inside_grid]
#     
#     for(p in potential) {
#         # hash <- sprintf("%s|%s", p[["pos"]], p[["path"]])
#         #
#         # if(!(hash %in% names(V))) {
#         #     V[[hash]] <- length(p[["path"]])
#         # } else if(length(p[["path"]]) < V[[hash]]) {
#         #     V[[hash]] <- length(p[["path"]])
#         # }
# 
#         Q <- c(Q, list(p))
#     }
# 
#     # Sort by distance to goal
#     i <- i + 1
#     if(i %% 50 == 0) {
#         Q <- Q[order(sapply(Q, function(x) sum(abs(coord2pos(x[["pos"]]) - c(4, 4)))))]
#     }
# }

# 19:18 (100th)


# part 2 ------------------------------------------------------------------


inp <- get_input("2016/17", parse = F, user = "blakeebets", cache = T)

start <- "1,1"
inp <- "kglvqrro"

Q <- list(list(pos = start, path = NULL))
V <- setNames(0, start)

longest <- 0
i <- 0

while(length(Q) > 0) {
    curr <- Q[[1]]
    Q <- Q[-1]
    
    pos <- curr[["pos"]]
    path <- curr[["path"]]
    
    if(pos == "4,4") {
        if(length(path) > longest) {
            longest <- length(path)
            print(longest)
        }
        
        next
    }
    
    nb <- get_4nb_coord(pos)  # D, U, R, L
    nb <- nb[c(2, 1, 4, 3)]  # U, D, L, R
    
    inside_grid <- !(sapply(nb, function(x) any(coord2pos(x) > 4 | coord2pos(x) < 1)))
    
    path_order <- c("U", "D", "L", "R")
    potential <- lapply(1:4, function(i) list(pos = nb[[i]], path = c(path, path_order[i])))
    
    doors <- check_doors(path, inp)  # U, D, L, R
    potential <- potential[doors & inside_grid]
    
    for(p in potential) {
        # hash <- sprintf("%s|%s", p[["pos"]], p[["path"]])
        # 
        # if(!(hash %in% names(V))) {
        #     V[[hash]] <- length(p[["path"]])
        # } else if(length(p[["path"]]) < V[[hash]]) {
        #     V[[hash]] <- length(p[["path"]])
        # }
        
        Q <- c(list(p), Q)
    }
    
    # Sort by distance to goal
    i <- i + 1
    if(i %% 500 == 0) {
        Q <- Q[order(sapply(Q, function(x) sum(abs(coord2pos(x[["pos"]]) - c(4, 4)))))]
    }
}

# 25:37 (85th)



