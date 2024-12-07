
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

# inp <- strsplit("The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
# The second floor contains a hydrogen generator.
# The third floor contains a lithium generator.
# The fourth floor contains nothing relevant.", "\n")[[1]]  # Example
# floors <- list(c("HM", "LM"), c("HG"), c("LG"), NULL)

# inp <- get_input("2016/11", parse = F, user = "blakeebets", cache = T)
# floors <- list(c("SG", "SM", "PG", "PM"), c("TG", "RG", "RM", "CG", "CM"), c("TM"), NULL)


is_valid <- function(room) {
    chips <- room[substr(room, 2, 2) == "M"]
    gens <- room[substr(room, 2, 2) == "G"]
    
    if(length(chips) >= 1 && length(gens) >= 1) {
        cn <- substr(chips, 1, 1)
        gn <- substr(gens, 1, 1)
        
        if(!all(sapply(cn, function(x) x %in% gn))) {
            return(F)
        }
    }
    return(T)
}


get_moves <- function(E, floors, steps) {
    moves <- list()
    items <- floors[[E]]    
    
    item_comb <- as.list(items)
    if(length(items) >= 2) {
        item_comb <- c(item_comb, combinat::combn(items, 2, simplify = F))
        item_comb <- item_comb[sapply(item_comb, is_valid)]
    }
    
    # Only select first instance of a pair
    w <- which(sapply(item_comb, function(x) {
        length(x) == 2 && substr(x[1], 1, 1) == substr(x[2], 1, 1)
    }))
    
    if(length(w) > 1) {
        pairs <- item_comb[w]
        
        # Remove single duplicates (e.g. if AG, AM, BG, BM on same floor,
        # then remove moves BM, as we already have AM)
        
        dups <- unlist(item_comb[w[2:length(w)]])
        to_remove <- c(dups, combinat::combn(dups, 2, simplify = F))
        
        # experimental - see if works
        item_comb <- item_comb[!(item_comb %in% to_remove)]
        
        # Remove any pairs after first 
        # item_comb <- item_comb[-w[2:length(w)]]
    }
    
    # Go down 1
    if(E != 1) {
        if(sum(sapply(floors[1:(E-1)], length)) > 0) {
            for(ic in item_comb) {
                # HEURISTIC 2: Never move two items down
                if(length(ic) == 2) {
                    next
                }
                
                # Never move a full pair down
                # if(length(ic) == 2 && substr(ic[1], 1, 1) == substr(ic[2], 1, 1)) {
                #     next
                # }
                
                new_floors <- copy(floors)
                new_floors[[E]] <- setdiff(new_floors[[E]], ic)
                new_floors[[E-1]] <- c(new_floors[[E-1]], ic)
                
                if(is_valid(new_floors[[E]]) && is_valid(new_floors[[E-1]])) {
                    alpha <- floors2alpha(new_floors)
                    moves[[length(moves) + 1]] <- list(floors = alpha, E = E-1, 
                                                       steps = steps+1, hash = floors2hash(alpha, E))
                }
            }
        }
    }
    
    # Go up 1
    if(E != 4) {
        for(ic in item_comb) {
            new_floors <- copy(floors)
            new_floors[[E]] <- setdiff(new_floors[[E]], ic)
            new_floors[[E+1]] <- c(new_floors[[E+1]], ic)
            
            if(is_valid(new_floors[[E]]) && is_valid(new_floors[[E+1]])) {
                alpha <- floors2alpha(new_floors)
                moves[[length(moves) + 1]] <- list(floors = alpha, E = E+1, 
                                                   steps = steps+1, hash = floors2hash(alpha, E))
            }
        }
    }
    
    moves <- unique(moves)
    return(moves)
}


score <- function(floors, steps) {  # higher is better
    csum <- sapply(floors, function(x) {
        sum(substr(x, 2, 2) == "M")
    })
    
    gsum <- sapply(floors, function(x) {
        sum(substr(x, 2, 2) == "G")
    })
    
    sum(csum * 1:4 + gsum * 1:4 * 3)
}


floors2alpha <- function(floors) {
    # pairs take precedence of id, then solos
    floor_ids <- sapply(floors, function(x) substr(x, 1, 1))
    floor_sort <- lapply(floor_ids, function(x) x[order(-(duplicated(x) | duplicated(x, fromLast = TRUE)))])
    
    item_ids <- unique(substr(unlist(floor_sort), 1, 1))
    item_map <- setNames(letters[1:length(item_ids)], item_ids)
    
    lapply(floors, function(x) {
        if(length(x) == 0) return(character(0))
        substr(x, 1, 1) <- item_map[substr(x, 1, 1)]
        sort(x)
    }) 
}


floors2hash <- function(floors, E) {
    floor_hash <- lapply(floors, function(x) {
        if(length(x) == 0) {
            return(".")
        } else {
            return(paste(x, collapse = ","))
        }
    })
    
    sprintf("%s|%s", E, paste(floor_hash, collapse = "|"))
}


too_far_away <- function(floors, steps, max_steps) {
    min_steps_req <- ceiling(sum(sapply(floors, length) * 3:0) / 2)
    return(steps + min_steps_req >= max_steps)
}


start <- floors2alpha(list(c("SG", "SM", "PG", "PM", "DM", "DG", "EG", "EM"), 
                           c("TG", "RG", "RM", "CG", "CM"), 
                           c("TM"), 
                           character(0)))

total_items <- sum(sapply(start, length))
max_steps <- 63

Q <- list(list(floors = copy(start), E = 1, steps = 0, hash = floors2hash(copy(start), 1)))
V <- setNames(0, Q[[1]][["hash"]])

iter <- 0

while(length(Q) > 0) {
    curr <- Q[[1]]
    Q <- Q[-1]
    
    floors <- curr[["floors"]]
    E <- curr[["E"]]
    steps <- curr[["steps"]]
    
    if(steps >= max_steps) {
        next
    }
    
    if(too_far_away(floors, steps, max_steps)) {
        next
    }
    
    # if too far away
    
    if(length(floors[[4]]) == total_items) {
        if(steps < max_steps) {
            max_steps <- steps
            print(sprintf("*********%d*********", steps))
        }
        
        next
    }
    
    moves <- get_moves(E, floors, steps)
    
    # Check if moves in V
    for(move in moves) {
        if(!(move[["hash"]] %in% names(V)) || move[["steps"]] < V[[move[["hash"]]]]) {
            V[[move[["hash"]]]] <- move[["steps"]]
            Q <- c(Q, list(move))
        }
    }
    
    # Heuristic for sorting
    iter <- iter + 1
    
    if(iter %% 200 == 0) {
        # remove dups
        Q <- Q[order(sapply(Q, function(x) x[["steps"]]))]
        Q <- Q[!duplicated(sapply(Q, function(x) x[["hash"]]))]
        
        # heuristic
        Q <- Q[order(sapply(Q, function(x) score(x[["floors"]], x[["steps"]])), decreasing = T)]
        
        # remove too-long
        Q <- Q[sapply(Q, function(x) x[["steps"]] < max_steps)]
        print(length(Q))
    }
}

# 2:55:43 (> 100th)

# 4:49:58 (> 100th)



