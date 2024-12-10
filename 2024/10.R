# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("012345
123456
234567
345678
4.6789
56789.", "\n")[[1]]  # Example
inp <- get_input("2024/10", parse = F, user = "bjebert", cache = F)

map <- mat2map(inp2mat(inp))
heads <- names(map)[map == "0"]


get_rating <- function(head) {
    paths <- list()
    
    Q <- list(list(pos = head, num = 0, path = head))
    
    while(length(Q) > 0) {
        curr <- Q[[1]]
        pos <- curr[["pos"]]
        num <- curr[["num"]]
        path <- curr[["path"]]
        
        Q <- Q[-1]
        
        if(map[[pos]] == "9") {
            paths[[length(paths) + 1]] <- path
            next
        }
        
        nb <- get_4nb_str(pos)
        nb <- nb[nb %in% names(map)]
        
        nb <- nb[map[nb] == as.numeric(num) + 1]
        
        if(length(nb) > 0) {
            new <- lapply(nb, function(x) list(pos = x, num = as.numeric(num) + 1,
                                               path = c(path, x)))
            
            Q <- c(Q, new)
        }
    }
    
    return(length(unique(paths)))
}




get_score <- function(head) {
    tops <- c()
    
    Q <- list(list(pos = head, num = 0))
    v <- head
    
    while(length(Q) > 0) {
        curr <- Q[[1]]
        pos <- curr[["pos"]]
        num <- curr[["num"]]
        
        Q <- Q[-1]
        
        if(map[[pos]] == "9") {
            tops <- c(tops, pos)
            next
        }
        
        nb <- get_4nb_str(pos)
        nb <- nb[nb %in% names(map)]
        nb <- nb[!(nb %in% v)]
        
        nb <- nb[map[nb] == as.numeric(num) + 1]
        
        if(length(nb) > 0) {
            new <- lapply(nb, function(x) list(pos = x, num = as.numeric(num) + 1))
            
            Q <- c(Q, new)
            v <- c(v, nb)
        }
    }
    
    return(length(tops))
}

sum(sapply(heads, get_score))
sum(sapply(heads, get_rating))
