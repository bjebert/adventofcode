
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")
i <- 1

# Problem -----------------------------------------------------------------


inp <- get_input("2024/18", parse = F, user = "bjebert", cache = F)
KB <- 1024
size <- 70

# inp <- strsplit("5,4
# 4,2
# 4,5
# 3,0
# 2,1
# 6,3
# 2,4
# 1,5
# 0,6
# 3,3
# 2,6
# 5,1
# 1,2
# 5,5
# 2,5
# 6,5
# 1,4
# 0,4
# 6,4
# 1,1
# 6,1
# 1,0
# 0,5
# 1,6
# 2,0", "\n")[[1]]  # Example
# KB <- 12
# size <- 6

for(k in 2500:length(inp)) {
    if(solve(k)) {
        print(inp[k])
        break
    } else {
        print(k)
    }
}


solve <- function(k) {
    walls <- inp[1:k]
    
    pos <- "0,0"
    target <- sprintf("%s,%s", size, size)
    
    Q <- list(list(pos = pos, path = pos))
    v <- pos
    
    best <- Inf
    
    while(length(Q) > 0) {
        curr <- Q[[1]]
        Q <- Q[-1]
        
        pos <- curr[["pos"]]
        path <- curr[["path"]]
        
        steps <- length(path)
        
        if(steps >= best) {
            next
        }
        
        if(pos == target) {
            if(steps < best) {
                best <- steps
            }
            
            next
        }
        
        nb <- get_4nb_str(pos)
        nb <- nb[!(nb %in% v)]
        nb <- nb[!(nb %in% walls)]
        
        if(length(nb) == 0) {
            next
        }
        
        nb <- nb[sapply(nb, function(n) {
            all(str2pos(n) >= 0) && all(str2pos(n) <= size)
        })]
        
        for(n in nb) {
            s <- list(pos = n, path = c(path, n))
            Q[[length(Q) + 1]] <- s
            v <- c(v, n)
        }
    }
    
    return(is.infinite(best))
}
