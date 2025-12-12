
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inr <- get_input("2025/12", parse = F, user = "bjebert", cache = F)
inx <- strsplit("0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2", "\n")[[1]]


# Input -------------------------------------------------------------------

inp <- inx
inp <- inr

w <- which(inp == "")
rdim <- lapply(strsplit(sapply(strsplit(inp[(max(w)+1):length(inp)], ": "), function(x) x[1]), "x"), as.numeric)
rpres <- lapply(strsplit(sapply(strsplit(inp[(max(w)+1):length(inp)], ": "), function(x) x[2]), " "), as.numeric)
shapes <- lapply(1:length(w), function(i) {
    if(i == 1) inp[1:(w[i] - 1)] else inp[(w[i-1]+1):(w[i]-1)]
})
idx <- as.numeric(gsub(":", "", sapply(shapes, function(x) x[1])))
mats <- setNames(lapply(lapply(shapes, function(x) x[2:length(x)]), function(x) inp2mat(x)), idx)

# Part 1 ------------------------------------------------------------------



fits <- function(d, num, mats) {
    
    start <- matrix(".", nrow = d[2], ncol = d[1])
    Q <- list(list(grid = start, nums = num, hist = NULL))
    iter <- 1
    
    while(length(Q) > 0) {
        curr <- Q[[1]]
        Q <- Q[-1]
        
        grid <- curr[["grid"]]
        nums <- curr[["nums"]]
        
        if(sum(nums) == 0) {
            return(curr)
        }
        
        w_req <- which(nums > 0)
        for(k in 1:length(mats)) {
            if(!(k %in% w_req)) {
                next
            }
            
            n <- mats[[k]]
            combos <- unique(list(n, rotate(n), rotate(rotate(n)), rotate(rotate(rotate(n)))))
            
            for(combo in combos) {
                # Start top-left on any empty grid coordinate
                for(i in 1:(nrow(grid) - nrow(combo) + 1)) {
                    for(j in 1:(ncol(grid) - ncol(combo) + 1)) {
                        if(grid[i, j] == ".") {
                            w_clash <- intersect(which(combo == '#'), which(grid[i:(i+nrow(combo)-1),j:(j+ncol(combo)-1)] == '#'))
                            
                            if(length(w_clash) == 0) {
                                new <- copy(grid)
                                new[i:(i+nrow(combo)-1),j:(j+ncol(combo)-1)][which(combo == '#')] <- "#"
                                
                                nn <- copy(nums)
                                nn[k] <- nn[k] - 1
                                
                                Q <- c(list(list(grid = new, nums = nn, hist = c(curr[["hist"]], list(new)))), Q)
                            }
                        }
                    }
                }
            }
        }
        
        iter <- iter + 1
        
        if(iter %% 100 == 0) {
            Q <- unique(Q)
            Q <- Q[order(sapply(Q, function(x) sum(x[["nums"]])))]
            print(length(Q))
        }
    }
    
    return(FALSE)
}



i <- 1
d <- rdim[[i]]
num <- rpres[[i]]

fits(rdim[[i]], rpres[[i]], mats)


sum(sapply(1:length(rdim), function(i) {
    d <- rdim[[i]]
    num <- rpres[[i]]
    sum(num * sapply(mats, function(x) sum(x == '#'))) <= prod(d)
}))

