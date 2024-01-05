rm(list=ls())
source("utilities.R")
inp <- get_input("2023/22", parse = F, deesblake = F, cache = T)

inp <- strsplit("1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9", "\n")[[1]]

bricks1 <- lapply(strsplit(sapply(strsplit(inp, "~"), function(x) x[1]), ","), as.numeric)
bricks2 <- lapply(strsplit(sapply(strsplit(inp, "~"), function(x) x[2]), ","), as.numeric)

bricks <- lapply(1:length(bricks1), function(i) {
    list(start = bricks1[[i]], end = bricks2[[i]])
})

# Sort bricks so that closest to ground is first
bricks <- bricks[order(sapply(bricks, function(x) min(c(x[["start"]][3], x[["end"]][3]))))]

# Part 1 ------------------------------------------------------------------

# the ground is at z=0, lowest z value for bricks is 1

has_xy_overlap <- function(brick1, brick2) {
    x_overlap <- brick2[["start"]][1] <= brick1[["end"]][1] && brick2[["end"]][1] >= brick1[["start"]][1]
    y_overlap <- brick2[["start"]][2] <= brick1[["end"]][2] && brick2[["end"]][2] >= brick1[["start"]][2]
    return(x_overlap && y_overlap)
}


get_end_pos <- function(bricks) {
    while(TRUE) {
        num_changes <- 0
        for(i in 1:length(bricks)) {
            if(bricks[[i]][["start"]][3] == 1 || bricks[[i]][["end"]][3] == 1) {
                next
            }
            
            min_z <- min(c(bricks[[i]][["start"]][3], bricks[[i]][["end"]][3]))
            
            # Only need to check blocks beneath (they are now sorted by ascending Z values so can use index)
            if(i > 1) {
                overlapping_blocks <- bricks[1:(i-1)][sapply(1:(i-1), function(j) has_xy_overlap(bricks[[i]], bricks[[j]]))]
                z_vals <- sapply(overlapping_blocks, function(j) max(c(j[["start"]][3], j[["end"]][3])))
                z_vals <- z_vals[z_vals < min_z]
            } else {
                z_vals <- list()
            }
            
            z_delta <- 0
            
            if(length(z_vals) == 0) {  
                z_delta <- min_z - 1  # Fall to ground
            } else {
                z_delta <- min_z - max(z_vals) - 1  # Fall to nearest block beneath
            }
            
            if(z_delta > 0) {
                bricks[[i]][["start"]][3] <- bricks[[i]][["start"]][3] - z_delta
                bricks[[i]][["end"]][3] <- bricks[[i]][["end"]][3] - z_delta
                num_changes <- num_changes + 1
            }
        }
        
        if(num_changes == 0) {
            return(bricks)
        }
    }
}

bricks <- get_end_pos(copy(bricks))
bricks <- bricks[order(sapply(bricks, function(x) min(c(x[["start"]][3], x[["end"]][3]))))]


get_supports <- function(bricks) {
    supports <- lapply(1:length(bricks), function(i) {
        if(bricks[[i]][["start"]][3] == 1 || bricks[[i]][["end"]][3] == 1) {
            return(NULL)
        }
        
        w_overlap <- which(sapply(1:(i-1), function(j) {
            has_xy_overlap(bricks[[i]], bricks[[j]])
        }))
        
        w_max_z <- sapply(w_overlap, function(w) max(c(bricks[[w]][["start"]][3], bricks[[w]][["end"]][3])))
        
        return(w_overlap[which(w_max_z == max(w_max_z))])
    })
    
    return(supports)
}

supports <- get_supports(bricks)


# Part 1 ------------------------------------------------------------------

sum(sapply(1:length(bricks), function(i) {
    !any(sapply(supports, function(x) length(x) == 1 && x == i))
}))

# Part 2 ------------------------------------------------------------------

base_zero <- sum(sapply(supports, length) == 0)

sum(sapply(1:length(bricks), function(i) {
    tmp <- copy(supports)
    rmv <- i
    
    while(length(rmv) > 0) {
        w <- which(sapply(tmp, function(x) rmv[1] %in% x))
        tmp <- lapply(tmp, function(x) x[x != rmv[1]])
        
        rmv <- rmv[-1]
        rmv <- c(rmv, w[sapply(tmp[w], length) == 0])
    }
    
    sum(sapply(tmp, length) == 0) - base_zero
}))
