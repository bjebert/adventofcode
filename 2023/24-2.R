rm(list=ls())
source("utilities.R")
inp <- get_input("2023/24", parse = F, deesblake = F, cache = T)

inp <- strsplit("19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3", "\n")[[1]]

positions <- lapply(strsplit(sapply(strsplit(inp, " @ "), function(x) x[1]), ", "), as.numeric)
velos <- lapply(strsplit(sapply(strsplit(inp, " @ "), function(x) x[2]), ", "), as.numeric)


# Part 2 ------------------------------------------------------------------

px <- sapply(positions, function(x) x[1])
vx <- sapply(velos, function(x) x[1])

py <- sapply(positions, function(x) x[2])
vy <- sapply(velos, function(x) x[2])

pz <- sapply(positions, function(x) x[3])
vz <- sapply(velos, function(x) x[3])


works <- function(t1, t2, dim = "x", return_rock = F) {
    map <- c("x" = 1, "y" = 2, "z" = 3)
    
    p <- sapply(positions, function(x) x[map[dim]])
    v <- sapply(velos, function(x) x[map[dim]])
    
    # tn = (rp - pn) / (vn - rv) 
    
    p
    v[4] <- 2
    
    ranges <- list()
    for(i in 1:length(p)) {
        ranges[[length(ranges) + 1]] <- list(rp = c(p[i] + 1, Inf),
                                             rv = c(-Inf, v[i] - 1))
        
        ranges[[length(ranges) + 1]] <- list(rp = c(-Inf, p[i] - 1),
                                             rv = c(v[i] + 1, Inf))
    }
    i <- 1
    
    # constraints for each:
    # t1: rp >= 20 & rv <= -3, OR rp <= 18 & rv >= -1
    # t2: rp >= 19 & rv <= -2, OR rp <= 17 & rv >= 0
    # t4: rp >= 13 & rv <= 1,  OR rp <= 11 & rv >= 3
        
    
    
    
    # This is to ensure that no collisions occur before t >= 1
    # possible_rv <- setdiff(-500:500, v)
    possible_rv <- setdiff(-5:5, v)
    possible_rv <- possible_rv[possible_rv != 0]
    
    sapply(possible_rv, function(rv) {
        
        if(rv < 0) {
            rp_min <- max(p+v) - rv
        } else {
            rp_max <- min(p+v) - rv
        }
        
    })
    
    rv <- (p[2] + v[2]*t2 - p[1] - v[1]*t1) / (t2 - t1)
    rp <- p[1] + v[1]*t1 - rv*t1
    
    if(is.na(rv) || is.na(rp) || is.infinite(rv) || is.infinite(rp) || rv %% 1 != 0 || rp %% 1 != 0) {
        return(FALSE)
    }
    
    t_rem <- (rp - p[3:length(p)]) / (v[3:length(p)] - rv)  # t3, t4, t5, ..., tN
    
    (rp - p) / (v - rv)
    
    tx <- c(t1, t2, t_rem)
    
    if(return_rock) {
        return(c(rp, rv))
    }
    
    return(sum(is.na(tx) | is.infinite(tx)) == 0 && all(t_rem %% 1 == 0) && all(t_rem >= 1))
}


for(t1 in 1:500) {
    for(t2 in 1:500) {
        if(works(t1, t2, "x")) {
            print(c(t1, t2))
        }
        
        # if(works(t1, t2, "y") && works(t1, t2, "x") && works(t1, t2, "z")) {
        #     # print(c(t1, t2))
        #     print(works(t1, t2, "x", return_rock = T))
        # }
    }
}




# Infinite solutions on X axis

# Correct is t = (5, 3, 4, 6, 1)


rp = (-2-rv)*k1 + 19
rv = NULL


(-2-rv) + 19

rp+rv >= 21



