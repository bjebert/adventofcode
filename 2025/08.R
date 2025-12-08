
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inr <- get_input("2025/08", parse = F, user = "bjebert", cache = F)
inx <- strsplit("162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689", "\n")[[1]]


# Part 1 ------------------------------------------------------------------

inp <- inr

nums <- lapply(strsplit(inp, ","), as.numeric)
circ <- NULL
dist <- sapply(nums, function(n1) sapply(nums, function(n2) sqrt(sum((n1 - n2)^2))))
diag(dist) <- Inf

for(i in 1:1000) {
    n1 <- which.min(sapply(1:nrow(dist), function(i) min(dist[i,])))
    n2 <- which.min(dist[n1,])
    
    if(length(circ) == 0) {
        circ[[1]] <- c(n1, n2)   
    } else {
        w1 <- which(sapply(circ, function(x) n1 %in% x))
        w2 <- which(sapply(circ, function(x) n2 %in% x))
        
        if(length(w1) == 1 && length(w2) == 0) {
            # append to w1
            circ[[w1]] <- c(circ[[w1]], n2)
            
        } else if(length(w2) == 1 && length(w1) == 0) {
            # append to w2
            circ[[w2]] <- c(circ[[w2]], n1)
            
        } else if(length(w1) == 1 && length(w2) == 1) {
            if(w1 != w2) {
                # join two groups together
                circ[[w1]] <- unique(c(circ[[w1]], circ[[w2]]))
                circ <- circ[-w2]
            }
        } else {
            circ[[length(circ) + 1]] <- c(n1, n2)
        }
    }
    
    dist[n1, n2] <- Inf
    dist[n2, n1] <- Inf
}

Reduce(`*`, rev(sort(sapply(circ, length)))[1:3])

# Part 2 ------------------------------------------------------------------

inp <- inx
inp <- inr

nums <- lapply(strsplit(inp, ","), as.numeric)
circ <- NULL
dist <- sapply(nums, function(n1) sapply(nums, function(n2) sqrt(sum((n1 - n2)^2))))
diag(dist) <- Inf

while(TRUE) {
    n1 <- which.min(sapply(1:nrow(dist), function(i) min(dist[i,])))
    n2 <- which.min(dist[n1,])
    
    if(length(circ) == 0) {
        circ[[1]] <- c(n1, n2)   
    } else {
        w1 <- which(sapply(circ, function(x) n1 %in% x))
        w2 <- which(sapply(circ, function(x) n2 %in% x))
        
        if(length(w1) == 1 && length(w2) == 0) {
            # append to w1
            circ[[w1]] <- c(circ[[w1]], n2)
            
        } else if(length(w2) == 1 && length(w1) == 0) {
            # append to w2
            circ[[w2]] <- c(circ[[w2]], n1)
            
        } else if(length(w1) == 1 && length(w2) == 1) {
            if(w1 != w2) {
                # join two groups together
                circ[[w1]] <- unique(c(circ[[w1]], circ[[w2]]))
                circ <- circ[-w2]
            }
        } else {
            circ[[length(circ) + 1]] <- c(n1, n2)
        }
    }
    
    dist[n1, n2] <- Inf
    dist[n2, n1] <- Inf
    
    if(length(circ[[1]]) == length(nums)) {
        print(nums[[n1]] * nums[[n2]])
        break
    }
}


