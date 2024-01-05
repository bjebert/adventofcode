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


# Part 1 ------------------------------------------------------------------

test_area <- c(7, 27)

for(i in 1:(length(positions) - 1)) {
    for(j in (i+1):length(positions)) {
        px_i <- positions[[i]][1]
        py_i <- positions[[i]][2]
        
        px_j <- positions[[j]][1]
        py_j <- positions[[j]][2]
        
        vx_i <- velos[[i]][1]
        vy_i <- velos[[i]][2]
        
        vx_j <- velos[[j]][1]
        vy_j <- velos[[j]][2]
        
        
        
        # Can't loop/check discrete, must be a continuous check for path collision
        ggplot(data.table(t = 0:4), aes(x = t)) +
            stat_function(fun = function(x) 19 - 2*x, color = '#FF3300') +  
            stat_function(fun = function(x) 18 - x, color = '#0033FF') +   
            stat_function(fun = function(x) 13 + x, color = '#FF0033') +
            stat_function(fun = function(x) 19 - x, color = '#3300FF') +
            geom_vline(xintercept = c(7/3, 11/3), linetype = 'dashed')
        
        # Essentially need to form a box
        # (14.33, 2.33), (15.33, 2.33), (14.33, 3.66), (15.33, 3.66)
        # Binary search on t to try and find it?  Is there no equation that solves it?
        
        
        # But how?
        # Need to solve eqn?
        #Hailstone A: 19, 13, 30 @ -2, 1, -2
        #Hailstone B: 18, 19, 22 @ -1, -1, -2
        #Hailstones' paths will cross inside the test area (at x=14.333, y=15.333)
        
        # ans is t=7/3 (2.3333) for h1, t=11/3 (3.6666) for h2
        t <- 7/3
        px_i + vx_i * t
        py_i + vy_i * t
        
        t <- 11/3
        px_j + vx_j * t
        py_j + vy_j * t
        
    }
}

i <- 1
j <- 2
positions
velos
>>>>>>> 8432427dbae880ca00b960a45fc36be61a89c179
