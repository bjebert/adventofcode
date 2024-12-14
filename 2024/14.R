
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- get_input("2024/14", parse = F, user = "bjebert", cache = F)
width <- 101
height <- 103

# inp <- strsplit("p=0,4 v=3,-3
# p=6,3 v=-1,-3
# p=10,3 v=-1,2
# p=2,0 v=2,-1
# p=0,0 v=1,3
# p=3,0 v=-2,-2
# p=7,6 v=-1,-3
# p=3,0 v=-1,-2
# p=9,3 v=2,3
# p=7,3 v=-1,2
# p=2,4 v=2,-3
# p=9,5 v=-3,-3", "\n")[[1]]  # Example

# width <- 11
# height <- 7

p1 <- as.numeric(gsub("p=", "", sapply(strsplit(inp, ","), function(x) x[1])))
p2 <- as.numeric(sapply(strsplit(sapply(strsplit(inp, ","), function(x) x[2]), " "), function(x) x[1]))

z <- sapply(strsplit(inp, "="), function(x) x[3])
v1 <- sapply(strsplit(z, ","), function(x) as.numeric(x[1]))
v2 <- sapply(strsplit(z, ","), function(x) as.numeric(x[2]))

p <- t(sapply(1:length(p1), function(i) c(p1[i], p2[i])))
v <- t(sapply(1:length(p1), function(i) c(v1[i], v2[i])))

deltas <- NULL

for(step in 1:7603) {
    p <- p + v
    p[,1] <- p[,1] %% width
    p[,2] <- p[,2] %% height
    
    delta <- sum(sapply(1:nrow(p), function(i) {
        sum(abs(p[i,] - p))
    }))

    print(step)
    print(delta)
    # plot(m)
    
    deltas <- c(deltas, delta)
}

if(delta < 12000000) {
    m <- matrix(0, nrow = height, ncol = width)
    for(x in 1:nrow(p)) {
        m[p[x,2]+1, p[x,1]+1] <- 1
    }
    
    plot(m)
}

ww <- floor(width / 2) - 1
wh <- floor(height / 2) - 1

quads <- list(list(0:ww, 0:wh), 
     list((ww+2):(width-1), 0:wh),
     list(0:ww, (wh+2):(height-1)),
     list((ww+2):(width-1), (wh+2):(height-1))
)

prod(sapply(quads, function(q) {
    sum(sapply(1:nrow(p), function(x) {
        p[x,1] %in% q[[1]] && p[x,2] %in% q[[2]]
    }))
}))
