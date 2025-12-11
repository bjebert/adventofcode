
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inr <- get_input("2025/09", parse = F, user = "bjebert", cache = F)
inx <- strsplit("7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3", "\n")[[1]]


# Part 1 ------------------------------------------------------------------

inp <- inx
inp <- inr

n <- t(sapply(strsplit(inp, ","), as.numeric))

e <- sapply(1:(nrow(n)-1), function(i) {
    sapply((i+1):nrow(n), function(j) {
        Reduce(`*`, abs(n[i,] - n[j,]) + 1)
    })
})

max(sapply(e, max))

# Part 2 ------------------------------------------------------------------

is_valid <- function(i, j) {
    if(j == i + 1 || (i == 1 && j == N)) return(TRUE)
    
    rect_x <- sort(n[c(i,j), 1])
    rect_y <- sort(n[c(i,j), 2])
    
    # Along each edge of the rectangle, find the range that isn't part of an existing wall
    
    # Vertical walls: ---
    # rect_x[1], rect_y[1]:rect_y[2]
    vy1 <- sort(n[n[, 1] == rect_x[1], ][, 2])
    
    # rect_x[2], rect_y[1]:rect_y[2]
    vy2 <- sort(n[n[, 1] == rect_x[2], ][, 2])
    
    # Horizontal walls: ---
    # rect_x[1]:rect_x[2], rect_y[1]
    hx1 <- sort(n[n[, 2] == rect_y[1], ][, 1])
    
    # rect_x[1]:rect_x[2], rect_y[2]
    hx2 <- sort(n[n[, 2] == rect_y[2], ][, 1])
    
    get_search_segment <- function(rect_b, wall) {
        #' One of the bounds of each of these walls always connects to our rectangle
        
        if(identical(rect_b, wall)) {
            # Wall goes the whole way along rectangle edge, so no search needed (it's valid.)
            return(NULL)
        }
        
        if(rect_b[1] == wall[1]) {
            if(wall[2] > rect_b[2]) {
                return(NULL)  # Wall fully contains edge
            }
            
            return(c(wall[2] + 0.5, rect_b[2] - 0.5))
        } else if(rect_b[2] == wall[2]) {
            if(wall[1] < rect_b[1]) {
                return(NULL)  # Wall fully contains edge
            }
            
            return(c(rect_b[1] + 0.5, wall[1] - 0.5))
        } else {
            # No overlap between rectangle bounds and wall
            return(c(rect_b[1] + 0.5, rect_b[2] - 0.5))
        }
    }
    
    search_y1 <- get_search_segment(rect_y, vy1)
    search_y2 <- get_search_segment(rect_y, vy2)
    search_x1 <- get_search_segment(rect_x, hx1)
    search_x2 <- get_search_segment(rect_x, hx2)
    
    cast_bounds <- function(search, fixed, y = TRUE) {
        if(is.null(search)) {
            return(TRUE)
        }
        
        d <- if(y) 1 else 2
        candidates <- if(y) edges[["vert"]] else edges[["horz"]]
        
        # Only look for candidates in a certain direction (right for y=TRUE, down for y=FALSE)
        candcast <- candidates[sapply(candidates, function(x) x[[1]][d]) > fixed]
        bounds <- lapply(candcast, function(cand) sapply(cand, function(x) x[3-d]))        
        
        if(length(bounds) == 0) {
            return(FALSE)
        }
        
        bounds <- bounds[order(sapply(bounds, function(x) x[1]))]
        
        # Remove bounds/walls that are entirely outside our search range.
        bounds <- bounds[sapply(bounds, function(x) x[1] <= search[2] & x[2] >= search[1])]
        
        if(length(bounds) == 0) {
            return(FALSE)
        }
        
        # We need every number inside search to hit bounds an odd number of times
        if(length(bounds) == 1) {
            bounds_odd <- TRUE
        } else {
            # See if this hack works?
            bounds_table <- table(unlist(bounds))
            
            middle_good <- length(bounds_table) <= 2 ||
                all(bounds_table[2:(length(bounds_table)-1)] == 2)
            
            bounds_odd <- head(bounds_table, 1) == 1 &&
                tail(bounds_table, 1) == 1 && middle_good
        }
        
        bounds_contain_search <- bounds[[1]][1] <= search[1] && bounds[[length(bounds)]][2] >= search[2]
        
        return(bounds_odd && bounds_contain_search)
    }
    
    cast_bounds(search_y1, rect_x[1], y = TRUE) && 
        cast_bounds(search_y2, rect_x[2], y = TRUE) &&
        cast_bounds(search_x1, rect_y[1], y = FALSE) && 
        cast_bounds(search_x2, rect_y[2], y = FALSE)
}


get_edges <- function(n) {
    edges <- list(horz = NULL, vert = NULL)
    
    for(i in 1:N) {
        j <- if(i == N) 1 else i+1
        edge <- list(n[i, ], n[j, ])
        
        if(n[i, 1] == n[j, 1]) {
            edges[["vert"]][[length(edges[["vert"]]) + 1]] <- edge[order(sapply(edge, function(x) x[2]))]
        } else if(n[i, 2] == n[j, 2]) {
            edges[["horz"]][[length(edges[["horz"]]) + 1]] <- edge[order(sapply(edge, function(x) x[1]))]
        } else {
            stop()
        }
    }
    
    edges[["horz"]] <- edges[["horz"]][order(sapply(edges[["horz"]], function(x) x[[1]][2]), sapply(edges[["horz"]], function(x) x[[1]][1]))]
    edges[["vert"]] <- edges[["vert"]][order(sapply(edges[["vert"]], function(x) x[[1]][1]), sapply(edges[["vert"]], function(x) x[[1]][2]))]
    
    return(edges)
}


inp <- inr
n <- t(sapply(strsplit(inp, ","), as.numeric))
N <- nrow(n)
edges <- get_edges(n)
best <- 0


for(i in 1:(N-1)) {
    for(j in (i+1):N) {
        size <- Reduce(`*`, abs(n[i,] - n[j,]) + 1)
        if(size <= best) {
            next
        }
        
        if(is_valid(i, j)) {
            print(c(i, j))
            best <- size
            print(sprintf("BEST: %s", best))
        }
    }
}


# 144547308 (43,203): valid, but I think we can do bigger
# 272424405: wrong: outside box.

i <- 219
j <- 249
p <- n[c(i,j), ]

plot(n[,1], n[,2], type = "l", xaxt = "n", yaxt = "n")
axis(side = 1, at = p[, 1])
axis(side = 2, at = p[, 2])
abline(h = p[,2], lty = 2, col = 'blue')
abline(v = p[,1], lty = 2, col = 'blue')