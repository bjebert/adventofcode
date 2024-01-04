rm(list=ls())
source("utilities.R")
inp <- get_input("2023/13", parse = F, deesblake = F, cache = T)

inp <- strsplit("#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#", "\n")[[1]]


sep <- which(nchar(inp) == 0)

mats <- lapply(1:(length(sep) + 1), function(i) {
    start <- if(i == 1) 1 else sep[i - 1] + 1
    end <- if(i <= length(sep)) sep[i] - 1 else length(inp)
    
    inp2mat(inp[start:end])
})


# Part 1 ------------------------------------------------------------------


check_vert <- function(mat) {
    for(i in 1:(ncol(mat) - 1)) {
        # Vertical line is between columns i and i+1
        
        left <- mat[, 1:i]
        right <- mat[, (i+1):ncol(mat)]
        
        # Make the halves equal number of cols
        if(class(left)[1] == "character") {
            right <- right[,1]
        } else if(class(right)[1] == "character") {
            left <- left[, ncol(left)]
        } else {
            size <- min(ncol(left), ncol(right))
            
            left <- left[, (ncol(left) - size + 1):ncol(left)]
            right <- right[, size:1]  # Flip right side
        }
        
        if(identical(left, right)) {
            return(i)
        }
    }
    
    return(NA)
}


check_horiz <- function(mat) {
    for(i in 1:(nrow(mat) - 1)) {
        # Horizontal line is between rows i and i+1
        
        upper <- mat[1:i, ]
        lower <- mat[(i+1):nrow(mat), ]
        
        # Make the halves equal number of cols
        if(class(upper)[1] == "character") {
            lower <- lower[1,]
        } else if(class(lower)[1] == "character") {
            upper <- upper[nrow(upper), ]
        } else {
            size <- min(nrow(upper), nrow(lower))
            
            upper <- upper[(nrow(upper) - size + 1):nrow(upper), ]
            lower <- lower[size:1, ]  # Flip lower side
        }
        
        if(identical(upper, lower)) {
            return(i)
        }
    }
    
    return(NA)
}


solve <- function(mat) {
    vert <- check_vert(mat)
    if(!is.na(vert)) {
        return(vert)
    } else {
        horiz <- check_horiz(mat)
        return(horiz * 100)
    }
}

sum(sapply(mats, solve))

# Part 2 ------------------------------------------------------------------


check_vert2 <- function(mat) {
    for(i in 1:(ncol(mat) - 1)) {
        # Vertical line is between columns i and i+1
        
        left <- mat[, 1:i]
        right <- mat[, (i+1):ncol(mat)]
        
        # Make the halves equal number of cols
        if(class(left)[1] == "character") {
            right <- right[,1]
        } else if(class(right)[1] == "character") {
            left <- left[, ncol(left)]
        } else {
            size <- min(ncol(left), ncol(right))
            
            left <- left[, (ncol(left) - size + 1):ncol(left)]
            right <- right[, size:1]  # Flip right side
        }
        
        if(sum(left != right) == 1) {
            return(i)
        }
    }
    
    return(NA)
}


check_horiz2 <- function(mat) {
    for(i in 1:(nrow(mat) - 1)) {
        # Horizontal line is between rows i and i+1
        
        upper <- mat[1:i, ]
        lower <- mat[(i+1):nrow(mat), ]
        
        # Make the halves equal number of cols
        if(class(upper)[1] == "character") {
            lower <- lower[1,]
        } else if(class(lower)[1] == "character") {
            upper <- upper[nrow(upper), ]
        } else {
            size <- min(nrow(upper), nrow(lower))
            
            upper <- upper[(nrow(upper) - size + 1):nrow(upper), ]
            lower <- lower[size:1, ]  # Flip lower side
        }
        
        if(sum(upper != lower) == 1) {
            return(i)
        }
    }
    
    return(NA)
}


solve2 <- function(mat) {
    vert <- check_vert2(mat)
    if(!is.na(vert)) {
        return(vert)
    } else {
        horiz <- check_horiz2(mat)
        return(horiz * 100)
    }
}

sum(sapply(mats, solve2))
