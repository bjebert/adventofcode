rm(list=ls())
source("utilities.R")
inp <- get_input("2023/3", parse = F, deesblake = F, cache = F)

inp <- strsplit("467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..", "\n")[[1]]

mat <- matrix(strsplit(paste(inp, collapse = ""), "")[[1]], nrow = length(inp), byrow = T)
s <- 0

for(i in 1:nrow(mat)) {
    j <- 1
    
    num_started <- FALSE
    num <- c()
    coords <- list()
    
    while(j <= ncol(mat)) {
        if(!is.na(as.numeric(mat[i, j]))) {
            num_started <- TRUE
            num <- c(num, mat[i, j])
            coords[[length(coords) + 1]] <- c(i, j)
        } else {
            if(num_started) {
                final_num <- as.numeric(paste(num, collapse = ""))
                
                # check adjacents
                nb <- c(unlist(sapply(coords, function(cd) {
                    
                    adj <- list(c(cd[1] - 1, cd[2]),
                                c(cd[1], cd[2]),
                                c(cd[1] + 1, cd[2]),
                                c(cd[1] - 1, cd[2] - 1),
                                c(cd[1], cd[2] - 1),
                                c(cd[1] + 1, cd[2] - 1),
                                c(cd[1] - 1, cd[2] + 1),
                                c(cd[1], cd[2] + 1),
                                c(cd[1] + 1, cd[2] + 1))
                    
                    adj <- unique(lapply(adj, function(a) {
                        c(min(max(a[1], 1), nrow(mat)), min(max(a[2], 1), ncol(mat)))
                    }))
                    
                    sapply(adj, function(a) {
                        mat[a[1], a[2]]
                    })
                })))
                
                sym <- nb[!(nb %in% c(as.character(0:9), "."))]
                if(length(sym) >= 1) {
                    s <- s + final_num
                    print(sprintf("Adding num %d", final_num))
                } else {
                    print(sprintf("Rejecting num %d", final_num))
                }
            }
            
            num_started <- FALSE
            num <- c()
            coords <- list()
        }
        
        j <- j + 1
        
        if(j > ncol(mat) && num_started) {
            final_num <- as.numeric(paste(num, collapse = ""))
            
            # check adjacents
            nb <- c(unlist(sapply(coords, function(cd) {
                
                adj <- list(c(cd[1] - 1, cd[2]),
                            c(cd[1], cd[2]),
                            c(cd[1] + 1, cd[2]),
                            c(cd[1] - 1, cd[2] - 1),
                            c(cd[1], cd[2] - 1),
                            c(cd[1] + 1, cd[2] - 1),
                            c(cd[1] - 1, cd[2] + 1),
                            c(cd[1], cd[2] + 1),
                            c(cd[1] + 1, cd[2] + 1))
                
                adj <- unique(lapply(adj, function(a) {
                    c(min(max(a[1], 1), nrow(mat)), min(max(a[2], 1), ncol(mat)))
                }))
                
                sapply(adj, function(a) {
                    mat[a[1], a[2]]
                })
            })))
            
            sym <- nb[!(nb %in% c(as.character(0:9), "."))]
            if(length(sym) >= 1) {
                s <- s + final_num
                print(sprintf("Adding num %d", final_num))
            } else {
                print(sprintf("Rejecting num %d", final_num))
            }
        }
    }
}

s


# Part 2 ------------------------------------------------------------------

x <- which(mat == "*") %% nrow(mat)
y <- which(mat == "*") %/% nrow(mat) + 1
s <- 0

for(i in 1:length(x)) {
    
    cd <- c(x[i], y[i])
    
    adj <- list(c(cd[1] - 1, cd[2]),
                c(cd[1], cd[2]),
                c(cd[1] + 1, cd[2]),
                c(cd[1] - 1, cd[2] - 1),
                c(cd[1], cd[2] - 1),
                c(cd[1] + 1, cd[2] - 1),
                c(cd[1] - 1, cd[2] + 1),
                c(cd[1], cd[2] + 1),
                c(cd[1] + 1, cd[2] + 1))
    
    adj <- unique(lapply(adj, function(a) {
        c(min(max(a[1], 1), nrow(mat)), min(max(a[2], 1), ncol(mat)))
    }))
    
    res <- unique(lapply(adj, function(a) {
        if(!is.na(as.numeric(mat[a[1], a[2]]))) {
            # Look left and right to determine the number and its bounds
            bounds <- list(a)
            
            b_left <- a[2]
            b_right <- a[2]
            
            while(b_left >= 1 && !is.na(as.numeric(mat[a[1], b_left]))) {
                bounds[[length(bounds) + 1]] <- c(a[1], b_left)
                b_left <- b_left - 1
            }            
            
            while(b_right <= ncol(mat) && !is.na(as.numeric(mat[a[1], b_right]))) {
                bounds[[length(bounds) + 1]] <- c(a[1], b_right)
                b_right <- b_right + 1
            }
            
            bounds <- unique(bounds)
            bounds <- bounds[order(sapply(bounds, function(x) x[2]))]
            
            num <- as.numeric(paste(sapply(bounds, function(b) mat[b[1], b[2]]), collapse = ""))
            
            return(list(num, bounds))
        }
        
    }))
    
    gears <- res[!sapply(res, is.null)]
    
    if(length(gears) == 2) {
        s <- s + gears[[1]][[1]] * gears[[2]][[1]]
    }

}
