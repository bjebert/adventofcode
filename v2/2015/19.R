
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

# inp <- strsplit("H => HO
# H => OH
# O => HH", "\n")[[1]]  # Example
# text <- "HOHOHO"

inp <- get_input("2015/19", parse = F, user = "blakeebets", cache = T)

trans <- inp[1:43]
text <- inp[45]

map <- list()

for(line in trans) {
    ls <- strsplit(line, " => ")[[1]]
    map[[ls[1]]] <- c(map[[ls[1]]], ls[[2]])
}

outs <- c()

for(x in names(map)) {
    matches <- as.numeric(gregexpr(x, text)[[1]])
    
    if(matches[1] != -1) {
        for(w in matches) {
            for(replace in map[[x]]) {
                if(w == 1) {
                    newstr <- paste0(replace, substr(text, w+nchar(x), nchar(text)))
                } else if(w == nchar(text)-nchar(x)+1) {
                    newstr <- paste0(substr(text, 1, w-1), replace)
                } else {
                    newstr <- paste(c(substr(text, 1, w-1), replace, substr(text, w+nchar(x), nchar(text))), collapse = "")
                }
                
                outs <- c(outs, newstr)
            }
        }
    }
    
    outs <- unique(outs)
}

length(outs)

# 14:36 (43rd)


# Part 2 ------------------------------------------------------------------

inp <- get_input("2015/19", parse = F, user = "blakeebets", cache = T)
trans <- inp[1:43]
target <- inp[45]

map <- list()

for(line in trans) {
    ls <- strsplit(line, " => ")[[1]]
    map[[ls[1]]] <- c(map[[ls[1]]], ls[[2]])
}

# Transformations function ------------------------------------------------

get_transformations <- function(text, map) {
    outs <- c()
    
    for(x in names(map)) {
        matches <- as.numeric(gregexpr(x, text)[[1]])
        
        if(matches[1] != -1) {
            for(w in matches) {
                for(replace in map[[x]]) {
                    if(w == 1) {
                        newstr <- paste0(replace, substr(text, w+nchar(x), nchar(text)))
                    } else if(w == nchar(text)-nchar(x)+1) {
                        newstr <- paste0(substr(text, 1, w-1), replace)
                    } else {
                        newstr <- paste(c(substr(text, 1, w-1), replace, substr(text, w+nchar(x), nchar(text))), collapse = "")
                    }
                    
                    outs <- c(outs, newstr)
                }
            }
        }
    }
    
    return(unique(outs))
}


# Build DFS Forwards ------------------------------------------------------
MAX_CHAR <- 12

Q_fwd <- setNames(0, "e")
v_fwd <- copy(Q_fwd)
iter <- 0

while(length(Q_fwd) > 0) {
    iter <- iter + 1
    curr <- Q_fwd[1]
    Q_fwd <- Q_fwd[-1]
    
    # Get transformations
    transformations <- get_transformations(names(curr), map)
    newQ_fwd <- setNames(rep(curr + 1, length(transformations)), transformations)
    
    if(length(newQ_fwd) == 0) {
        next
    }
    
    is_valid <- sapply(1:length(newQ_fwd), function(i) {
        if(nchar(names(newQ_fwd)[i]) > MAX_CHAR) {
            return(FALSE)
        }
        
        is_unvisited <- !(names(newQ_fwd)[i] %in% names(v_fwd))
        is_lesser <- !is_unvisited && newQ_fwd[i] < v_fwd[[names(newQ_fwd)[i]]]
        
        return(is_unvisited || is_lesser)
    })    
    
    if(!any(is_valid)) {
        next
    }
    
    newQ_fwd <- newQ_fwd[is_valid]
    
    v_fwd <- c(v_fwd, newQ_fwd)
    Q_fwd <- c(Q_fwd, newQ_fwd)
    
    if(iter %% 50 == 0) {
        nc <- sapply(names(Q_fwd), nchar)
        # Q_fwd <- Q_fwd[order(-nc)]
        print(sprintf("%d/%d/%d", length(Q_fwd), length(v_fwd), min(nc)))
    }
}

# Build DFS Backwards -----------------------------------------------------

revmap <- list()

for(line in trans) {
    ls <- strsplit(line, " => ")[[1]]
    revmap[[ls[2]]] <- c(revmap[[ls[2]]], ls[[1]])
}

for(k in 1:100) {
    Q <- setNames(0, inp[45])
    target <- "e"
    min_steps <- Inf
    iter <- 0
    
    while(length(Q) > 0 && iter < 2000) {
        iter <- iter + 1
        curr <- Q[1]
        Q <- Q[-1]
        
        # Instead of searching for the target, we will also     
        if(nchar(names(curr)) <= MAX_CHAR && names(curr) %in% names(v_fwd)) {
            ans_steps <- v_fwd[[names(curr)]] + curr
            if(ans_steps < min_steps) {
                print('*****************')
                beepr::beep()
                print(sprintf("%d/%d", ans_steps, iter))
                min_steps <- ans_steps
            }
            next
        }
        
        if(curr >= min_steps) {
            next
        }
        
        # Get transformations
        transformations <- get_transformations(names(curr), revmap)
        newQ <- setNames(rep(curr + 1, length(transformations)), transformations)
        
        if(length(newQ) == 0) {
            next
        }
        
        # Add randomisation into solution - critical for "greedy" search to work for this input
        newQ <- newQ[order(sapply(names(newQ), nchar), runif(length(newQ)))]
        Q <- c(newQ, Q)
    }
}

# 2:14:33 (65th)
# (solved once again in Python luckily due to non-determinism of sorting sets)
# (non-deterministic solution in a loop added to R)
# (bullshit problem and solution)
