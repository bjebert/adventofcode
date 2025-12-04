
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("", "\n")[[1]]  # Example
inp <- get_input("2024/21", parse = F, user = "bjebert", cache = F)

keypad <- strsplit("789
456
123
.0A", "\n")[[1]]

dirpad <- strsplit(".^A
<v>", "\n")[[1]]

k <- mat2map(inp2mat(keypad))
d <- mat2map(inp2mat(dirpad))

k <- k[k != "."]
d <- d[d != "."]

path <- function(x, y, m) {
    start <- names(m)[m == x]
    end <- names(m)[m == y]
    
    Q <- list(list(pos = start, path = NULL))
    paths <- NULL
    best <- Inf
    
    while(length(Q) > 0) {
        curr <- Q[[1]]
        Q <- Q[-1]
        
        pos <- curr[["pos"]]
        path <- curr[["path"]]
        
        steps <- length(path)
        
        if(steps > best) {
            next
        }
        
        if(pos == end) {
            if(steps < best) {
                best <- steps
                paths <- paste(path, collapse = "")
            } else if(steps == best) {
                paths <- c(paths, paste(path, collapse = ""))
            }
            next
        }
        
        nb <- get_4nb_str(pos)
        move <- c("^", "v", ">", "<")
        
        dist1 <- sum(abs(str2pos(pos) - str2pos(end)))
        distn <- sapply(nb, function(p) sum(abs(str2pos(p) - str2pos(end))))
        nb <- nb[distn < dist1]
        move <- move[distn < dist1]
        
        if(length(nb) > 0) {
            w2 <- nb %in% names(m)
            nb <- nb[w2]
            move <- move[w2]
        }
        
        if(length(nb) > 0) {
            Q <- c(Q, lapply(1:length(nb), function(i) {
                list(pos = nb[i], path = c(path, move[i]))
            }))
        }
    }
    return(paths)
}

kmap <- setNames(lapply(unname(k), function(x) {
    setNames(lapply(unname(k)[unname(k) != x], function(y) path(x, y, k)), unname(k)[unname(k) != x])
}), unname(k))

dmap <- setNames(lapply(unname(d), function(x) {
    setNames(lapply(unname(d)[unname(d) != x], function(y) path(x, y, d)), unname(d)[unname(d) != x])
}), unname(d))

solve <- function(goal, map) {
    gs <- c("A", strsplit(goal, "")[[1]])
    
    ways <- NULL
    for(i in 1:(length(gs)-1)) {
        if(gs[[i]] == gs[[i+1]]) {
            paths <- ""
        } else {
            paths <- map[[gs[i]]][[gs[i+1]]]
        }
        
        if(i == 1) {
            ways <- sprintf("%sA", paths)
        } else {
            ways <- unname(c(sapply(ways, function(x) sprintf("%s%sA", x, paths))))
        }
    }
    return(ways)
}

complexity <- function(goal, max_iter = 100) {
    s1 <- solve(goal, kmap)  # robot kmap
    s2 <- unlist(unname(sapply(s1, function(s) solve(s, dmap))))  # robot dmap
    
    iter <- 1
    best <- Inf
    while(iter < max_iter) {
        s <- sample(s2, 1)        
        m <- min(sapply(solve(s, dmap), nchar))
        if(m < best) {
            best <- m
            print(best)
        }
        
        iter <- iter + 1
    }
    
    # s3 <- unlist(unname(sapply(s2, function(s) solve(s, dmap))))  # us
    
    cmp <- best * as.numeric(gsub("A", "", goal))
    print(cmp)
    return(cmp)
}

pmap <- setNames(1:4, c("<", "v", "^", ">"))

choose <- function(paths) {
    if(length(paths) == 1) return(paths)
    ps <- strsplit(paths, "")
    
    changes1 <- sapply(ps, function(p) {
        length(p) - sum(p[1:4] == p[2:5], na.rm = T)
    })
    
    changes2 <- sapply(ps, function(x) pmap[[x[1]]])
    paths[order(changes1, changes2)][1]
}


solve_opt <- function(goal, map) {
    if(goal == "") {
        return("")
    }
    gs <- c("A", strsplit(goal, "")[[1]])
    
    ways <- NULL
    for(i in 1:(length(gs)-1)) {
        if(gs[[i]] == gs[[i+1]]) {
            path <- ""
        } else {
            path <- choose(map[[gs[i]]][[gs[i+1]]])
        }
        
        if(i == 1) {
            ways <- sprintf("%sA", path)
        } else {
            ways <- sprintf("%s%sA", ways, path)
        }
    }
    return(ways)
}


complexity2 <- function(goal, iter = 25) {
    s1 <- solve_opt(goal, kmap)
    for(i in 1:iter) {
        s1 <- solve_opt(s1, dmap)
    }
    # nchar(s1) * as.numeric(gsub("A", "", goal))
    nchar(s1)
}

complexity3 <- function(goal, iter = 25) {
    s1 <- solve_opt(goal, kmap)
    t <- table(s1)
    
    for(k in 1:iter) {
        t <- unlist(lapply(1:length(t), function(i) {
            t2 <- table(strsplit(names(t)[i], "A")[[1]])
            
            setNames(t2 * t[i], sapply(names(t2), function(y) {
                solve_opt(sprintf("%sA", y), dmap)
            }))
        }))
    
        t <- tapply(t, names(t), sum)    
    }
    
    res <- sum(nchar(names(t)) * as.bigz(t))
    res
    res * as.numeric(gsub("A", "", goal))
}

res <- sapply(inp, function(x) complexity3(x, 25))
Reduce(`+`, res)

# test cases --------------------------------------------------------------

complexity3("90", 2) == 37
Reduce(`+`, sapply(inp, function(x) complexity3(x, 2))) == 342
Reduce(`+`, sapply(inp, function(x) complexity3(x, 25))) < 478450602992

