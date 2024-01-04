rm(list=ls())
source("utilities.R")
library(memoise)

inp <- get_input("2023/12", parse = F, deesblake = F, cache = T)
lines <- sapply(strsplit(inp, " "), function(x) x[1])
contig <- lapply(strsplit(sapply(strsplit(inp, " "), function(x) x[2]), ","), as.numeric)


count <- function(chars) {
    groups <- c()
    curr <- "."
    x <- 0
    for(i in 1:length(chars)) {
        if(chars[i] == '#') {
            x <- x + 1
        } else if(curr == '#') {
            groups <- c(groups, x)
            x <- 0
        }
        curr <- chars[i]
    }
    
    if(curr == '#') {
        groups <- c(groups, x)
    }
    
    return(groups)
}


solve <- function(line, groups) {
    line <- gsub("^\\.*|\\.*$", "", paste(line, collapse = ""))
    chars <- strsplit(line, "")[[1]]
    
    if(length(chars) == 0) {
        return(as.numeric(sum(groups) == 0))
    }
    
    w_q <- which(chars == "?")
    
    if(length(w_q) == 0) {
        if(identical(count(chars), groups)) {
            return(1)
        } else {
            return(0)
        }
    }
    
    w_hash <- which(chars == '#')
    
    if(sum(groups) == 0) {
        return(as.numeric(length(w_hash) == 0))
    }
    
    if(sum(groups) > length(w_q) + length(w_hash)) {
        return(0)
    }
    
    w_start <- w_hash[!(w_hash - 1) %in% w_hash]
    w_end <- w_hash[!(w_hash + 1) %in% w_hash]
    
    if(chars[1] == "#") {
        if(length(w_start[1]:w_end[1]) > groups[1]) {
            return(0)
        } 
        
        if(length(w_start[1]:w_end[1]) < groups[1]) {
            
            # Try to auto-fill ?s if part of first group
            if(sum(chars[1:groups[1]] %in% c('#', '?')) == groups[1]) {
                
                # Check to make sure next char is not a #, which would make group too big
                if(groups[1] + 1 <= length(chars) && chars[groups[1] + 1] == '#') {
                    return(0)
                }
                
                if(groups[1] + 2 > length(chars)) {
                    return(solve("", groups[-1]))
                } else {
                    return(solve(chars[(groups[1]+2):length(chars)], groups[-1]))
                }
            } else {
                return(0)
            }
        }
        
        if(w_end[1] + 2 > length(chars)) {
            return(solve("", groups[-1]))
        } else {
            return(solve(chars[(w_end[1]+2):length(chars)], groups[-1]))
        }
    }
    
    if(tail(chars, 1) == "#") {
        if(length(tail(w_start, 1):tail(w_end, 1)) > tail(groups, 1)) {
            return(0)   
        }
        
        if(length(tail(w_start, 1):tail(w_end, 1)) < tail(groups, 1)) {
            
            # Try to auto-fill ?s if part of last group
            if(sum(chars[(length(chars)-tail(groups, 1)+1):length(chars)] %in% c('#', '?')) == tail(groups, 1)) {
                
                # Check to make sure next char is not a #, which would make group too big
                if(length(chars) - tail(groups, 1) >= 1 && chars[length(chars) - tail(groups, 1)] == '#') {
                    return(0)
                }
                
                if(length(chars) - tail(groups, 1) - 1 < 1) {
                    return(solve("", groups[-length(groups)]))
                } else {
                    return(solve(chars[1:(length(chars) - tail(groups, 1) - 1)], groups[-length(groups)]))
                }
            } else {
                return(0)
            }
        }
        
        if(tail(w_start, 1) - 2 < 1) {
            return(solve("", groups[-length(groups)]))
        } else {
            return(solve(chars[1:(tail(w_start, 1)-2)], groups[-length(groups)]))
        }
    }
    
    # Fill in dots on maximum sized groups
    
    if(length(w_start) > 0) {
        w_len <- sapply(1:length(w_start), function(i) length(w_start[i]:w_end[i]))
        
        if(any(w_len > max(groups))) {
            return(0)
        }
        
        w_max <- sapply(1:length(w_start), function(i) w_len[i] == max(groups))
        
        if(length(w_max) > 0) {
            w_must_dot <- sort(c(w_start[w_max] - 1, w_end[w_max] + 1))
            w_must_dot <- w_must_dot[w_must_dot >= 1 & w_must_dot <= length(chars)]
            
            if(length(w_must_dot) >= 0 && sum(chars[w_must_dot] != ".") > 0) {
                chars[w_must_dot] <- "."
                
                return(solve(chars, groups))
            }
        }
    }
    
    if(sum(chars == ".") == 0) {
        if(length(w_hash) == 0) {  # string contains only question marks
            chars_hash <- copy(chars)
            chars_hash[1] <- "#"
            
            return(solve(chars_hash, groups) + solve(chars[-1], groups))
        }
        
        # Try assigning first hash to each group
        potentials <- unique(unlist(lapply(1:length(groups), function(j) {
            x <- groups[j]
            
            sapply(1:x, function(i) {
                idx_start <- w_start[1] - x + i - 1
                idx_end <- w_start[1] + i
                idx_range <- (idx_start + 1):(idx_end - 1)
                
                if(all(idx_range >= 1 & idx_range <= length(chars))) {
                    chars_tmp <- copy(chars)
                    chars_tmp[idx_range] <- '#'
                    
                    if(idx_start >= 1) {
                        if(chars_tmp[idx_start] == "?") {
                            chars_tmp[idx_start] <- "."
                        } else {
                            return(NULL)
                        }
                    }
                    
                    if(idx_end <= length(chars)) {
                        if(chars_tmp[idx_end] == "?") {
                            chars_tmp[idx_end] <- "."
                        } else {
                            return(NULL)
                        }
                    }
                    
                    return(paste(chars_tmp, collapse = ""))
                }
            })
        })))
        
        if(length(potentials) == 0) {
            return(0)
        } else {
            return(sum(sapply(1:length(potentials), function(i) solve(potentials[i], groups))))
        }
    }
    
    # Try dot separation heuristic
    line_sep <- strsplit(line, "\\.")[[1]]
    line_sep <- line_sep[line_sep != ""]
    
    num_solutions <- sapply(0:length(groups), function(i) {
        if(i == 0) {
            return(solve(line_sep[1], NULL))
        } else {
            return(solve(line_sep[1], groups[1:i]))
        }
    })
    
    w_sol <- which(num_solutions > 0)
    
    if(length(w_sol) == 0) {
        return(0)
    }
    
    return(sum(sapply(w_sol, function(i) {
        if(i > length(groups)) {
            new_groups <- NULL
        } else {
            new_groups <- groups[i:length(groups)]
        }
        
        new_line <- paste(line_sep[2:length(line_sep)], collapse = ".")
        num_solutions[i] * solve(new_line, new_groups)
        
    })))
}

solve <- memoise(solve)
# 
# solve("?????#?", c(2, 2))
# solve2("?????#?", c(2, 2))
# 
# line <- lines[2]
# groups <- contig[[2]]






# N <- 5
# line <- paste(rep("?????#???#??#????", N), collapse = "?")
# groups <- rep(c(3, 8), N)
# solve(line, groups)



solve("?", 1)  # 1
solve("?", 0)  # 1
solve("#", 1)  # 1
solve("#", 0)  # 0
solve(".", 1)  # 0
solve(".", 0)  # 1
solve('??', 1)  # 2
solve('#?', 1)  # 1
solve('.?', 1)  # 1
solve('???', 1)  # 3
solve('#??', 1)  # 1
solve('.??', 1)  # 2
solve("?##", 3)  # 1
solve('????', 1)  # 4
solve('?????', 1)  # 5
solve('???', c(1, 1))  # 1
solve('????', c(1, 1))  # 3
solve("???.###", c(1, 1, 3))  # 1
solve(".??..??...?##.", c(1, 1, 3))  # 4
solve("?#?#?#?#?#?#?#?", c(1, 3, 1, 6))  # 1
solve("????.#...#... 4,1,1", c(4, 1, 1))  # 1
solve("????.######..#####.", c(1, 6, 5))  # 4
solve("?###????????", c(3, 2, 1))  # 10
solve('###???', 2)  # 0
solve('#?#???', c(2, 1))  # 0
solve('???#?#', c(1, 2))  # 0

solve(paste(rep("???.###", 5), collapse = "?"), rep(c(1, 1, 3), 5))  # 1
solve(paste(rep(".??..??...?##.", 5), collapse = "?"), rep(c(1, 1, 3), 5))  # 16384
solve(paste(rep("?#?#?#?#?#?#?#?", 5), collapse = "?"), rep(c(1, 3, 1, 6), 5))  # 1
solve(paste(rep("????.#...#...", 5), collapse = "?"), rep(c(4, 1, 1), 5))  # 16
solve(paste(rep("????.######..#####.", 5), collapse = "?"), rep(c(1, 6, 5), 5))  # 2500
solve(paste(rep("?###????????", 5), collapse = "?"), rep(c(3, 2, 1), 5))  # 506250


lines2 <- sapply(lines, function(x) paste(rep(x, 5), collapse = "?"))
contig2 <- lapply(contig, function(x) rep(x, 5))

ans <- sum(sapply(1:length(lines), function(i) {
    print(i)
    s1 <- solve(lines2[i], contig2[[i]])
    # s2 <- solve2(lines[i], contig[[i]])
    
    # if(s1 != s2) {
        # stop(sprintf("%s/%s/%s", i, s1, s2))
    # }
    
    return(s1)
}))



