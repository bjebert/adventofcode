library(data.table)


count_contig <- function(chars) {
    groups <- c()
    curr <- "."
    count <- 0
    for(i in 1:length(chars)) {
        if(chars[i] == '#') {
            count <- count + 1
        } else if(curr == '#') {
            groups <- c(groups, count)
            count <- 0
        }
        curr <- chars[i]
    }
    
    if(curr == '#') {
        groups <- c(groups, count)
    }
    
    return(groups)
}


solve_brute <- function(line, groups) {
    if(length(line) == 1) {
        line <- strsplit(line, "")[[1]]
    }
    
    w <- which(line == "?")
    if(length(w) == 0) {
        if(paste(groups, collapse = "|") == paste(count_contig(line), collapse = "|")) {
            return(1)
        } else {
            return(0)
        }
    }
    
    grid <- as.matrix(do.call(expand.grid, replicate(length(w), c("#", "."), simplify = FALSE)))
    
    grid_adj <- sapply(1:nrow(grid), function(j) {
        xx <- copy(line)
        xx[w] <- grid[j,]
        return(paste(xx, collapse = ""))
    })
    
    splits <- strsplit(grid_adj, "\\.")
    
    splitn <- lapply(splits, nchar)    
    splitn <- unname(lapply(splitn, function(x) x[x != 0]))
    
    grp <- sapply(splitn, function(x) paste(x, collapse = "|"))
    return(sum(grp == paste(groups, collapse = "|")))
}


solve_r <- function(line, groups) {
    if(length(line) == 1) {
        line <- strsplit(line, "")[[1]]
    }
    
    # Remove leading and trailing dots
    line <- strsplit(gsub("^\\.*|\\.*$", "", paste(line, collapse = "")), "")[[1]]
    
    w <- which(line == "?")
    if(length(w) == 0) {
        if(length(line) == 0 && length(groups) == 0) {
            return(1)
        } else if(length(line) > 0 && length(groups) == 0) {
            return(0)
        } else if(length(line) == 0 && length(groups) > 0) {
            return(0)
        } else if(paste(groups, collapse = "|") == paste(count_contig(line), collapse = "|")) {
            return(1)
        } else {
            return(0)
        }
    }
    
    if(length(groups) == 0) {
        sum_hash <- sum(line == '#')
        if(sum_hash > 0) {
            return(0)
        } else {
            return(1)
        }
    }
    
    # Apply dot separation strategy, find spots that must be dots
    w_hash <- which(line == '#')
    w_not <- which(line != '#')
    
    if(sum(line == '#') > sum(groups)) {
        return(0)
    }
    
    if(length(w_hash) > 0) {
        starts <- w_hash[(w_hash-1) %in% c(0, w_not)]
        ends <- w_hash[(w_hash+1) %in% c(w_not, length(line)+1)]
        
        # If first or last character of line is '#', then we can fill the first/last group in, and remove from line
        if(line[1] == '#') {
            if(length(starts[1]:ends[1]) == groups[1]) {
                return(solve_r(line[-c(1:(groups[1]+1))], groups[-1]))
            }
        }
        
        if(tail(line, 1) == '#') {
            if(length(tail(starts, 1):tail(ends, 1)) == tail(groups, 1)) {
                return(solve_r(line[-((length(line) - tail(groups, 1)):length(line))], groups[-length(groups)]))
            }
        }
        
        curr_contig <- sapply(1:length(starts), function(i) length(starts[i]:ends[i]))
        w_ismax <- curr_contig == max(groups)
        
        w_must_dot <- sort(c(starts[w_ismax] - 1, ends[w_ismax] + 1))
        w_must_dot <- w_must_dot[w_must_dot >= 1 & w_must_dot <= length(line)]
        
        line[w_must_dot] <- "."
    }
    
    if(length(w) <= 6) {
        return(solve_brute(line, groups))
    }
    
    line_sep <- strsplit(paste(line, collapse = ""), "\\.")[[1]]
    line_sep <- line_sep[line_sep != ""]
    
    if(length(line_sep) == 1) {
        # Need to apply heuristics
        missing_hash <- sum(groups) - sum(line == '#')
        missing_dots <- sum(line == "?") - missing_hash
        
        # if(missing_dots == 0) {
        #     return(solve_r(gsub("\\?", "#", line), groups))
        # }
        
        # Find all combinations of first possible group.  Do we know that w_hash[1] is part of it?  No.
        # print(sum(line == "?"))
        # w_hash
        # groups[1]
        # 
        # line_heu <- copy(line)
        # line_heu[w[abs(w - length(line) / 2) == min(abs(w - length(line) / 2))][1]] <- "."
        # 
        browser()
        # amt <- solve_r(line_heu, groups)
        stop()
    } 
    
    head_n <- sapply(0:length(groups), function(i) solve_r(line_sep[1], groups[0:i]))
    tail_n <- sapply(1:(length(groups) + 1), function(i) {
        if(i == length(groups) + 1) {
            return(solve_r(tail(line_sep, 1), NULL))
        } else {
            return(solve_r(tail(line_sep, 1), groups[i:length(groups)]))
        }
    })
    
    head_i <- which(head_n > 0) - 1
    tail_i <- which(tail_n > 0)
    
    if(length(head_i) == 0 || length(tail_i) == 0) {
        return(0)
    }
    
    if(length(line_sep) == 2) {
        return(sum(head_n * tail_n))
    }
    
    return(sum(sapply(1:length(head_i), function(i) {
        sum(sapply(1:length(tail_i), function(j) {
            if(tail_i[j] - head_i[i] == 1) {
                
                sol <- solve_r(paste(line_sep[2:(length(line_sep)-1)], collapse = "."), NULL)
                return(head_n[i] * tail_n[j] * sol)
                
            } else if(head_i[i] < tail_i[j]) {
                grp_new <- groups[(head_i[i]+1):(tail_i[j]-1)]
                
                sol <- solve_r(paste(line_sep[2:(length(line_sep)-1)], collapse = "."), grp_new)
                return(head_n[head_i[i]+1] * tail_n[tail_i[j]] * sol)
                
            } else {
                return(0)
            }
        }))
    })))
}

line <- lines2[3]
groups <- contig2[[3]]
solve_r(line, groups)

line <- "??#?#?#?#?#?#?#???#?#?#?#?#?#?#?"
groups <- c(6, 1, 3, 1, 6, 1, 3, 1, 6)
solve_r(line, groups)
solve_brute(line, groups)


line <- paste(rep("?#?#?#?#?#?#?#?", 5), collapse = "?")
groups <- rep(c(1,3,1,6), 5)
solve_r(line, groups)


inp <- readLines("2023/12.txt")

inp <- strsplit("???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1", "\n")[[1]]

lines <- sapply(strsplit(inp, " "), function(x) x[1])
contig <- lapply(strsplit(sapply(strsplit(inp, " "), function(x) x[2]), ","), as.numeric)

lines2 <- unname(sapply(lines, function(x) paste(rep(x, 5), collapse = "?")))
contig2 <- lapply(contig, function(x) rep(x, 5))



for(i in 1:length(inp)) {
    print(solve_r(lines2[i], contig2[[i]]))
}



# ----

line <- strsplit(".??..??...?##.", "")[[1]]
groups <- c(1, 1, 3)

solve(line, groups)
solve_r(line, groups)

# ----

line <- strsplit("???#?????.???#?.??.??.???", "")[[1]] 
groups <- c(2, 1, 1, 1, 1)

solve_r(line, groups)

# Only non-zero combinations are the following (soln is sum of all):
solve(line_sep[1], groups[1:2]) * solve(line_sep[2], groups[3:4]) * solve(line_sep[3], groups[5])
solve(line_sep[1], groups[1:3]) * solve(line_sep[2], groups[4:4]) * solve(line_sep[3], groups[5])
solve(line_sep[1], groups[1:3]) * solve(line_sep[2], groups[4:5]) * solve(line_sep[3], NULL)
solve(line_sep[1], groups[1:4]) * solve(line_sep[2], groups[5]) * solve(line_sep[3], NULL)


# ----

line <- strsplit("???#?????.???#?", "")[[1]] 
groups <- c(2, 1, 1, 1, 1)

solve(line, groups)  # 19
solve_r(line, groups)  # 19

# Only non-zero combinations are the following (soln is sum of all):
solve(line_sep[1], groups[1:3]) * solve(line_sep[2], groups[4:5]) 
solve(line_sep[1], groups[1:4]) * solve(line_sep[2], groups[5]) 


# ----
