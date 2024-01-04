rm(list=ls())
source("utilities.R")
inp <- get_input("2023/12", parse = F, deesblake = F, cache = T)

inp <- strsplit("???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1", "\n")[[1]]

lines <- sapply(strsplit(inp, " "), function(x) x[1])
contig <- lapply(strsplit(sapply(strsplit(inp, " "), function(x) x[2]), ","), as.numeric)
csum <- sapply(contig, sum)


# Part 1 ------------------------------------------------------------------


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


solve2 <- function(line, groups) {
    if(length(line) == 1) {
        x <- strsplit(line, "")[[1]]
    } else {
        x <- line
    }
    
    w <- which(x == "?")
    if(length(w) == 0) {
        if(paste(groups, collapse = "|") == paste(count_contig(x), collapse = "|")) {
            return(1)
        } else {
            return(0)
        }
    }
    
    grid <- as.matrix(do.call(expand.grid, replicate(length(w), c("#", "."), simplify=FALSE)))
    
    grid_adj <- sapply(1:nrow(grid), function(j) {
        xx <- copy(x)
        xx[w] <- grid[j,]
        return(paste(xx, collapse = ""))
    })
    
    splits <- strsplit(grid_adj, "\\.")
    
    splitn <- lapply(splits, nchar)    
    splitn <- unname(lapply(splitn, function(x) x[x != 0]))
    
    grp <- sapply(splitn, function(x) paste(x, collapse = "|"))
    return(sum(grp == paste(groups, collapse = "|")))
}


sum(sapply(1:length(lines), function(i) solve2(lines[i], contig[[i]])))


# Part 2 ------------------------------------------------------------------

# Depending on number of groups, answer seems to resemble triangular numbers.  

sapply(1:10, function(x) solve(paste(rep("?", x), collapse = ""), 1))
sapply(1:10, function(x) solve(paste(rep("?", x), collapse = ""), 2))
sapply(1:10, function(x) solve(paste(rep("?", x), collapse = ""), 3))

# 1, 2, 3, 4, 5, 6, 7, ...
sapply(1:8, function(n) n/factorial(1))

sapply(1:10, function(x) solve(paste(rep("?", x), collapse = ""), c(1, 1)))
sapply(1:10, function(x) solve(paste(rep("?", x), collapse = ""), c(1, 2)))
sapply(1:10, function(x) solve(paste(rep("?", x), collapse = ""), c(1, 3)))
sapply(1:10, function(x) solve(paste(rep("?", x), collapse = ""), c(1, 4)))
sapply(1:10, function(x) solve(paste(rep("?", x), collapse = ""), c(1, 5)))

# 1, 3, 6, 10, 15, 21, 28, ... (triangular)
sapply(1:8, function(n) n*(n+1)/factorial(2))

sapply(1:14, function(x) solve(paste(rep("?", x), collapse = ""), c(1, 1, 1)))
sapply(1:14, function(x) solve(paste(rep("?", x), collapse = ""), c(1, 1, 2)))
sapply(1:14, function(x) solve(paste(rep("?", x), collapse = ""), c(2, 1, 1)))

# 1, 4, 10, 20, 35, 56, 84, 120, 165, ... (tetrahedral)
sapply(1:8, function(n) n*(n+1)*(n+2)/factorial(3))

sapply(1:14, function(x) solve(paste(rep("?", x), collapse = ""), c(1, 1, 1, 1)))
sapply(1:14, function(x) solve(paste(rep("?", x), collapse = ""), c(1, 1, 1, 2)))
sapply(1:14, function(x) solve(paste(rep("?", x), collapse = ""), c(1, 1, 1, 3)))

# 1, 5, 15, 35, 70, 126, 210, 330, ...
sapply(1:8, function(n) n*(n+1)*(n+2)*(n+3)/factorial(4))

sapply(1:14, function(x) solve(paste(rep("?", x), collapse = ""), c(1, 1, 1, 1, 1)))
sapply(1:14, function(x) solve(paste(rep("?", x), collapse = ""), c(1, 1, 1, 1, 2)))
sapply(1:14, function(x) solve(paste(rep("?", x), collapse = ""), c(1, 1, 1, 1, 3)))

sapply(1:8, function(n) n*(n+1)*(n+2)*(n+3)*(n+4)/factorial(5))

f <- function(n, x) {  # x = number of unique groups?
    prod(n+(0:(x-1))) / factorial(x)
}

# This sequence only works for the all "?s" problem.  If we add a hash, how do things change?  Let's look at last example.


solve("?#?.???", c(2, 1, 1))
solve("?#?.????", c(2, 1, 1))
solve("?#?.?????", c(2, 1, 1))
solve("?#?.??????", c(2, 1, 1))
solve("?#?.???????", c(2, 1, 1))
# 2, 6, 12, 20, 30, ...

# = 2 * (1, 3, 6, 10)

solve("?#?.", 2)  # 2

solve("???", c(1, 1))
solve("????", c(1, 1))
solve("?????", c(1, 1))
solve("??????", c(1, 1))
solve("???????", c(1, 1))
# 1, 3, 6, 10, ...

# This problem is pretty straightforward.

solve("?#????", c(2, 1, 1))
solve("?#?????", c(2, 1, 1))
solve("?#??????", c(2, 1, 1))
solve("?#???????", c(2, 1, 1))
solve("?#????????", c(2, 1, 1))
solve("?#?????????", c(2, 1, 1))
solve("?#??????????", c(2, 1, 1))
# 1, 4, 9, 16, 25, 36, 49, ...

c(1, 3, 6, 10, 15, 21, 28) + c(0, 1, 3, 6, 10, 15, 21)


solve("???#???.?", c(2, 1, 2))
solve("???#???.??", c(2, 1, 2))
solve("???#???.???", c(2, 1, 2))
solve("???#???.????", c(2, 1, 2))
solve("???#???.?????", c(2, 1, 2))



solve("#??#???.???", c(2, 1, 2))
solve("?#?#???.???", c(2, 1, 2))
solve("??##???.???", c(2, 1, 2))
solve("???##??.???", c(2, 1, 2))

# Just don't see how the hell we can generalise the groups problem away, the order and amt is so damn important.

solve("?##??????", c(3, 1, 1))
solve("??????##?", c(1, 1, 3))

solve("????#????.???#?", c(2, 1, 2))
solve("?#???.????#????", c(2, 1, 2))


# We know exactly how many of the ?s need to be '.'s or '#'s, right?.  #s must sum up to contig

# 1-a
# ???.### 1,1,3

ss <- strsplit(lines[1], "")[[1]]
hashes <- csum[1] - sum(ss == "#")
Qs <- sum(ss == "?")

choose(2, hashes)  # First number must be <= Qs

# 1-b
# .??..??...?##. 1,1,3

ss <- strsplit(lines[2], "")[[1]]
hashes <- csum[2] - sum(ss == "#")
Qs <- sum(ss == "?")

choose(4, hashes)  # First number must be <= Qs, and represents number of possible spots hashes can be

# 1-c
# ?#?#?#?#?#?#?#? 1,3,1,6

ss <- strsplit(lines[3], "")[[1]]
hashes <- csum[3] - sum(ss == "#")
Qs <- sum(ss == "?")

choose(4, hashes)  # First number must be <= Qs, and represents number of possible spots hashes can be

# 1-d
# ????.#...#... 4,1,1

ss <- strsplit(lines[4], "")[[1]]
hashes <- csum[4] - sum(ss == "#")
Qs <- sum(ss == "?")

choose(4, hashes)  # First number must be <= Qs, and represents number of possible spots hashes can be

# 1-e
# ????.######..#####. 1,6,5

ss <- strsplit(lines[5], "")[[1]]
hashes <- csum[5] - sum(ss == "#")
Qs <- sum(ss == "?")

choose(4, hashes)  # First number must be <= Qs, and represents number of possible spots hashes can be

# 1-f
# ?###???????? 3,2,1

ss <- strsplit(lines[6], "")[[1]]
hashes <- csum[6] - sum(ss == "#")
Qs <- sum(ss == "?")

choose(5, hashes)  # First number must be <= Qs, and represents number of possible spots hashes can be


# DOT separation method ---------------------------------------------------

solve("?????#??.????", c(2, 2))

solve("?????#??.", c(2))
solve("?????#??.", c(2, 2))
solve(".????", c(2))
solve(".????", c(2, 2))

solve("?????#??.??", c(2, 2))
solve("?????#??.????", c(2, 2))

# ---

solve("????.######..#####.", c(1, 6, 5))

solve("????.", c(1, 6, 5))
solve("????.", c(1, 6))
solve("????.", c(1))

solve(".######.", c(1, 6, 5))
solve(".######.", c(6, 5))
solve(".######.", c(5))

solve(".#####.", c(1, 6, 5))
solve(".#####.", c(6, 5))
solve(".#####.", c(5, 6))
solve(".#####.", c(5))

# ---

solve("???#?????.???#?", c(2, 1, 1, 1, 1))

solve("???#?????.", c(2, 1, 1, 1, 1)) * solve("???#?", c(0))
solve("???#?????.", c(2, 1, 1, 1)) * solve("???#?", c(1))  
solve("???#?????.", c(2, 1, 1)) * solve("???#?", c(1, 1))  
solve("???#?????.", c(2, 1)) * solve("???#?", c(1, 1, 1))  
solve("???#?????.", c(2)) * solve("???#?", c(1, 1, 1, 1))  
solve("???#?????.", c(0)) * solve("???#?", c(2, 1, 1, 1, 1))  


solve("???#?????.???#?.??", c(2, 1, 1, 1, 1))

solve("???#?????", c())
solve("???#?????", c(2))
solve("??#?", c())



# Recursive "chunk" method ------------------------------------------------


line <- strsplit("?###????????", "")[[1]]
groups <- c(3,2,1)

# Analyse first chunk (up to end of first # sequence)

chunk <- line[1:(which(line != '#')[which(line != '#') > which(line == '#')[1]][1] - 1)]
solve(chunk, groups[1])

solve("?###????????", c(3,2,1))
solve(".###????????", c(3,2,1))
solve(".###.???????", c(3,2,1))
solve(".###.???????", c(3,2,1))


# Analyse bottom row (A=506250) -------------------------------------------

line <- strsplit(paste(rep("??.?#??#?#??##????", 5), collapse = "?"), "")[[1]]
groups <- rep(c(2,4,6,1), 5)

line <- strsplit("???#?????.???#?.??", "")[[1]] 
groups <- c(2, 1, 1, 1, 1)

solve(line, groups)

# Dot separation strategy ----------------------------------------

w_hash <- which(line == '#')
w_not <- which(line != '#')

starts <- w_hash[(w_hash-1) %in% c(0, w_not)]
ends <- w_hash[(w_hash+1) %in% c(w_not, length(line)+1)]

w_ismax <- sapply(1:length(starts), function(i) length(starts[i]:ends[i])) == max(groups)

w_must_dot <- sort(c(starts[w_ismax] - 1, ends[w_ismax] + 1))
w_must_dot <- w_must_dot[w_must_dot >= 1 & w_must_dot <= length(line)]

line_mod <- copy(line)
line_mod[w_must_dot] <- "."

line_sep <- strsplit(paste(line_mod, collapse = ""), "\\.")[[1]]
line_sep <- line_sep[line_sep != ""]

solve_r <- function(line, groups) {
    if(length(line) == 1) {
        line <- strsplit(line, "")[[1]]
    }
    
    if(sum(line == ".") == 0) {
        return(solve(line, groups))
    } else {
        
        
        
    }
    
    
}

line_sep
j <- 0; k <- 0; l <- 0
for(j in 0:length(groups)) {
    sol1 <- solve(line_sep[1], groups[0:j])
    
    if(sol1 > 0 && j < length(groups)) {
        for(k in j:length(groups)) {
            if(k == j) {
                sol2 <- solve(line_sep[2], c())  # null case
            } else {
                sol2 <- solve(line_sep[2], groups[(j+1):k])
            }
            
            if(sol2 > 0 && k < length(groups)) {
                for(l in k:length(groups)) {
                    if(l == k) {
                        sol3 <- solve(line_sep[3], c())  # null case
                    } else {
                        sol3 <- solve(line_sep[3], groups[(k+1):length(groups)])
                    }
                    
                    if(sol3 > 0) {
                        # print(sprintf("%s,%s,%s", j, k, l))
                        print(prod(c(sol1, sol2, sol3)))
                    }
                }
            }
        }
    }
}

solve(line_sep[1], groups[1:2]) * solve(line_sep[2], groups[3:4]) * solve(line_sep[3], groups[5])
solve(line_sep[1], groups[1:3]) * solve(line_sep[2], groups[4]) * solve(line_sep[3], groups[5])
solve(line_sep[3], c())

solve(line_sep[1], groups[1]) * solve(line_sep[2], groups[2:5])

